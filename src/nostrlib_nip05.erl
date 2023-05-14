%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc `nostrlib_nip05' implements NIP/05 specification for the
%%% client client. When a client receives a kind 0 event containing a
%%% `pubkey' and a `content' attribute with `nip05' fields in it, it
%%% should check if the user is correct.
%%%
%%% ```
%%% % to test: https://nostrprotocol.net/.well-known/nostr.json?name=bob
%%% '''
%%%
%%% see: https://github.com/nostr-protocol/nips/blob/master/05.md
%%%
%%% @todo improve specifications
%%% @todo rename this module: nostr_client_nip05.
%%% @todo the state present in this process should be stored somewhere
%%%       else (database).
%%% @end
%%%===================================================================
-module(nostrlib_nip05).
-behavior(gen_statem).
-export([start/1]).
-export([init/1]). %, terminate/3]).
-export([callback_mode/0]).
-export([wait/3, response/3, done/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-record(state, { target = undefined
               , public_key = undefined :: undefined | binary() | bitstring()
               , connection = undefined
               , connection_reference = undefined
               , valid = false :: boolean()
               }).

%%--------------------------------------------------------------------
%% required for eunit.
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(Opts) -> Return when
      Opts :: proplists:proplists(),
      Return :: {ok, pid()}.

start(Opts) ->
    gen_statem:start(?MODULE, Opts, []).

-spec callback_mode() -> [atom()].
callback_mode() -> [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% Create a new connection based on the identifier passed as first
%% argument. The identifier use the format "localpart@domain", like
%% defined in nip/05 specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Opts) -> Return when
      Opts :: proplists:proplists(),
      Return :: {ok, wait, #state{}}.

init(Opts) ->
    logger:set_module_level(?MODULE, debug),
    State = #state{},
    init_identifier(Opts, State).

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% check if the identifier is present. If it's the case, convert it as
%% target.
%%
%% @todo manage exception when creating the target.
%% @end
%%--------------------------------------------------------------------
init_identifier(Opts, State) ->
    case proplists:get_value(identifier, Opts, undefined) of
        undefined ->
            ?LOG_ERROR("~p: missing identifier", [{?MODULE, self()}]),
            {stop, [{message, <<"missing identifier">>}]};
        Identifier ->
            {ok, Target} = new(Identifier),
            NewState = State#state{ target = Target },
            init_public_key(Opts, NewState)
    end.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% A public key is required there, the content from the server should
%% be compared to it.
%%
%% @todo validated the public key passed in opts.
%% @end
%%--------------------------------------------------------------------
init_public_key(Opts, State) ->
    case proplists:get_value(public_key, Opts, undefined) of
        undefined ->
            ?LOG_ERROR("~p: missing public_key", [{?MODULE, self()}]),
            {stop, [{message, <<"missing public_key">>}]};
        PublicKey when is_binary(PublicKey) ->
            NewState = State#state{ public_key = PublicKey },
            init_connection(NewState);
        _Elsewise ->
            ?LOG_ERROR("~p: invalid public_key", [{?MODULE, self()}]),
            {stop, [{message, <<"invalid public_key">>}]}
    end.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% initialize the connection with gun:open wrapper (open/1) function
%% and start the main process loop.
%%
%% @end
%%--------------------------------------------------------------------
init_connection(#state{ target = Target } = State) ->
    case open(Target) of
        {ok, Connection} ->
            ?LOG_DEBUG("~p: successfully connected to ~p", [{?MODULE, self()}, Target]),
            NewState = State#state{ connection = Connection },
            {ok, wait, NewState};
        {error, Reason} ->
            ?LOG_ERROR("~p: can't connect", [{?MODULE, self()}]),
            {stop, [{reason, Reason}]}
    end.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% This function get the path on the remote server and wait for an
%% answer, if successful, the data are extracted in another state.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait(Type, Message, Data) -> Return when
      Type :: gen_statem:event_type(),
      Message :: tuple(),
      Data :: #state{},
      Return :: tuple().

wait(enter, _, #state{ target = #{ path := Path }
                     , connection = Connection } = State) ->
    ConnectionReference = gun:get(Connection, Path),
    ?LOG_DEBUG("~p: get path ~p", [{?MODULE, self(), State}, Path]),
    NewState = State#state{ connection_reference = ConnectionReference },
    {next_state, wait, NewState};
wait(info, {gun_response, _Conn, _Ref, fin, _Status, _Headers}, State) ->
    ?LOG_DEBUG("~p: received ~p", [{?MODULE, self()}, fin]),
    {next_state, done, State};
wait(info, {gun_response, _Conn, _Ref, nofin, _Status, _Headers}, State) ->
    ?LOG_DEBUG("~p: received ~p", [{?MODULE, self()}, nofin]),
    {next_state, response, State};
wait(info, {'DOWN', _MRef, process, ConnPid, Reason}, State) ->
    ?LOG_DEBUG("~p: process ~p down with ~p", [{?MODULE, self()}, ConnPid, Reason]),
    {next_state, done, State};
wait(_, Message, State) ->
    ?LOG_DEBUG("~p, received ~p", [{?MODULE, self()}, Message]),
    {keep_state, State}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% This function extract the response from the remote server.
%%
%% @end
%%--------------------------------------------------------------------
-spec response(Type, Message, Data) -> Return when
      Type :: gen_statem:event_type(),
      Message :: tuple(),
      Data :: #state{},
      Return :: tuple().

response(info, {gun_data, _Conn, _Ref, nofin, Data}, State) ->
    ?LOG_DEBUG("~p: received ~p", [{?MODULE, self()}, {nofin, Data}]),
    {keep_state, State};
response(info, {gun_data, _Conn, _Ref, fin, Data}, State) ->
    ?LOG_DEBUG("~p: received ~p", [{?MODULE, self()}, {fin, Data}]),
    {next_state, done, State};
response(info, {'DOWN', _MRef, process, ConnPid, Reason}, State) ->
    ?LOG_DEBUG("~p: process ~p down with ~p", [{?MODULE, self()}, ConnPid, Reason]),
    {next_state, done, State};
response(_, Message, State) ->
    ?LOG_DEBUG("~p, received ~p", [{?MODULE, self()}, Message]),
    {keep_state, State}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% This is the final state where we know the identifier is valid (or
%% not). If needed, the process can be stopped there.
%%
%% @end
%%--------------------------------------------------------------------
-spec done(Type, Message, Data) -> Return when
      Type :: gen_statem:event_type(),
      Message :: tuple(),
      Data :: #state{},
      Return :: tuple().

done(enter, _, State) ->
    ?LOG_DEBUG("~p: entered done state", [{?MODULE, self()}]),
    {next_state, done, State};
done({call, _From}, get, State) ->
    ?LOG_DEBUG("~p: received get", [{?MODULE, self()}]),
    {keep_state, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(binary()) -> any().
new(Identifier) ->
    Regex = <<"^(?<localpart>[a-z0-_]+)",
              "@",
              "(?<domain>[a-zA-Z]+\.[a-zA-Z]+)$">>,
    RegexOpts = [extended, {capture,all_names,binary}],
    {ok, MP} = re:compile(Regex),
    {namelist, Names} = re:inspect(MP, namelist),
    case re:run(Identifier, Regex, RegexOpts) of
        {match, Match}->
            List = lists:zip(Names, Match),
            target(maps:from_list(List));
        Result ->
            {error, Result}
    end.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% @todo add documentation
%% @end
%%--------------------------------------------------------------------
target(#{ <<"localpart">> := LocalPart
              , <<"domain">> := Domain
              } = Identifier) ->
    {ok, Uri} = uri(LocalPart, Domain),
    {ok, Url} = url(Uri),
    {ok, Path} = path(Uri),
    Return = Identifier#{ url => Url,
                          uri => Uri,
                          path => Path },
    {ok, Return}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% @todo add documentation
%% @end
%%--------------------------------------------------------------------
uri(LocalPart, Domain) ->
    URI = #{ scheme => <<"https">>,
             host => Domain,
             path => <<"/.well-known/nostr.json">>,
             query => <<"name=",LocalPart/bitstring>>
           },
    {ok, URI}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% @todo add documentation
%% @end
%%--------------------------------------------------------------------
url(URI) ->
    {ok, uri_string:recompose(URI)}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% @todo add documentation
%% @end
%%--------------------------------------------------------------------
path(#{ path := Path, query := Query }) ->
    {ok, uri_string:recompose(#{ path => Path, query => Query })}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% Opens a new HTTP connection to a remote server by using only HTTPS
%% with verified certificates.
%%
%% @end
%%--------------------------------------------------------------------
open(#{ <<"domain">> := Domain } = _Target) ->
    ok = public_key:cacerts_load(),
    Certs = public_key:cacerts_get(),
    DomainList = binary_to_list(Domain),
    TlsOpts = [{cacerts, Certs}
              ,{verify, verify_peer}],
    GunOpts = #{ tls_opts => TlsOpts
               % @todo fix this timeout, to something more acceptable!
               % , tls_handshake_timeout => 3
               , transport => tls
               },
    gun:open(DomainList, 443, GunOpts).

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% parse the data received from gun. It's a wrapper around
%% thoas:decode/1 function.
%%
%% @todo improve coverage on this function.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_json(Data) -> Return when
      Data :: bitstring() | binary(),
      Return :: {ok, map()} | {error, Reason},
      Reason :: proplist:proplists().

parse_json(Data) ->
    case thoas:decode(Data) of
        {ok, Result} ->
            check_json_names(Result, #{});
        Elsewise -> Elsewise
    end.

-spec check_json_test() -> any().
check_json_test() ->
    BobKey = <<"b0635d6a9851d3aed0cd6c495b282167acf761729078d975fc341b22650b07b9">>,
    [?assertEqual({error, [{data, #{}}]}, parse_json(<<"{}">>))
    ,?assertEqual({ok, #{ <<"names">> => #{ <<"bob">> => BobKey }
                        , <<"relays">> => #{}
                        }}
                 ,parse_json(<<"{ \"names\": { \"bob\": \"",BobKey/bitstring,"\"} }">>)
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% Check if the json object returned by the server has a mandatory
%% attribute called `names'.
%%
%% @todo check the content of the names, it must be a string
%%       containing an hexadecimal encoded public key.
%%
%% @end
%%--------------------------------------------------------------------
check_json_names(#{ <<"names">> := Names } = Map, Buffer)
  when is_map(Names) ->
    check_json_relays(Map, Buffer#{ <<"names">> => Names });
check_json_names(Map, _Buffer) ->
    {error, [{data, Map}]}.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% Check if the optional attribute `relays' is present in the data
%% returned by the remote server.
%%
%% @todo check the content of each relays, it must be an array
%%       containing valid nostr URL.
%% @end
%%--------------------------------------------------------------------
check_json_relays(#{ <<"relays">> := Relays }, Buffer)
  when is_map(Relays) ->
    Return = Buffer#{ <<"relays">> => Relays },
    {ok, Return};
check_json_relays(_, Buffer) ->
    Return = Buffer#{ <<"relays">> => #{} },
    {ok, Return}.
