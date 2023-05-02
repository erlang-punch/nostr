%%%-------------------------------------------------------------------
%%% @doc DRAFT
%%% @end
%%%-------------------------------------------------------------------
-module(nostrlib_nip05).
-behavior(gen_statem).
-export([start/1]).
-export([init/1]). %, terminate/3]).
-export([callback_mode/0]).
-export([wait/3, response/3, done/3]).
-record(state, { target = undefined
               , connection = undefined
               , connection_reference = undefined
               }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(binary()) -> {ok, pid()}.
start(Identifier) ->
    gen_statem:start(?MODULE, Identifier, []).

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
-spec init(binary()) -> {ok, wait, #state{}}.
init(Identifier) ->
    % 1. load the certificates
    ok = public_key:cacerts_loads(),

    % 2. retrieve the certificates
    % @todo: add certificate support
    _Certs = public_key:cacerts_get(),

    {ok, Target} = new(Identifier),
    {ok, Connection} = open(Target),

    State = #state{ target = Target
                  , connection = Connection
                  },
    {ok, wait, State}.

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
    NewState = State#state{ connection_reference = ConnectionReference },
    {next_state, wait, NewState};
wait(info, {gun_response, _Conn, _Ref, fin, _Status, _Headers}, State) ->
    {next_state, done, State};
wait(info, {gun_response, _Conn, _Ref, nofin, _Status, _Headers}, State) ->
    {next_state, response, State};
wait(info, {'DOWN', _MRef, process, _Conn, _Reason}, State) ->
    {next_state, done, State}.

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

response(info, {gun_data, _Conn, _Ref, nofin, _Data}, State) ->
    {keep_state, State};
response(info, {gun_data, _Conn, _Ref, fin, _Data}, State) ->
    {next_state, done, State};
response(info, {'DOWN', _MRef, process, _ConnPid, _Reason}, State) ->
    {next_state, done, State}.

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
    {next_state, done, State};
done({call, _From}, get, State) ->
    {keep_state, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
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

uri(LocalPart, Domain) ->
    URI = #{ scheme => <<"https">>,
             host => Domain,
             path => <<".well-known/nostr.json">>,
             query => <<"name=",LocalPart/bitstring>>
           },
    {ok, URI}.

url(URI) ->
    {ok, uri_string:recompose(URI)}.

path(#{ path := Path, query := Query }) ->
    {ok, uri_string:recompose(#{ path => Path, query => Query })}.

open(#{ domain := Domain }) ->
    DomainList = binary_to_list(Domain),
    gun:open(DomainList, 443).
    
    
