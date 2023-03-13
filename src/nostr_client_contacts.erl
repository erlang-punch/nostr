%%%===================================================================
%%% @doc `nostr_client_contacts' stores contact list from the server,
%%% it's an implementation of NIP/02.
%%%
%%% @end
%%%===================================================================
-module(nostr_client_contacts).
-behavior(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([add/2, add/3]).
-export([delete/2]).
-export([publish/1]).
-export([export/1, import/2]).
-export([list/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, { store = undefined }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start(Args) ->
    logger:set_module_level(?MODULE, debug),
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, to_be_defined()} | {stop, to_be_defined()}.
init(Args) ->
    logger:set_module_level(?MODULE, debug),
    Host = proplists:get_value(host, Args, undefined),
    pg:join(client, {Host, contacts}, self()),
    Store = ets:new(?MODULE, []),
    {ok, #state{ store = Store}}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_cast(publish, #state{ store = Store } = State) ->
    Event = to_event(Store),
    % need to find a way to set the private key:
    {ok, Encoded} = nostrlib:encode(Event, [{private_key, <<1:256>>}]),
    ?LOG_DEBUG("~p", [Encoded]),
    {noreply, State};

% delete a contact present in the database
handle_cast({delete, {contact, PublicKey, _, _}}, #state{ store= Store} = State) ->
    ets:delete(Store, PublicKey),
    {noreply, State};

% add a new contact or update an existing one
handle_cast({add, {contact, <<PublicKey:256/bitstring>>, ContactName, MainRelay}}
           , #state{ store = Store } = State) 
  when is_binary(ContactName) andalso is_binary(MainRelay) ->
    ets:insert(Store, {PublicKey, ContactName, MainRelay}),
    {noreply, State};

% default action
handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: to_be_defined(),
      From :: gen_server:from(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_call(export, _, #state{ store = Store } = State) ->
    Event = to_event(Store),
    {reply, Event, State};
handle_call(list, _, #state{ store = Store } = State) ->
    List = ets:tab2list(Store),
    {reply, List, State};
handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), call, Message, From, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
convert({PublicKey, ContactName, MainRelay}) ->
    #tag{ name = public_key
        , value = PublicKey
        , params = [MainRelay, ContactName]
        }.

to_event(Ets) ->
    List = ets:tab2list(Ets),
    Tags = lists:map(fun convert/1, List),
    #event{ content = <<>>
          , kind = contact_list
          , tags = Tags
          }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add(any(), any()) -> ok.
add(Pid, PublicKey) ->
    add(Pid, PublicKey, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec add(any(), any(), any()) -> ok.
add(Pid, <<PublicKey:256/bitstring>>, Opts) ->
    ContactName = proplists:get_value(name, Opts, <<>>),
    MainRelay = proplists:get_value(relay, Opts, <<>>),
    gen_server:cast(Pid, {add, {contact, PublicKey, ContactName, MainRelay}}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(any(), any()) -> ok.
delete(Pid, <<PublicKey:256/bitstring>>) ->
    gen_server:cast(Pid, {delete, {contact, PublicKey, undefined, undefined}}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish(any()) -> ok.
publish(Pid) ->
    gen_server:cast(Pid, publish).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec import(any(), any()) -> ok.
import(Pid, PublicKey) ->
    gen_server:cast(Pid, {import, PublicKey}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec export(any()) -> ok.
export(Pid) ->
    gen_server:call(Pid, export).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list(any()) -> ok.
list(Pid) ->
    gen_server:call(Pid, list).

