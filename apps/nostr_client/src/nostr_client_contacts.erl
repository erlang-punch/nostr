%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc DRAFT: `nostr_client_contacts' stores contact list from the
%%% server, it's an implementation of <a
%%% href="https://github.com/nostr-protocol/nips/blob/master/02.md">NIP/02</a>.
%%%
%%% A contact list should be dedicated to one user/profile and not per
%%% host but, it should also be possible to have one user having
%%% different contact list on different server. A protected/private
%%% relay could probably have more contact than a "public" one.
%%%
%%% == Examples ==
%%%
%%% ```
%%% % start a new contact list
%%% {ok, C} = nostr_client_contacts:start([{host, <<"localhost">>}]).
%%%
%%% % add a new contact
%%% nostr_client_contacts:add(C, <<1:256>>).
%%%
%%% % add another contact with a name
%%% nostr_client_contacts:add(C, <<2:256>>, [{name, <<"Mr. Two">>}]).
%%%
%%% % add a third contact
%%% nostr_client_contacts:add(C, <<2:256>>, [
%%%   {name, <<"Mr. Three">>},
%%%   {relay, <<"wss://three.relay">>}
%%%   ]).
%%%
%%% % export the contacts as list
%%% nostr_client_contacts:list(C).
%%%
%%% % export the contacts list as an event
%%% {ok, DecodedEvent} = nostr_client_contacts:export(C).
%%% {ok, EncodedEvent} = nostrlib:encode(Event, [{private_key, <<1:256>>}]).
%%% '''
%%%
%%% == Questions ==
%%%
%%% <ul>
%%% <li>Should we export the contact list locally, with the private key?</li>
%%% <li>if stored with the private key, should we encrypt its content?</li>
%%% <li>Should we export the contact list as an event? or a list of tag?</li>
%%% <li>When started and connected to a relay, should we fetch automatically
%%%     the list provided/stored there?</li>
%%% <li>Should we create a kind of versionning for this list?</li>
%%% </ul>
%%%
%%% @see nostr_client_key
%%% @end
%%%===================================================================
-module(nostr_client_contacts).
-behavior(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([add/2, add/3]).
-export([delete/2]).
-export([publish/2]).
-export([export/1, export/2]).
% -export([import/2]).
-export([list/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").
-record(state, { identity = undefined
               , host = undefined
               , store = undefined
               }).

%%--------------------------------------------------------------------
%% @doc start a new process.
%%
%% @see gen_server:start/3
%% @end
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start(Args) ->
    logger:set_module_level(?MODULE, debug),
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc start a new linked process.
%%
%% @see gen_server:start_link/3
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @see gen_server:init/1
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, to_be_defined()} | {stop, to_be_defined()}.
init(Args) ->
    logger:set_module_level(?MODULE, debug),
    Host = proplists:get_value(host, Args, undefined),
    pg:join(client, {Host, contacts}, self()),
    Store = ets:new(?MODULE, []),
    State = #state{ store = Store
                  , host = Host
                  },
    {ok, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @see gen_server:terminate/2
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @see gen_server:handle_cast/2
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

%% @TODO: import a list from a private key
%% handle_cast({import, PrivateKey}, #state{ host = Host, store = Store } = State) ->
%%     Event = ets_to_event(Store),
%%     {ok, Encoded} = nostrlib:encode(Event, [{private_key, PrivateKey}]),
%%     {ok, Connection} = nostr_client:get_process(Host, connection),
%%     nostr_client_connection:send_raw(Connection, Encoded),
%%     {noreply, State};

% publish the contact list to a relay
handle_cast({publish, PrivateKey}, #state{ host = Host, store = Store } = State) ->
    Event = ets_to_event(Store),
    % @todo need check here
    {ok, Encoded} = nostrlib:encode(Event, [{private_key, PrivateKey}]),
    {ok, Connection} = nostr_client:get_process(Host, connection),
    nostr_client_connection:send_raw(Connection, Encoded),
    {noreply, State};

% delete a contact present in the database
handle_cast({delete, {contact, PublicKey, _, _}} = M, #state{ store= Store} = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_cast, delete, M}]),
    ets:delete(Store, PublicKey),
    {noreply, State};

% add a new contact or update an existing one
handle_cast({add, {contact, <<PublicKey:256/bitstring>>, ContactName, MainRelay}} = M
           , #state{ store = Store } = State)
  when is_binary(ContactName) andalso is_binary(MainRelay) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_cast, add, M}]),
    ets:insert(Store, {PublicKey, ContactName, MainRelay}),
    {noreply, State};

% default action
handle_cast(Message, State) ->
    ?LOG_WARNING("~p", [{?MODULE, self(), cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @see gen_server:handle_call/3
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: to_be_defined(),
      From :: gen_server:from(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_call({export, _} = M, _, #state{ store = Store } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_call, M}]),
    Event = ets_to_event(Store),
    {reply, Event, State};
handle_call(list, _, #state{ store = Store } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_call, list}]),
    List = ets:tab2list(Store),
    {reply, List, State};
handle_call(Message, From, State) ->
    ?LOG_WARNING("~p", [{?MODULE, self(), call, Message, From, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @see gen_server:handle_info/2
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_info(Message, State) ->
    ?LOG_WARNING("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. `convert/1' converts a contact entry to a `#tag{}'
%% record.
%% @end
%%--------------------------------------------------------------------
convert({PublicKey, ContactName, MainRelay}) ->
    #tag{ name = public_key
        , value = PublicKey
        , params = [MainRelay, ContactName]
        }.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. `ets_to_event/1' converts the contact list present
%% in the ETS to an `#event{}' record.
%% @end
%%--------------------------------------------------------------------
ets_to_event(Ets) ->
    List = ets:tab2list(Ets),
    Tags = lists:map(fun convert/1, List),
    #event{ content = <<>>
          , kind = contact_list
          , tags = Tags
          }.

%%--------------------------------------------------------------------
%% @doc (API) `add/2'.
%% @see add/3
%% @end
%%--------------------------------------------------------------------
-spec add(any(), any()) -> ok.
add(Pid, PublicKey) ->
    add(Pid, PublicKey, []).

%%--------------------------------------------------------------------
%% @doc (API) `add/3' function adds a new contact or update an
%% existing one in the ETS table.
%%
%% == Examples ==
%%
%% ```
%% ok = nostr_client_contacts:add(Pid, <<1:256>>).
%% ok = nostr_client_contacts:add(Pid, <<1:256>>, [{name, Name}]).
%% ok = nostr_client_contacts:add(Pid, <<1:256>>, [{name, Name}
%%                                                ,{relay, Relay}]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec add(any(), any(), any()) -> ok.
add(Pid, <<PublicKey:256/bitstring>>, Opts) ->
    ContactName = proplists:get_value(name, Opts, <<>>),
    MainRelay = proplists:get_value(relay, Opts, <<>>),
    gen_server:cast(Pid, {add, {contact, PublicKey, ContactName, MainRelay}}).

%%--------------------------------------------------------------------
%% @doc (API) `delete/2' deletes a contact in the database.
%%
%% == Examples ==
%%
%% ```
%% ok = nostr_client_contacts:delete(Pid, <<1:256>>).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(any(), any()) -> ok.
delete(Pid, <<PublicKey:256/bitstring>>) ->
    gen_server:cast(Pid, {delete, {contact, PublicKey, undefined, undefined}}).

%%--------------------------------------------------------------------
%% @doc (API) `publish/2' function publishes to the current server.
%%
%% == Examples ==
%%
%% ```
%% ok = nostr_client_contacts:publish(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec publish(any(), any()) -> ok.
publish(Pid, PrivateKey) ->
    gen_server:cast(Pid, {publish, PrivateKey}).

%%--------------------------------------------------------------------
%% (API) `import/2' function import a contact list from a public key.
%%
%% == Examples ==
%%
%% ```
%% ok = nostr_client_contacts:import(Pid, <<1:256>>).
%% '''
%%--------------------------------------------------------------------
% -spec import(any(), any()) -> ok.
% import(Pid, PublicKey) ->
%     gen_server:cast(Pid, {import, PublicKey}).

%%--------------------------------------------------------------------
%% @doc (API) `export/1' function exports the contact as `#event{}'.
%% @see export/2
%% @end
%%--------------------------------------------------------------------
-spec export(any()) -> ok.
export(Pid) ->
    export(Pid, []).

%%--------------------------------------------------------------------
%% @doc (API) `export/1' function exports the contact as `#event{}'.
%%
%% == Examples ==
%%
%% ```
%% Event = nostr_client_contacts:export(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec export(any(), proplists:proplists()) -> ok.
export(Pid, Opts) ->
    gen_server:call(Pid, {export, Opts}).

%%--------------------------------------------------------------------
%% @doc (API) `list/1' function list return the contacts as list.
%%
%% == Examples ==
%%
%% ```
%% ContactList = nostr_client_contacts:list(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec list(any()) -> ok.
list(Pid) ->
    gen_server:call(Pid, list).
