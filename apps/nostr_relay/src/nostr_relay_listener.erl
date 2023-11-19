%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc `nostr_relay_listener' module is a wrapper around cowboy
%%%      module to start and stop a cowboy listener. Any message
%%%      received on this process will stop the listener.
%%%
%%% @todo cleanup the doc
%%% @end
%%%===================================================================
-module(nostr_relay_listener).
-behavior(gen_server).
-export([start_link/1]).
-export([modules/0, default_state/0, default_pipeline/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-record(?MODULE, { name
                 , transport
                 , protocol
                 , state :: map()
                 , pid   :: pid()
                 }).

%%--------------------------------------------------------------------
%% @doc supported modules.
%% @end
%%--------------------------------------------------------------------
-spec modules() -> [atom()].

modules() ->
    [ nostr_relay_module_init
    , nostr_relay_module_nip01
    , nostr_relay_module_request
    , nostr_relay_module_store
    ].

%%--------------------------------------------------------------------
%% @doc returns the default pipeline used.
%% @end
%%--------------------------------------------------------------------
-spec default_pipeline() -> [atom(), ...].

default_pipeline() ->
    [ nostr_relay_module_init
    , nostr_relay_module_nip01
    ].

%%--------------------------------------------------------------------
%% @doc returns the default handler state used.
%% @end
%%--------------------------------------------------------------------
-spec default_state() -> map().

default_state() ->
    #{ websocket_pipeline => default_pipeline() }.

%%--------------------------------------------------------------------
%% @doc start a new listener.
%% @todo fix the specification
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, pid()}.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc initialize a new listener.
%% @todo fix the specifications
%% @todo add multi port/domain support
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, #?MODULE{ pid :: pid() }}.

init(Args) ->
    % get general information about the new cowboy server we will
    % start.
    Host = maps:get(host, Args, "localhost"),
    HandlerState = maps:get(state, Args, default_state()),
    Port = maps:get(port, Args, 4000),

    % create routes, dispatch and options for the socket.
    Routes = routes(HandlerState),
    Dispatch = cowboy_router:compile(Routes),
    TransportOpts = [{port, Port}],
    ProtocolOpts = #{ env => #{ dispatch => Dispatch }},

    % if needed, start all modules to initialize them (useful for
    % databases).
    nostr_relay_module:start_modules(modules()),

    % craft the name containing the name of the module, the host and
    % the port of this new server, and start it.
    Name = {?MODULE, Host, Port},
    {ok, Pid} = cowboy:start_clear(Name, TransportOpts, ProtocolOpts),

    % create a new global state containing all previous information.
    GlobalState = #?MODULE{ name = Name
                          , transport = TransportOpts
                          , protocol = ProtocolOpts
                          , state = HandlerState
                          , pid = Pid
                          },
    {ok, GlobalState}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc By default, all routes are set directly into a database, it
%% gives us the ability to remove/modify/add them dynamically.
%% @end
%%--------------------------------------------------------------------
-spec routes(term()) -> term().

routes(HandlerState) ->
    [{'_', [{'_', nostr_relay_handler, HandlerState}] }].

%%--------------------------------------------------------------------
%% @doc stop a listener
%% @todo fix the specifications
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: any(),
      State :: #?MODULE{ pid :: pid() },
      Return :: ok.

terminate(_Reason, #?MODULE{ name = Name } = _State) ->
    cowboy:stop_listener(Name).

%%--------------------------------------------------------------------
%% @doc stop the process if a message is received.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: any(),
      State :: #?MODULE{ pid :: pid() },
      Return :: {stop, {received, cast, Message}, State}.

handle_cast(Message, State) ->
    {stop, {received, cast, Message}, State}.

%%--------------------------------------------------------------------
%% @doc stop the process if a message is received.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: any(),
      From :: any(),
      State :: #?MODULE{ pid :: pid() },
      Return :: {stop, {received, call, Message, From}, State}.

handle_call(Message, From, State) ->
    {stop, {received, call, Message, From}, State}.

%%--------------------------------------------------------------------
%% @doc stop the process if a message is received.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: any(),
      State :: #?MODULE{ pid :: pid() },
      Return :: {stop, {received, info, Message}, State}.

handle_info(Message, State) ->
    {stop, {received, info, Message}, State}.
