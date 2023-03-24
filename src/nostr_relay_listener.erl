%%%===================================================================
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
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-record(state, { name = undefined
               , transport = undefined
               , protocol = undefined
               , pid = undefined 
               }).

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
      Return :: {ok, #state{ pid :: pid() }}.
init(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    Domain = proplists:get_value(domain, Args, '_'),
    Name = {nostr_relay_listener, Port},
    TransportOpts = [{port, Port}],
    RouteOpts = [{port, Port}, {domain, Domain}],
    Routes = [{Domain, [{"/", nostr_relay_handler, RouteOpts}]}],
    Dispatch = cowboy_router:compile(Routes),
    ProtocolOpts = #{ env => #{ dispatch => Dispatch }},
    {ok, Pid} = cowboy:start_clear(Name, TransportOpts, ProtocolOpts),
    State = #state{ name = Name
                  , transport = TransportOpts
                  , protocol = ProtocolOpts
                  , pid = Pid 
                  },
    pg:join(relay, {?MODULE, Port, Domain}, self()),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc stop a listener
%% @todo fix the specifications
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: any(),
      State :: #state{ pid :: pid() },
      Return :: ok.
terminate(_Reason, #state{ name = Name } = _State) ->
    cowboy:stop_listener(Name).

%%--------------------------------------------------------------------
%% @doc stop the process if a message is received.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: any(),
      State :: #state{ pid :: pid() },
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
      State :: #state{ pid :: pid() },
      Return :: {stop, {received, call, Message, From}, State}.
handle_call(Message, From, State) ->
    {stop, {received, call, Message, From}, State}.

%%--------------------------------------------------------------------
%% @doc stop the process if a message is received.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: any(),
      State :: #state{ pid :: pid() },
      Return :: {stop, {received, info, Message}, State}.
handle_info(Message, State) ->
    {stop, {received, info, Message}, State}.
