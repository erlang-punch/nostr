%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc A simple, quick and dirty, websocket server.
%%%
%%% == Usage ==
%%%
%%% ```
%%% {ok, Pid} = websocket_server:start("localhost", 8080).
%%% ok = websocket_server:stop(Pid).
%%% '''
%%%
%%% @end
%%%===================================================================
-module(websocket_server).
-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
-export([start/2, stop/1]).
-export([level/1]).
% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
% cowboy callbacks
-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

%%--------------------------------------------------------------------
%% @doc start a new listener.
%% @end
%%--------------------------------------------------------------------
-spec start(string(), pos_integer()) -> {ok, pid()}.
start(Host, Port) ->
    State = #{ websocket_pipeline => [ websocket_server_action_guard
                                     , websocket_server_action_nip01
                                     ]},
                             
    gen_server:start(?MODULE, #{ host => Host
                               , port => Port
                               , state => State
                               }
                    , []).

%%--------------------------------------------------------------------
%% @doc a wrapper around logger:set_module_level/2
%% @end
%%--------------------------------------------------------------------
-spec level(atom()) -> ok.
level(Level) ->
    logger:set_module_level(?MODULE, Level).

%%--------------------------------------------------------------------
%% @doc stops a listener.
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% @hidden
%% @doc gen_server callback.
%% @end
%%--------------------------------------------------------------------
-spec init(map()) -> {ok, pid()}.
init(#{ host := _Host, port := Port, state := S }) ->
    application:ensure_all_started(cowboy),

    % This dispatch should be "dynamically" created or simply forward
    % to a module doing the hostname/path validation if nostr is
    % supporting it (present in the database).
    Dispatch = cowboy_router:compile([
        {'_', [{'_', ?MODULE, S}]}
    ]),

    % By default we are configuring listening port
    Opts = [{port, Port}],
    Env = #{ env => #{ dispatch => Dispatch }},

    % we start a clear cowboy session
    {ok, Pid} = cowboy:start_clear({?MODULE, Port}, Opts, Env),

    % we ensure our current process is linked to the new one we
    % created
    % erlang:link(Pid),

    % we create the state containing cowboy pid.
    State = #{ listener => Pid },
    {ok, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc gen_server callback.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(any(), any(), map()) -> term().
handle_call(Message, From, State) ->
    ?LOG_WARNING("~p", [{Message, From, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc gen_server callback.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(any(), map()) -> term().
handle_cast(Message, State) ->
    ?LOG_WARNING("~p", [{Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc gen_server callback.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(any(), map()) -> term().
handle_info(Message, State) ->
    ?LOG_WARNING("~p", [{Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc gen_server callback.
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> {cowboy_websocket, any(), any()}.
init(Req, State) ->
    ?LOG_INFO("~p", [{?MODULE, init, Req, State}]),
    WebsocketOptions = #{ idle_timeout => 60_000*5 },
    WebsocketState = State#{ request => Req
                           , websocket_options => WebsocketOptions 
                           },
    {cowboy_websocket, Req, WebsocketState, WebsocketOptions}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @end
%%--------------------------------------------------------------------
-spec websocket_init(term()) -> {ok, term()}.
websocket_init(State) ->
    ?LOG_INFO("~p", [{?MODULE, websocket_init, State}]),
    {[{text, "hello!"}], State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle(term(), term()) -> {term(), term()}.

websocket_handle(Frame, State) ->
    websocket_server_module:init(Frame, State).

%% websocket_handle({text, _Message} = Frame, State) ->
%%     ?LOG_INFO("~p", [{?MODULE, websocket_handle, Frame, State}]),
%%     {[Frame], State};
%% websocket_handle({binary, _Binary} = Frame, State) ->
%%     ?LOG_INFO("~p", [{?MODULE, websocket_handle, Frame, State}]),
%%     {[Frame], State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @todo find a way to export this websocket_info to other modules,
%%       for example, if one module needs to receive one info, it 
%%       should automatically find it. example:
%%         websocket_info({ModuleName, Callback}, State) -> ...
%%         websocket_info({ModuleName, Callback, Args}, State) -> ...
%%       where Module name is the creator of event, Callback is the
%%       function to call and Args is the optional arguments list to 
%%       pass.
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(any(), any()) -> {ok, any()}.
websocket_info(subscription, #{ subscriptions := _Subscriptions } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), subscription}]),
    {ok, State};
websocket_info(Info, State) ->
    ?LOG_INFO("~p", [{?MODULE, websocket_info, Info, State}]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), any(), any()) -> ok.
terminate(_Reason, _Req, #{ timer := Timer } = _State) ->
    timer:cancel(Timer);
terminate(_Reason, _Req, _State) -> 
    ok.
