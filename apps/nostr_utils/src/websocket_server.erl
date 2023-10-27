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
    gen_server:start(?MODULE, #{ host => Host, port => Port }, []).

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
init(#{ host := _Host, port := Port }) ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [{"/", ?MODULE, []}]}
    ]),
    Opts = [{port, Port}],
    Env = #{ env => #{ dispatch => Dispatch }},
    {ok, Pid} = cowboy:start_clear({?MODULE, Port}, Opts, Env),
    erlang:link(Pid),
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
    {cowboy_websocket, Req, State, #{ idle_timeout => 60000*5}}.

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
websocket_handle({text, _Message} = Frame, State) ->
    ?LOG_INFO("~p", [{?MODULE, websocket_handle, Frame, State}]),
    {[Frame], State};
websocket_handle({binary, _Binary} = Frame, State) ->
    ?LOG_INFO("~p", [{?MODULE, websocket_handle, Frame, State}]),
    {[Frame], State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(any(), any()) -> {ok, any()}.
websocket_info(Info, State) ->
    ?LOG_INFO("~p", [{?MODULE, websocket_info, Info, State}]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc cowboy callback.
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), any(), any()) -> ok.
terminate(_Reason, _Req, _State) -> ok.
