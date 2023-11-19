%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%%
%%% `nostr_relay_handler' module is in charge of handling cowboy
%%% websocket connections.
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_handler).
-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% `init/2' function initialize a new websocket connection when a
%% client request is coming. The websocket state contains the request
%% and some custom options (e.g. `idle_timeout').
%%
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> Return when
      Return :: {cowboy_websocket, any(), any()}.

init(Req, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Req, State]}]),
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
-spec websocket_init(term()) -> Return when 
      Return :: {ok, term()}.

websocket_init(State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, websocket_init, [State]}]),
    {[], State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc 
%%
%% `websocket_handle/2' function forwards all websocket message to
%% `nostr_relay_module:init/2' function where all the magic
%% happens. All logic part of this application is done in
%% `nostr_relay_module' module.
%%
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle(term(), term()) -> Return when
      Return :: {term(), term()}.

websocket_handle(Frame, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, websocket_handle, [Frame,State]}]),
    nostr_relay_module:init(Frame, State).

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

% define ModuleName:websocket_info/1
websocket_info({callback, ModuleName} = Info, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, [Info, State]}]),
    try erlang:apply(ModuleName, websocket_info, [State])
    catch
        error:undef:_ ->
            ?LOG_ERROR("~p",[{self(), ?MODULE, ModuleName, websocket_info, undef}]),
            {[], State};

        % this part of the code should not crash.
        Error:Reason:Stack ->
            E = {Error, Reason, Stack},
            ?LOG_ERROR("~p",[{self(), ?MODULE, ModuleName, websocket_info, E}]),
            {[], State}
    end;

% define ModuleName:websocket_info/2
websocket_info({callback, ModuleName, Args} = Info, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, websocket_info, [Info, State]}]),
    try erlang:apply(ModuleName, websocket_info, [Args, State]) 
    catch
        error:undef:_ ->
            ?LOG_ERROR("~p",[{self(), ?MODULE, ModuleName, websocket_info, undef}]),
            {[], State};

        % this part of the code should not crash.
        Error:Reason:Stack ->
            E = {Error, Reason, Stack},
            ?LOG_ERROR("~p",[{self(), ?MODULE, ModuleName, websocket_info, E}]),
            {[], State}
    end;

% other messages are simply dropped and logged.
websocket_info(Info, State) ->
    ?LOG_WARNING("~p", [{self(), ?MODULE, websocket_info, [Info, State]}]),
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
