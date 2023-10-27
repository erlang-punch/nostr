%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc A simple, quick and dirty, websocket client only using
%%% gun. This module was created for testing purpose only and offer a
%%% decent way to send raw data to a websocket server.
%%%
%%% == Usage ==
%%%
%%% ```
%%% {ok, Pid} = websocket_client:start("wss://myremoteserver:443/").
%%% ok = websocket_client:send(Pid, "text message").
%%% ok = websocket_client:send(Pid, <<1,2,3,"binary message">>).
%%% ok = websocket_client:stop(Pid).
%%% '''
%%%
%%% @end
%%%===================================================================
-module(websocket_client).
-behavior(gen_server).
-export([start/1, start/2, start/3, start/4]).
-export([stop/1]).
-export([send/2]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc start a new websocket connection using an URL.
%% @end
%%--------------------------------------------------------------------
-spec start(string()) -> {ok, pid()}.
start(Url) ->
    Default = #{ port => 80, path => "/" },
    case maps:merge(Default, uri_string:parse(Url)) of
        #{ scheme := "ws", host := Host, port := Port, path := []} ->
            start(Host, Port, "/", #{});
        #{ scheme := "ws", host := Host, port := Port, path := Path} ->
            start(Host, Port, Path, #{});
        #{ scheme := "wss", host := Host, port := Port, path := []} ->
            Opts = #{ transport => tls
                    , tls_opts => [{verify, verify_none}]
                    },
            start(Host, Port, "/", Opts);
        #{ scheme := "wss", host := Host, port := Port, path := Path} ->
            Opts = #{ transport => tls
                    , tls_opts => [{verify, verify_none}]
                    },
            start(Host, Port, Path, Opts);
        #{ scheme := _ } ->
            {error, unsupported_protocol}
    end.

%%--------------------------------------------------------------------
%% @doc start a new server using only host and port
%% @end
%%--------------------------------------------------------------------
-spec start(string(), pos_integer()) -> {ok, pid()}.
start(Host, Port) ->
    start(Host, Port, "/").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start(string(), pos_integer(), string()) -> {ok, pid()}.
start(Host, Port, Path) ->
    start(Host, Port, Path, #{}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start(string(), pos_integer(), string(), map()) -> {ok, pid()}.
start(Host, Port, Path, Opts) ->
    Args = #{ url => Host, port => Port, path => Path, opts => Opts },
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc sends a message to a connected server.
%% @end
%%--------------------------------------------------------------------
-spec send(pid(), string() | binary()) -> ok.
send(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).

%%--------------------------------------------------------------------
%% @doc stop a process.
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(map()) -> {ok, map()}.
init(#{ url := Url, port := Port, path := Path, opts := Opts } = Args) ->
    {ok, Pid} = gun:open(Url, Port, Opts),
    {ok, Protocol} = gun:await_up(Pid),
    StreamRef = gun:ws_upgrade(Pid, Path),
    State = #{ args => Args
             , conn => Pid
             , protocol => Protocol
             , stream => StreamRef
             },
    {ok, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), term(), map()) -> any().
handle_call(Message, From, State) ->
    ?LOG_WARNING("~p", [{Message, From, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), map()) -> any().
handle_cast({send, Message}, #{ conn := Pid, stream := StreamRef } = State)
  when is_list(Message) ->
    Payload = [{text, Message}],
    gun:ws_send(Pid, StreamRef, Payload),
    {noreply, State};
handle_cast({send, Message}, #{ conn := Pid, stream := StreamRef } = State)
  when is_binary(Message) ->
    Payload = [{binary, Message}],
    gun:ws_send(Pid, StreamRef, Payload),
    {noreply, State};
handle_cast(Message, State) ->
    ?LOG_WARNING("~p", [{Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), map()) -> any().
handle_info({gun_error, _, _, closed} = Message, State) ->
    ?LOG_INFO("~p", [{Message, State}]),
    {stop, normal, State};
handle_info({gun_down, _Pid, _, closed, _} = Message, State) ->
    ?LOG_INFO("~p", [{Message, State}]),
    {stop, normal, State};
handle_info(Message, State) ->
    ?LOG_WARNING("~p", [{Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), map()) -> ok.
terminate(_Reason, #{ conn := Conn } = _State) ->
    gun:close(Conn),
    ok.
