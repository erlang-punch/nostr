%%%===================================================================
%%% @doc A simple echo module.
%%%
%%% ```
%%% % start a new server
%%% {ok, S} = websocket_server:start("localhost", 8081).
%%%
%%% % start a new client
%%% {ok, C} = websocket_client:start("ws://localhost:8081").
%%%
%%% % send a text message
%%% websocket_client:send(C, "test").
%%%
%%% % send a binary message
%%% websocket_client:send(C, <<"test">>).
%%% '''
%%%
%%% @end
%%%===================================================================
-module(websocket_server_module_echo).
-export([init/2]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().

init(Message, _) -> {stop, [Message]}.
