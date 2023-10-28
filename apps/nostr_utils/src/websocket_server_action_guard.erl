%%%===================================================================
%%% @doc This module should validate the content of the message
%%% received by nostr client, if valid, it goes to the next action.
%%%
%%% @end
%%%===================================================================
-module(websocket_server_action_guard).
-export([init/2]).
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc parse a message and if valid go to the next action.
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().

init({text, Message}, _State) ->
    case nostrlib:decode(Message) of
        {ok, Decoded, _} ->
            {next, Decoded};
        _ ->
            Return = #notice{ message = <<"command not supported">> },
            {stop, Return}
    end;
init({binary, _}, _State) ->
    Return = #notice{ message = <<"binary not supported">> },
    {stop, Return};
init(_Data, _State) ->
    Return = #notice{ message = <<"unsupported command">> },
    {stop, Return}.
