%%%===================================================================
%%% @doc An example of nip01 implementation as module callback.
%%% @end
%%%===================================================================
-module(websocket_server_action_nip01).
-export([init/2]).
-include_lib("nostrlib/include/nostrlib.hrl").

-spec init(any(), any()) -> any().
init(#event{} = Event, _State) ->
    io:format("debug: test~n"),
    {{next, websocket_server_action_store}, Event};
init(#request{} = _Request, _State) ->
    Notice = #notice{ message = <<"Received request!">> },
    {stop, Notice};
init(#close{} = _Close, _State) ->
    Notice = #notice{ message = <<"Received close!">> },
    {stop, Notice};
init(_Data, _State) ->
    Notice = #notice{ message = <<"well done!">> },
    {stop, Notice}.

