%%%===================================================================
%%% @doc An example of nip01 implementation as module callback.
%%% @end
%%%===================================================================
-module(websocket_server_action_request).
-export([init/2]).
-include_lib("nostrlib/include/nostrlib.hrl").

-spec init(any(), any()) -> any().
init(#request{ filter = Filter } = Request, State) ->
    io:format("~p~n", [{Request, State}]),
    _Events = get_events(Filter#filter.event_ids),
    Notice = #notice{ message = <<"Received request!">> },
    {stop, Notice}.

get_events(Ids) ->
    lists:flatten([ mnesia:dirty_read(event, Event) || Event <- Ids ]).

