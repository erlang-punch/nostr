%%%===================================================================
%%%
%%%===================================================================
-module(websocket_server_nip01).
-export([start/0]).
-export([init/2]).
-export([get_event/1, exist_event/1, create_event/1, list_events/0]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

-spec start() -> any().
start() ->
    mnesia:create_table(event
                       ,[{attributes, record_info(fields, event)}]).

% when data is valid, we must insert it in the database.
-spec init(any(), any()) -> any().
init({#event{} = Event, _Info}, State) ->
    case create_event(Event) of
        {ok, _} ->
            Message = message(Event#event.id, true, <<>>),
            {[{text, Message}], State};
        {error, exist} ->
            Message = message(Event#event.id, true, <<"duplicate: already have this event">>),
            {[{text, Message}], State}
    end;
init({#close{}, _Info}, State) ->
    Message = <<"unsupported">>,
    {ok, Notice} = nostrlib:encode(#notice{ message = Message }),
    {[{text, Notice}], State};
init({#request{}, _Info}, State) ->
    Message = <<"unsupported">>,
    {ok, Notice} = nostrlib:encode(#notice{ message = Message }),
    {[{text, Notice}], State}.

-spec message(binary(), boolean(), binary()) -> binary().
message(EventId, State, Message) ->
    Final = [<<"OK">>, nostrlib:binary_to_hex(EventId), State, Message],
    thoas:encode(Final).

-spec get_event(event()) -> any().
get_event(#event{} = Event) ->
    Fun = fun() -> mnesia:read(event, Event#event.id) end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> {error, not_found};
        {atomic, [Result]} -> {ok, Result};
        Error -> {error, Error}
    end.

-spec exist_event(event()) -> boolean().
exist_event(#event{} = Event) ->
    case get_event(Event) of
        {ok, _} -> true;
        _ -> false
    end.

-spec list_events() -> any().
list_events() ->
    Fun = fun() -> mnesia:select(event, [{'$1', [], ['$1']}]) end,
    mnesia:transaction(Fun).

-spec create_event(event()) -> any().
create_event(#event{} = Event) ->
    Fun = fun() -> 
                  case exist_event(Event) of
                      true -> exist;
                      false -> mnesia:write(Event) 
                  end
          end,
    case mnesia:transaction(Fun) of
        {atomic, exist} -> {error, exist};
        {atomic, _} -> {ok, Event};
        Error -> {error, Error}
    end.

