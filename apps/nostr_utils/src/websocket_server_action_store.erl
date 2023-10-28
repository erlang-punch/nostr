%%%===================================================================
%%% @doc An example of nip01 implementation as module callback.
%%% @end
%%%===================================================================
-module(websocket_server_action_store).
-export([start/0]).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

-spec start() -> any().
start() ->
    mnesia:create_table(event
                       ,[{attributes, record_info(fields, event)}]).

-spec init(any(), any()) -> any().

init(#event{} = Event, _State) ->
    case create_event(Event) of
        {error, exist} ->
            Message = #ok{ event_id = Event#event.id
                         , accepted = true
                         , prefix = <<"duplicate">>
                         , message = <<"already have this event">>
                         },            
            {stop, Message};
        {ok, _} ->
            Message = #ok{ event_id = Event#event.id
                         , accepted = true },
            {stop, Message};
        _ ->
            Message = #ok{ event_id = Event#event.id
                         , accepted = false
                         , prefix = <<"error">>
                         , message = <<"could not connect to the database">>
                         },
            {stop, Message}
    end.

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

% -spec list_events() -> any().
% list_events() ->
%     Fun = fun() -> mnesia:select(event, [{'$1', [], ['$1']}]) end,
%     mnesia:transaction(Fun).

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
