%%%===================================================================
%%% @doc An example of nip01 implementation as module callback.
%%% @end
%%%===================================================================
-module(nostr_relay_module_store).
-export([start/0]).
-export([init/2]).
-export([list_events/0, get_event/1, create_event/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc create `event' table in mnesia.
%%
%% This function must be called before starting `nostr_relay'
%% application or `event' table must be present on the system.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> any().

start() ->
    mnesia:create_table(event
                       ,[{attributes, record_info(fields, event)}]).

%%--------------------------------------------------------------------
%% @doc receives a valid event and store it in the database.
%%
%% Based on the previous module where the event is coming from, this
%% event should be valid. In this case, this function will be in
%% charge to store it safely. At this time, it stores all event in
%% mnesia database.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().

init(#event{} = Event, _State) ->
    case create_event(Event) of
        % @todo create dedicated functions for each case.
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

%%--------------------------------------------------------------------
%% @doc returns one element from the database using an event record.
%% @end
%%--------------------------------------------------------------------
-spec get_event(event()) -> any().

get_event(#event{} = Event) ->
    Fun = fun() -> mnesia:read(event, Event#event.id) end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> {error, not_found};
        {atomic, [Result]} -> {ok, Result};
        Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec exist_event(event()) -> boolean().

exist_event(#event{} = Event) ->
    case get_event(Event) of
        {ok, _} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc lists all events available.
%% @end
%%--------------------------------------------------------------------
-spec list_events() -> any().

list_events() ->
    Fun = fun() -> mnesia:select(event, [{'$1', [], ['$1']}]) end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @doc inserts a new event in the database.
%% @end
%%--------------------------------------------------------------------
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
