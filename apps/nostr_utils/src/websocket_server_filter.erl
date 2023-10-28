%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%%
%%% ```
%%% rr(nostrlib).
%%% {ok, S} = websocket_server:start("localhost", 8081).
%%% {ok, C} = websocket_client:start("ws://localhost:8081").
%%% {ok, E} = nostrlib:encode(#event{ content = <<"test">>
%%%                                 , kind = text_note }
%%%                          ,[{private_key, <<1:256>>}]).
%%% websocket_client:send(C, binary_to_list(E)).
%%% '''
%%%
%%% @end
%%%===================================================================
-module(websocket_server_filter).
-export([start/0]).
-export([init/2]).
-export([get_event/1, exist_event/1, create_event/1]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

-record(nostr_relays, { name
                      , domain
                      , path
                      , configuration
                      }).

-spec start() -> any().
start() ->
    mnesia:create_table(event
                       ,[{attributes, record_info(fields, event)}]),
    mnesia:create_table(nostr_relays
                       ,[{attributes, record_info(fields, nostr_relays)}]).

-spec init(term(), term()) -> term().
init({binary, _} = _Frame, State) ->
    Message = <<"binary messages are not accepted">>,
    {ok, Notice} = nostrlib:encode(#notice{ message = Message }),
    {[{text, Notice}], State};
init({text, Payload} = _Frame, State) ->
    ?LOG_INFO("~p", [{?MODULE, Payload}]),
    case nostrlib:decode(Payload) of
        {ok, Data, Info} ->
            nip_01(Data, Info, State);
        _ ->
            Message = <<"could not parse command">>,
            {ok, Notice} = nostrlib:encode(#notice{ message = Message }),
            {[{text, Notice}], State}
    end.

% when data is valid, we must insert it in the database.
nip_01(#event{} = Event, _Info, State) ->
    case create_event(Event) of
        {ok, _} ->
            Message = message(Event#event.id, true, <<>>),
            {[{text, Message}], State};
        {error, exist} ->
            Message = message(Event#event.id, true, <<"duplicate: already have this event">>),
            {[{text, Message}], State}
    end;
nip_01(#close{}, _Info, State) ->
    Message = <<"unsupported">>,
    {ok, Notice} = nostrlib:encode(#notice{ message = Message }),
    {[{text, Notice}], State};
nip_01(#request{}, _Info, State) ->
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
