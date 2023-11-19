%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%%
%%% This module requires an important number of optimization to
%%% support an important load. All events are using lists... It works
%%% for few events, but will be quite difficult to scale when having
%%% lot of data. Because events will be stored in mnesia, continuation
%%% should be used, with, if possible, QLC. At this time, this module
%%% just work. Refacto will be required soon though.
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_events).
-export([match/2, match_limit/2, match_event/2, match_event_filter/2]).
-export([generate_random_event/1, generate_random_event/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc Check if a list of filters match a list of events.
%%
%% This function is quite dangerous though. We need a full eventlist,
%% that means if we are using a huge amount of events... It will
%% costly in resources. This function must be optimized, a
%% continuation method or some facilities should also be implemented.
%%
%% @end
%%--------------------------------------------------------------------
-spec match(list(), list()) -> any().

match([], []) -> [];
match([], _EventList) -> [];
match(_EventList, []) -> [];
% if we have only one event
match(#event{} = Event, FilterList) ->
    match([Event], FilterList);
% if only one filter is set
match(Event, #filter{} = Filter) ->
    match(Event, [Filter]);
match(EventList, FilterList) ->
    Fun = fun(Event) -> match_event(Event, FilterList) end,
    Events = lists:filter(Fun, EventList),
    sort_events_by_created_at(Events).

%%--------------------------------------------------------------------
%% @doc Return a limited number of events based on the limit field.
%%
%% Note: the specification does not explain how to deal with limit if
%% a list of filters is given, the only thing is if this field is
%% present, we should limit the number of events during the initial
%% query.
%%
%% @end
%%--------------------------------------------------------------------
-spec match_limit(list(), list()) -> list().

match_limit([], []) -> [];
match_limit(_EventList, []) -> [];
match_limit([], _FilterList) -> [];
% if we have only one event
match_limit(#event{} = Event, FilterList) ->
    match_limit([Event], FilterList);
% if only one filter is set
match_limit(Event, #filter{} = Filter) ->
    match_limit(Event, [Filter]);
match_limit(EventList, FilterList) ->
    Events =  match(EventList, FilterList),
    case get_limit(FilterList) of
        undefined -> Events;
        Limit when is_integer(Limit) ->
            SortedEvents = sort_events_by_created_at(Events),
            take(SortedEvents, Limit)
    end.

%%--------------------------------------------------------------------
%% @doc this function sort events using created_at field.
%%
%% @todo optimize this function.
%% @end
%%--------------------------------------------------------------------
-spec sort_events_by_created_at(list()) -> list().

sort_events_by_created_at(Events) ->
    CreatedAtKey = [ {E#event.created_at, E} || E <- Events ],
    Sorted = lists:sort(CreatedAtKey),
    Reversed = lists:reverse(Sorted),
    [ E || {_, E} <- Reversed ].

%%--------------------------------------------------------------------
%% @doc A quick and dirty way to extract a fixed number of events from
%% an event list.
%%
%% @end
%%--------------------------------------------------------------------
-spec take(list(), pos_integer()) -> list().

take(Events, Limit) -> take(Events, Limit, []).
take([], _, Buffer) -> lists:reverse(Buffer);
take(_, 0, Buffer) -> lists:reverse(Buffer);
take(_, Limit, _) when Limit < 1 -> [];
take([Event|Rest], Limit, Buffer) ->
    take(Rest, Limit-1, [Event|Buffer]).

%%--------------------------------------------------------------------
%% 1. we want to extract all limit fields from filter list, then we
%% take the last one.
%%--------------------------------------------------------------------
-spec get_limit(list()) -> undefined | integer().

get_limit(FilterList) ->
    Fun = fun(#filter{limit = Limit}, _) when is_integer(Limit) -> Limit;
             (_, Acc) -> Acc
          end,
    lists:foldl(Fun, undefined, FilterList).

%%--------------------------------------------------------------------
%% @doc Check if a filter list match an event.
%% @end
%%--------------------------------------------------------------------
-spec match_event(term(), list()) -> any().

match_event(_Event, []) -> [];
match_event(Event, FilterList) ->
    Result = [ match_event_filter(Event, Filter)
               || Filter <- FilterList ],
    Fun = fun(X,A) -> X or A end,
    lists:foldl(Fun, false, Result).

%%--------------------------------------------------------------------
%% @doc Return true if a filter match an event, else false.
%% @end
%%--------------------------------------------------------------------
-spec match_event_filter(term(), term()) -> boolean().

match_event_filter(#event{} = Event, #filter{} = Filter) ->
    % this is quite slow, but it works.
    MatchEvent = match_event_id(Event, Filter),
    MatchAuthor = match_event_author(Event, Filter),
    MatchKind = match_event_kind(Event, Filter),
    MatchSince = match_event_since(Event, Filter),
    MatchUntil = match_event_until(Event, Filter),
    case {MatchEvent, MatchAuthor, MatchKind, MatchSince, MatchUntil} of
        {true,true,true,true,true} -> true;
        _ -> false
    end;
match_event_filter(_Event, _Filter) -> false.

%%--------------------------------------------------------------------
%% @hidden
%% @doc check if an event id is present in filtering ids list
%% @end
%%--------------------------------------------------------------------
-spec match_event_id(#event{}, #filter{}) -> boolean().

match_event_id(_, #filter{ event_ids = undefined }) -> true;
match_event_id(_, #filter{ event_ids = [] }) -> true;
match_event_id(#event{ id = EventId }, #filter{ event_ids = EventIds }) ->
    Fun = fun (EI) -> EI =:= EventId end,
    case lists:filter(Fun, EventIds) of
        [] -> false;
        _ -> true
    end.

-spec match_event_id_test() -> any().

match_event_id_test() ->
    Opts = [{year,2020},{month,01},{day, 01},{hour,00},{minute,00},{second,01}],
    E = ?MODULE:generate_random_event(<<1:256>>, Opts),

    % a filter with an empty event_ids list should always return
    % true (undefined).
    ?assertEqual(true, match_event_id(E, #filter{ event_ids = undefined })),
    ?assertEqual(true, match_event_id(E, #filter{ event_ids = [] })),

    % a filter must return false when an event id is not in event_ids
    % list.
    WrongIds = [<<0:256>>, <<1:256>>],
    ?assertEqual(false, match_event_id(E, #filter{ event_ids = WrongIds })),

    % a filter must return true when an event id is present in
    % event_ids list
    RightIds = [<<0:256>>, <<1:256>>, E#event.id],
    ?assertEqual(true, match_event_id(E, #filter{ event_ids = RightIds })).

%%--------------------------------------------------------------------
%% @hidden
%% @doc check if an event id is present in filtering ids list
%% @end
%%--------------------------------------------------------------------
-spec match_event_author(#event{}, #filter{}) -> boolean().

match_event_author(_, #filter{ authors = undefined }) -> true;
match_event_author(_, #filter{ authors = [] }) -> true;
match_event_author(#event{ public_key = Author }, #filter{ authors = Authors }) ->
    Fun = fun (A) -> A =:= Author end,
    case lists:filter(Fun, Authors) of
        [] -> false;
        _ -> true
    end.

-spec match_event_author_test() -> any().
match_event_author_test() ->
    Opts = [{year,2020},{month,01},{day, 01}
           ,{hour,00},{minute,00},{second,01}],
    E = ?MODULE:generate_random_event(<<1:256>>, Opts),
    {ok, Public} = nostrlib_schnorr:new_publickey(<<1:256>>),

    % a filter with an empty authors list must always return true
    % here (undefined).
    ?assertEqual(true, match_event_author(E, #filter{ authors = undefined })),
    ?assertEqual(true, match_event_author(E, #filter{ authors = [] })),

    % a filter where an author public key is not in the authors list
    % must return false
    WrongAuthors = [<<0:256>>, <<1:256>>],
    ?assertEqual(false, match_event_author(E, #filter{ authors = WrongAuthors })),

    % a filter containing an author public key in the authors list
    % must return true.
    RightAuthors = [<<0:256>>, <<1:256>>, Public],
    ?assertEqual(true, match_event_author(E, #filter{ authors = RightAuthors })).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec match_event_kind(#event{}, #filter{}) -> boolean().

match_event_kind(_, #filter{ kinds = undefined }) -> true;
match_event_kind(_, #filter{ kinds = [] }) -> true;
match_event_kind(#event{ kind = Kind }, #filter{ kinds = Kinds }) ->
    Fun = fun (K) -> K =:= Kind end,
    case lists:filter(Fun, Kinds) of
        [] -> false;
        _ -> true
    end.

-spec match_event_kind_test() -> any().
match_event_kind_test() ->
    Opts = [{year,2020},{month,01},{day, 01}
           ,{hour,00},{minute,00},{second,01}
           ,{kind,text_note}],
    E = ?MODULE:generate_random_event(<<1:256>>, Opts),

    % a filter with an empty kinds list must always return true
    % here.
    ?assertEqual(true, match_event_kind(E, #filter{ kinds = [] })),
    ?assertEqual(true, match_event_kind(E, #filter{ kinds = undefined })),

    % a filter where an kind public key is not in the kinds list
    % must return false
    WrongKinds = [set_metadata],
    ?assertEqual(false, match_event_kind(E, #filter{ kinds = WrongKinds })),

    % a filter containing an kind public key in the kinds list
    % must return true.
    RightKinds = [set_metadata, text_note],
    ?assertEqual(true, match_event_kind(E, #filter{ kinds = RightKinds })).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec match_event_since(#event{}, #filter{}) -> boolean().

match_event_since(_, #filter{ since = undefined }) -> true;
match_event_since(#event{ created_at = CreatedAt }, #filter{ since = Since }) ->
    Since =< CreatedAt.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec match_event_until(#event{}, #filter{}) -> boolean().

match_event_until(_, #filter{ until = undefined }) -> true;
match_event_until(#event{ created_at = CreatedAt }, #filter{ until = Until }) ->
    CreatedAt =< Until.

%%--------------------------------------------------------------------
%% @hidden
%% @doc this function should be available only in testing mode, and
%% then move into common_test SUITE.
%% @end
%%--------------------------------------------------------------------
-spec generate_random_event(binary()) -> #event{}.
generate_random_event(PrivateKey) ->
    generate_random_event(PrivateKey, [{content, <<"test">>}]).

%%--------------------------------------------------------------------
%% @doc
%% ```
%% [ nu_filter:generate_random_event(<<1:256>>, [{year, Y}])
%%   || Y <- lists:seq(1970,2020) ].
%% '''
%% @end
%%--------------------------------------------------------------------
-spec generate_random_event(binary(), proplists:proplists()) -> #event{}.
generate_random_event(PrivateKey, Opts) ->
    RandomContent = << <<($a + rand:uniform(25))>>
                      || _ <- lists:seq(1,rand:uniform(50)) >>,
    Content = proplists:get_value(content, Opts, RandomContent),
    Tags = proplists:get_value(tags, Opts, []),
    RandomKind = fun() -> case rand:uniform(2) of
                              1 -> set_metadata;
                              2 -> text_note
                          end
                 end,
    Kind = proplists:get_value(kind, Opts, RandomKind()),
    Year = proplists:get_value(year, Opts, 2020),
    Month = proplists:get_value(month, Opts, rand:uniform(11)+1),
    Day = proplists:get_value(day, Opts, rand:uniform(27)+1),
    Hour = proplists:get_value(hour, Opts, rand:uniform(23)),
    Minute = proplists:get_value(minute, Opts, rand:uniform(59)),
    Second = proplists:get_value(second, Opts, rand:uniform(59)),
    Event = #event{ created_at = {{Year,Month,Day},{Hour,Minute,Second}},
                    kind = Kind,
                    tags = Tags,
                    content = Content
                  },
    {ok, EncodedEvent} = nostrlib:encode(Event, [{private_key, PrivateKey}]),
    {ok, E, _} = nostrlib:decode(EncodedEvent),
    E.

-spec match_test() -> any().
match_test() ->
    % generate a new participant. Example only, this is static data,
    % the key used here should not be used in production environment.
    PrivateKey_01 = <<1:256>>,
    {ok, PublicKey_01} = nostrlib_schnorr:new_publickey(PrivateKey_01),

    % When using fixed date/time, with same parameters, it should
    % produce the same event (assuming we have the private and the
    % public key).
    FixedOpts_01 = [{year,2020},{month,01},{day, 01}
                   ,{hour,00},{minute,00},{second,01}
                   ,{kind,text_note}
                   ,{content, <<>>}],
    FixedEvent_01 = generate_random_event(PrivateKey_01, FixedOpts_01),

    % default options used to generate random events.
    RandomEventOpts = [{year,2020},{month,01},{day, 01}
                      ,{minute,00},{second,01}
                      ,{content, <<"test">>}],

    % Generate 5 random event text_note
    RandomEvent_Note = [ generate_random_event(PrivateKey_01
                                              ,[{hour, H}
                                               ,{kind,text_note}
                                               |RandomEventOpts])
                         || H <- lists:seq(1,5) ],

    % Generate 5 random event set_metadata
    RandomEvent_Meta = [ generate_random_event(PrivateKey_01
                                              ,[{hour, H}
                                               ,{kind,set_metadata}
                                               |RandomEventOpts])
                         || H <- lists:seq(1,5) ],

    % we have now 10+1 random events
    RandomEvents = RandomEvent_Note
        ++ RandomEvent_Meta
        ++ [FixedEvent_01],

    % extract the event id, we must be sure the id is always the same,
    % but we also must be sure all other value are not moving as
    % well. The signature, in this case, must be okay.
    FixedEvent_01_id = <<168,219,255,132,6,172,15,138,66,26,207
                        ,219,139,145,136,150,128,103,30,245,159
                        ,100,134,70,185,146,75,182,124,232,212,45>>,
    ?assertEqual(FixedEvent_01#event.id, FixedEvent_01_id),
    ?assertEqual(FixedEvent_01#event.public_key, PublicKey_01),
    ?assertEqual(FixedEvent_01#event.kind, text_note),
    ?assertEqual(FixedEvent_01#event.tags, []),
    ?assertEqual(FixedEvent_01#event.content, <<>>),

    % now we need to create a new filter that MUST match our
    % previously created event. We already have all information
    % required to do that in this event, let try it.
    Filter_01 = #filter{ event_ids = [FixedEvent_01_id] },
    ?assertEqual(true, match_event_filter(FixedEvent_01, Filter_01)),
    ?assertEqual(true, match_event(FixedEvent_01, [Filter_01])),
    ?assertEqual([FixedEvent_01], match([FixedEvent_01], [Filter_01])),
    ?assertEqual([FixedEvent_01], match(RandomEvents, [Filter_01])),

    % let check if we can filter kinds
    Filter_02 = #filter{ kinds = [set_metadata] },
    ?assertEqual(5, length(match(RandomEvents, [Filter_02]))),
    Filter_03 = #filter{ kinds = [text_note] },
    ?assertEqual(6, length(match(RandomEvents, [Filter_03]))),

    % let check if we can filter authors
    Filter_04 = #filter{ authors = [PublicKey_01] },
    ?assertEqual(11, length(match(RandomEvents, [Filter_04]))),
    Filter_05 = #filter{ authors = [<<3:256>>] },
    ?assertEqual(0, length(match(RandomEvents, [Filter_05]))),

    % let check if we can filter since and until, we should find 2
    % value from the events we generated.
    Filter_06 = #filter{ since = 1577840401, until = 1577840401 },
    ?assertEqual(2, length(match(RandomEvents, [Filter_06]))),

    % here we check since, we should find 10 events.
    Filter_07 = #filter{ since = 1577840401 },
    ?assertEqual(10, length(match(RandomEvents, [Filter_07]))),

    % here we check until, we should find 2 random events + the fixed
    % one.
    Filter_08 = #filter{ until = 1577840401 },
    ?assertEqual(3, length(match(RandomEvents, [Filter_08]))),

    % we can now check the limit.
    Filter_09 = #filter{ limit = 0 },
    ?assertEqual(0, length(match_limit(RandomEvents, [Filter_09]))),

    % the last limit should be used.
    Filter_10 = [#filter{ limit = 1 }, #filter{ limit = 5}],
    ?assertEqual(5, length(match_limit(RandomEvents, Filter_10))),

    % by default, no limit is applied and we send all events.
    Filter_11 = #filter{},
    ?assertEqual(11, length(match_limit(RandomEvents, [Filter_11]))).
