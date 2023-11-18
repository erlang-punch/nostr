-module(nu_filter).
-export([match/2, match_event/2, match_event_filter/2]).
-export([generate_random_event/1, generate_random_event/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc Check if a list of filters match a list of events.
%% @end
%%--------------------------------------------------------------------
-spec match(list(), list()) -> any().
match([], []) -> [];
match([], _EventList) -> [];
match(_EventList, []) -> [];
match(EventList, FilterList) ->
    Fun = fun(Event) -> match_event(Event, FilterList) end,
    lists:filter(Fun, EventList).

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
    E = nu_filter:generate_random_event(<<1:256>>, Opts),

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
    E = nu_filter:generate_random_event(<<1:256>>, Opts),
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
    E = nu_filter:generate_random_event(<<1:256>>, Opts),
    
    % a filter with an empty kinds list must always return true
    % here.
    ?assertEqual(true, match_event_kind(E, #filter{ kinds = [] })),
    ?assertEqual(true, match_event_kind(E, #filter{ kinds = undefined })),

    % a filter where an kind public key is not in the kinds list
    % must return false
    WrongKinds = [metadata],
    ?assertEqual(false, match_event_kind(E, #filter{ kinds = WrongKinds })),
    
    % a filter containing an kind public key in the kinds list
    % must return true.
    RightKinds = [metadata, text_note],
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
    ?assertEqual(3, length(match(RandomEvents, [Filter_08]))).



