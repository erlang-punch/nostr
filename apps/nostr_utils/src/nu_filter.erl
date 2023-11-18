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
match(_EventList, _FilterList) -> todo.

%%--------------------------------------------------------------------
%% @doc Check if a filter list match an event.
%% @end
%%--------------------------------------------------------------------
-spec match_event(term(), list()) -> any().
match_event(_Event, []) -> [];
match_event(_Event, _FilterList) -> todo.

%%--------------------------------------------------------------------
%% @doc Return true if a filter match an event, else false.
%% @end
%%--------------------------------------------------------------------
-spec match_event_filter(term(), term()) -> boolean().

match_event_filter(#event{} = _Event, #filter{} = _Filter) ->
    todo;
match_event_filter(_Event, _Filter) -> false.

%%--------------------------------------------------------------------
%% @hidden
%% @doc check if an event id is present in filtering ids list
%% @end
%%--------------------------------------------------------------------
-spec match_event_id(#event{}, #filter{}) -> boolean().

match_event_id(#event{ id = EventId }, #filter{ event_ids = EventIds }) ->
    Fun = fun (X) when X =:= EventId -> true; 
              (_) -> false 
          end,
    case lists:filter(Fun, EventIds) of
        [] -> false;
        _ -> true
    end.

-spec match_event_id_test() -> any().
match_event_id_test() ->
    Opts = [{year,2020},{month,01},{day, 01},{hour,00},{minute,00},{second,01}],
    E = nu_filter:generate_random_event(<<1:256>>, Opts),

    % a filter with an empty event_ids list should always return
    % false.
    ?assertEqual(false, match_event_id(E, #filter{ event_ids = [] })),

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

match_event_author(#event{ public_key = Author }, #filter{ authors = Authors }) ->
    Fun = fun (X) when X =:= Author -> true; 
              (_) -> false 
          end,
    case lists:filter(Fun, Authors) of
        [] -> false;
        _ -> true
    end.

-spec match_event_author_test() -> any().
match_event_author_test() ->
    Opts = [{year,2020},{month,01},{day, 01},{hour,00},{minute,00},{second,01}],
    E = nu_filter:generate_random_event(<<1:256>>, Opts),
    {ok, Public} = nostrlib_schnorr:new_publickey(<<1:256>>),

    % a filter with an empty authors list must always return false
    % here.
    ?assertEqual(false, match_event_author(E, #filter{ authors = [] })),

    % a filter where an author public key is not in the authors list
    % must return false
    WrongAuthors = [<<0:256>>, <<1:256>>],
    ?assertEqual(false, match_event_author(E, #filter{ authors = WrongAuthors })),
    
    % a filter containing an author public key in the authors list
    % must return true.
    RightAuthors = [<<0:256>>, <<1:256>>, Public],
    ?assertEqual(true, match_event_author(E, #filter{ authors = RightAuthors })).


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
    Content = proplists:get_value(content, Opts, <<>>),
    Tags = proplists:get_value(tags, Opts, []),
    Kind = proplists:get_value(kind, Opts, text_note),
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
                   ],
    FixedEvent_01 = generate_random_event(PrivateKey_01, FixedOpts_01),

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
    ?assertEqual(true, match_event_filter(FixedEvent_01, Filter_01)).

