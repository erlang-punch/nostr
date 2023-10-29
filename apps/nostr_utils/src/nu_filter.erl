-module(nu_filter).
-export([match/2]).
-export([generate_random_event/1, generate_random_event/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").
-spec test() -> any().

-spec match(any(), any()) -> any().
match(_FilterList, _EventList) ->
    ok.

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
    Month = rand:uniform(11)+1,
    Day = rand:uniform(27)+1,
    Hour = rand:uniform(23),
    Minute = rand:uniform(59),
    Second = rand:uniform(59),
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
    %% % generate participant. Example only, this is static data.
    %% PRIVATE_KEY1 = <<1:256>>,
    %% {ok, PUBLIC_KEY1} = nostrlib_schnorr:new_publickey(PRIVATE_KEY1),
    %% PRIVATE_KEY2 = <<2:256>>,
    %% {ok, PUBLIC_KEY2} = nostrlib_schnorr:new_publickey(PRIVATE_KEY2),
    %% PRIVATE_KEY3 = <<3:256>>,
    %% {ok, PUBLIC_KEY3} = nostrlib_schnorr:new_publickey(PRIVATE_KEY3),
    %% PRIVATE_KEY4 = <<4:256>>,
    %% {ok, PUBLIC_KEY4} = nostrlib_schnorr:new_publickey(PRIVATE_KEY4),

    %% % Let Generate "static" events now.
    %% % nostrlib:encode(#event{ created_at = {{Year,Month,Day},{01,01,01}}

    %% % Let Generate "random" events now.
    %% RandomEvent = fun(PrivateKey) ->
    %%                       % generate year even from a close future
    %%                       Year = rand:uniform(2100)+1,
    %%                       Month = rand:uniform(13)+1,
    %%                       Day = rand:uniform(27)+1,
    %%                       Hour = rand:uniform(59),
    %%                       Minute = rand:uniform(59),
    %%                       Second = rand:uniform(59),
    %%                       {ok, EncodedEvent} = 
    %%               end,

    %% % that's a fixed element
    %% FE1 = #event{ 
    %%          id = <<115,68,195,84,131,92,215,112,189,239,237,172,199,190,68,111,96,
    %%                 213,23,53,119,34,237,2,171,142,24,54,41,171,88,51>>,
    %%          public_key = <<121,190,102,126,249,220,187,172,85,160,98,149,206,135,11,7,2,155,
    %%                         252,219,45,206,40,217,89,242,129,91,22,248,23,152>>,
    %%          created_at = 1698571782,
    %%          kind = text_note,
    %%          tags = [],
    %%          content = <<"test">>,
    %%          signature = <<184,3,117,71,251,104,57,161,15,115,249,159,239,246,119,224,215,
    %%            12,62,228,102,65,176,80,234,237,33,180,205,103,124,88,28,108,18,
    %%            191,146,217,40,192,60,188,84,175,36,153,33,124,189,82,196,55,44,
    %%            8,125,195,182,82,141,62,143,169,210,144>>
    %%         },

    %% {ok, DE1} = nostrlib:encode(#event{ content = <<"test">>
    %%                                  , kind = text_note}
    %%                           , [{private_key, <<1:256>>}]),

    %% {ok, DE2} = nostrlib:encode(#event{ content = <<"test">>
    %%                                  , kind = text_note}
    %%                           , [{private_key, <<2:256>>}]),

    %% {ok, DE3} = nostrlib:encode(#event{ content = <<"test">>
    %%                                  , kind = text_note}
    %%                           , [{private_key, <<3:256>>}]),

    %% {ok, DE4} = nostrlib:encode(#event{ content = <<"test">>
    %%                                  , kind = text_note}
    %%                           , [{private_key, <<4:256>>}]),
    ok.
