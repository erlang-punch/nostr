%%%====================================================================
%%% @doc
%%%
%%% @end
%%%====================================================================
-module(nostrlib_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([common/0, common/1]).
-export([valid_json/0, valid_json/1]).
-export([encode_event/0, encode_event/1]).
-export([encode_request/0, encode_request/1]).
-export([encode_close/0, encode_close/1]).
-export([encode_eose/0, encode_eose/1]).
-export([encode_subscription/0, encode_subscription/1]).
-export([encode_notice/0, encode_notice/1]).
-export([decode_event/0, decode_event/1]).
-export([decode_close/0, decode_close/1]).
-export([decode_notice/0, decode_notice/1]).
-export([decode_request/0, decode_request/1]).
-export([decode_subscription/0, decode_subscription/1]).
-export([decode_eose/0, decode_eose/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

-spec suite() -> any().
-spec init_per_suite(any()) -> any().
-spec end_per_suite(any()) -> any().
-spec init_per_group(any(), any()) -> any().
-spec end_per_group(any(), any()) -> any().
-spec init_per_testcase(any(), any()) -> any().
-spec end_per_testcase(any(), any()) -> any().
-spec groups() -> any().
-spec all() -> any().

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_per_suite(_Config) -> [].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
end_per_suite(_Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->  Config.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) -> Config.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
groups() -> [{encode, [parallel], [encode_event
                                  ,encode_request
                                  ,encode_close
                                  ,encode_eose
                                  ,encode_subscription
                                  ,encode_notice
                                  ]}
            ,{decode, [parallel], [decode_event
                                  ,decode_close
                                  ,decode_notice
                                  ,decode_request
                                  ,decode_subscription
                                  ,decode_eose
                                  ]}
            ,{common, [parallel], [common
                                  ,valid_json]}
            ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
all() -> [{group, encode, [parallel]}
         ,{group, decode, [parallel]}
         ,{group, common, [parallel]}
         ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec common() -> Return when
      Return :: any().
common() -> [].

-spec common(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
common(_Config) ->
    <<_:256/bitstring>> = nostrlib:new_subscription_id(),

    {error, [{encode, unsupported}]}
        = nostrlib:encode(#{}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec valid_json() -> Return when
      Return :: any().
valid_json() -> [].

-spec valid_json(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
valid_json(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Files = [ "valid_event_request.json"
            , "valid_event_kind0.json"
            , "valid_event_kind1.json"
            , "valid_event_kind2.json"
            , "valid_event_kind7.json"
            ],
    Test = fun Test([]) -> ok;
               Test([File|Rest]) ->
                   FullPath = filename:join(DataDir, File),
                   {ok, Json} = file:read_file(FullPath),
                   {ok, Decoded, Labels} = nostrlib:decode(Json),
                   ct:pal(info, "decoded ~p (~p): ~n~p", [File, Labels, Decoded]),
                   {ok, Encoded} = nostrlib:encode(Decoded),
                   ct:pal(info, "encoded ~p: ~n~p", [File, Encoded]),
                   Test(Rest)
           end,
    Test(Files),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_request() -> Return when
      Return :: any().
encode_request() -> [].

-spec encode_request(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
encode_request(_Config) ->
    PrivateKey = <<1:256>>,
    Opts = [{private_key, PrivateKey}],
    
    % a subcription_id must be defined
    {error, [{subscription_id, undefined}]}
        = nostrlib:encode(#request{},Opts),

    % a subscription id can't be an atom
    {error, [{subscription_id, test}]}
        = nostrlib:encode(#request{subscription_id = test}, Opts),

    % a filter can't be null
    {error, [{filter, []}]}
        = nostrlib:encode(#request{subscription_id = <<"1234">>, filter = []}, Opts),

    % a filter can't be null
    {ok,<<"[\"REQ\",\"1234\",{}]">>}
        = nostrlib:encode(#request{subscription_id = <<"1234">>}, Opts),

    % limit can't be negative
    {error,[{limit,-10}]}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ limit = -10 }}),
    
    % a limit is a positive integer
    {ok,<<"[\"REQ\",\"1234\",{\"limit\":10}]">>}
        = nostrlib:encode(#request{subscription_id = <<"1234">>, filter = #filter{ limit = 10 }}, Opts),

    % since can be an integer
    {ok,<<"[\"REQ\",\"test\",{\"since\":1577836800}]">>}
        =  nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ since = 1577836800 }}),

    % since can also be an universal date
    {ok,<<"[\"REQ\",\"test\",{\"since\":1577836800}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ since = {{2020,1,1},{0,0,0} }}}),
    
    % until can be an integer
    {ok,<<"[\"REQ\",\"test\",{\"until\":1577836800}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ until = 1577836800 }}),

    % until can also be an universal date
    {ok,<<"[\"REQ\",\"test\",{\"until\":1577836800}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ until = {{2020,1,1},{0,0,0} }}}),

    % an author can be a full id
    {ok,<<"[\"REQ\",\"test\",{\"authors\":[\"0000000000000000000000000000000000000000000000000000000000000001\"]}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ authors = [<<1:256>>] }}),

    % an author can be a prefix
    {ok,<<"[\"REQ\",\"test\",{\"authors\":[\"0000000000000001\"]}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ authors = [<<1:64>>] }}),

    % a prefix can't be smaller than 32bits
    {error,[{prefix,<<0,0,1>>}]}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ authors = [<<1:24>>] }}),

    % an event id can be a prefix
    {ok,<<"[\"REQ\",\"test\",{\"ids\":[\"0000000000000001\"]}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ event_ids = [<<1:64>>] }}),

    % an event id in a tag can't be a prefix
    {error,[{content,[<<0,0,0,0,0,0,0,1>>]}]}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ tag_event_ids = [<<1:64>>] }}),

    % an event id in a tag is a full address
    {ok,<<"[\"REQ\",\"test\",{\"#e\":[\"0000000000000000000000000000000000000000000000000000000000000001\"]}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ tag_event_ids = [<<1:256>>] }}),
    
    % a public key in a tag can't be a prefix
    {error,[{content,[<<0,0,0,0,0,0,0,1>>]}]}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ tag_public_keys = [<<1:64>>] }}),

    % a public key in a tag must be complete
    {ok,<<"[\"REQ\",\"test\",{\"#p\":[\"0000000000000000000000000000000000000000000000000000000000000001\"]}]">>}
        = nostrlib:encode(#request{ subscription_id = <<"test">>, filter = #filter{ tag_public_keys = [<<1:256>>] }}).


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_subscription() -> Return when
      Return :: any().
encode_subscription() -> [].

-spec encode_subscription(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
encode_subscription(_Config) ->
    PrivateKey = <<1:256>>,
    Opts = [{private_key, PrivateKey}],

    {error,[{id,undefined}]}
        = nostrlib:encode(#subscription{}),

    {error,[{content,undefined}]}
        = nostrlib:encode(#subscription{ id = <<"test">> }),

    {error,[{content,undefined}]}
        =  nostrlib:encode(#subscription{ id = <<"test">>, content = <<>> }),

    {error,[{content,[]}]}
        = nostrlib:encode(#subscription{ id = <<"test">>, content = [] }),


    Event = <<"{\"content\":\"hello\",\"created_at\":1577836800,\"id\":\"1c00f47c449d3d1da178a8287aa06ecf5664af22d1c2dad1f1ecdaac664a7b27\",\"kind\":0,\"pubkey\":\"79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798\",\"sig\":\"47ae0058e481c23aa34bf36dde38cf012f42df75febcd66aa30716b8e1e8fbdf7bd731cfd55680898b82f9ea5c4334b66590fb566db70f8c498daf5c9fc16b30\",\"tags\":[]}">>,
    Subscription = <<"[\"EVENT\",\"test\"," , Event/bitstring , "]">>,

    {ok, Subscription}
        = nostrlib:encode(#subscription{ id = <<"test">>
                                       , content = #event{ content = <<"hello">>
                                                         , kind = set_metadata
                                                         , created_at = {{2020,1,1},{0,0,0}} 
                                                         }
                                       }, Opts),
    
    {ok, Subscription}
        = nostrlib:encode(#subscription{ id = <<"test">>
                                       , content = Event }, Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_notice() -> Return when
      Return :: any().
encode_notice() -> [].

-spec encode_notice(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
encode_notice(_Config) ->
    % a message must be defined
    {error, [{message, undefined}]}
        = nostrlib:encode(#notice{}),
    
    % a message can't be an atom
    {error,[{message,test}]}
        = nostrlib:encode(#notice{ message = test }),

    % a message must be a bitstring
    {ok,<<"[\"NOTICE\",\"test\"]">>}
        = nostrlib:encode(#notice{ message = <<"test">> }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_event() -> Return when
      Return :: any().
encode_event() -> [].

-spec encode_event(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
encode_event(_Config) ->
    PrivateKey = <<1:256>>,
    Opts = [{private_key, PrivateKey}],

    {error,[{content,undefined}]}
        = nostrlib:encode(#event{}),

    {error,[{kind,undefined}]}
        = nostrlib:encode(#event{content = <<>>}),

    {error,[{kind,[]}]}
        = nostrlib:encode(#event{kind = [], content = <<>>}),

    {error,[{public_key,undefined},{private_key,undefined}]}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>}),

    {error,[{public_key,<<>>}]}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>, public_key = <<>> }),

    {error,[{private_key, <<1:255>>}]}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>}
                         ,[{private_key, <<1:255>>}]),

    {ok, <<_/bitstring>>}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>}, Opts),

    {error, [{created_at,{}}]}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>, public_key = <<1:256>>
                                , created_at = {}}
                         ,Opts),

    {error,[{signature,<<>>}]}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>, public_key = <<1:256>>
                                , signature = <<>> }
                         ,Opts),

    {error,[{event_id,[]}]}
        = nostrlib:encode(#event{kind = set_metadata, content = <<>>, public_key = <<1:256>>
                                , id = []}
                         ,Opts),    
    
    % encore a metadata event
    Event0 = #event{ kind = set_metadata
                   , content = thoas:encode(#{ about => <<"test">> })
                   },
    {ok, R0} = nostrlib:encode(Event0, Opts),
    {ok, [<<"EVENT">>, #{<<"kind">> := 0}]} = thoas:decode(R0),

    % encode a test note event
    Event1 = #event{ kind = text_note
                   , content = <<"this is a note">>
                   },
    {ok, R1} = nostrlib:encode(Event1, Opts),
    {ok, [<<"EVENT">>, #{<<"kind">> := 1}]} = thoas:decode(R1),

    % encode a recommend server event
    Event2 = #event{ kind = recommend_server
                   , content = <<"wss://myserver.local">>
                   },
    {ok, R2} = nostrlib:encode(Event2, Opts),
    {ok, [<<"EVENT">>, #{<<"kind">> := 2}]} = thoas:decode(R2).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_close() -> Return when
      Return :: any().
encode_close() -> [].

-spec encode_close(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
encode_close(_Config) ->
    Close0 = #close{},
    {error, [{subscription_id, undefined}]} = nostrlib:encode(Close0),

    Close1 = #close{ subscription_id = <<"test">> },
    {ok, C1} = nostrlib:encode(Close1),
    {ok, [<<"CLOSE">>, <<"test">>]} = thoas:decode(C1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode_eose() -> Return when
      Return :: any().
encode_eose() -> [].

-spec encode_eose(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
encode_eose(_Config) ->
    Eose0 = #eose{},
    {error, [{id, undefined}]} = nostrlib:encode(Eose0),
    Eose1 = #eose{ id = <<"test">> },
    {ok, E1} = nostrlib:encode(Eose1),
    {ok, [<<"EOSE">>, <<"test">>]} = thoas:decode(E1).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_event() -> Return when
      Return :: any().
decode_event() -> [].

-spec decode_event(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
decode_event(_Config) ->
    {error,[{event,{missing,id}}]}
        = nostrlib:decode(thoas:encode([<<"EVENT">>, #{}])),
    
    {error,[{event,{bad,id}},{labels,[]}]}
        = nostrlib:decode(thoas:encode([<<"EVENT">>, #{ id => <<"test">> }])).
    
    
%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_close() -> Return when
      Return :: any().
decode_close() -> [].

-spec decode_close(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
decode_close(_Config) ->
    {error,[{subscription_id,<<>>}]}
        = nostrlib:decode(thoas:encode([<<"CLOSE">>, <<>> ])),

    {error,[{subscription_id,1}]}
        = nostrlib:decode(thoas:encode([<<"CLOSE">>, 1])),

    {ok,#close{subscription_id = <<"test">>},[]}
        = nostrlib:decode(thoas:encode([<<"CLOSE">>, <<"test">>])).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_notice() -> Return when
      Return :: any().
decode_notice() -> [].

-spec decode_notice(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
decode_notice(_Config) ->
    {ok,#notice{message = <<>>},[]}
        =  nostrlib:decode(thoas:encode([<<"NOTICE">>, <<>>])),
    
    {ok,#notice{message = <<"test">>},[]}
        = nostrlib:decode(thoas:encode([<<"NOTICE">>, <<"test">>])).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_request() -> Return when
      Return :: any().
decode_request() -> [].

-spec decode_request(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
decode_request(_Config) ->
    {ok, #request{filter = #filter{kinds = [set_metadata]}}, _}
        = nostrlib:decode(thoas:encode([<<"REQ">>, <<"test">>, #{ kinds => [0] } ])),

    {ok, #request{filter = #filter{tag_public_keys = [<<1:256>>]}}, _}
        = nostrlib:decode(thoas:encode([<<"REQ">>, <<"test">>, #{ <<"#p">> => [nostrlib:binary_to_hex(<<1:256>>)] } ])),

    {ok, #request{filter = #filter{tag_event_ids = [<<1:256>>]}}, _}
        = nostrlib:decode(thoas:encode([<<"REQ">>, <<"test">>, #{ <<"#e">> => [nostrlib:binary_to_hex(<<1:256>>)] } ])),

    {ok, #request{filter = #filter{ event_ids = [<<1:256>>]
                                  }}, _}
        = nostrlib:decode(thoas:encode([<<"REQ">>, <<"test">>, #{ <<"ids">> => [nostrlib:binary_to_hex(<<1:256>>)] } ])),

    {ok, #request{filter = #filter{ authors = [<<1:256>>]
                                  }}, _}
        = nostrlib:decode(thoas:encode([<<"REQ">>, <<"test">>, #{ <<"authors">> => [nostrlib:binary_to_hex(<<1:256>>)] } ])),

    {ok,#request{subscription_id = <<"test">>,
             filter = #filter{ event_ids = undefined
                             , authors = undefined
                             , kinds = undefined
                             , tag_event_ids = undefined
                             , tag_public_keys = undefined
                             , since = undefined
                             , until = undefined,limit = undefined 
                             }}, []}
        = nostrlib:decode(thoas:encode([<<"REQ">>, <<"test">>, #{}])).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_subscription() -> Return when
      Return :: any().
decode_subscription() -> [].

-spec decode_subscription(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
decode_subscription(_Config) ->
    Event =  #event{ kind = set_metadata, content = <<"test">>},
    Subscription = #subscription{ id = <<"test">>, content = Event },
    {ok, EncodedEvent} = nostrlib:encode(Subscription, [{private_key, <<1:256>>}]),

    {error,{unsupported,[<<"EVENT">>,<<"123123123123">>,<<>>]}}
        = nostrlib:decode(thoas:encode([<<"EVENT">>, <<"123123123123">>, <<>>])),

    {error,[{event,{missing,id}}]}
        = nostrlib:decode(thoas:encode([<<"EVENT">>, <<"123123123123">>, #{}])),

    {ok, _Message, _Labels} = nostrlib:decode(EncodedEvent).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec decode_eose() -> Return when
      Return :: any().
decode_eose() -> [].

-spec decode_eose(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
decode_eose(_Config) ->
    {error,{unsupported,[<<"EOSE">>]}}
        = nostrlib:decode(thoas:encode([<<"EOSE">>])),

    {ok,#eose{id = <<"test">>},[]}
        = nostrlib:decode(thoas:encode([<<"EOSE">>, <<"test">>])).
