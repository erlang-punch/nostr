%%%===================================================================
%%% @doc DRAFT: `nostrlib_decoder' module offers a way to decode the message
%%% coming from the connection and used in the router. The generated
%%% Erlang term is a record (or a tuple if used in other language).
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib_decoder).
-export([decode/1]).
-include_lib("eunit/include/eunit.hrl").
-include("nostrlib_decoder.hrl").

%%--------------------------------------------------------------------
%% spec used for eunit
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc `decode/1' decodes a raw message coming from a connection,
%% parse it with thoas and then convert it to a record defined in
%% `nostrlib_decoder.hrl' file.
%%
%% @todo creates more test
%% @todo creates examples
%% @see thoas:decode/1
%% @end
%%--------------------------------------------------------------------
-spec decode(Bitstring) -> Return when
      Bitstring :: bitstring(),
      Return :: decoded_messages().
decode(Bitstring) ->
    case thoas:decode(Bitstring) of
        {ok, Json} -> 
            decode_message(Json);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% Random messages from open relay
%%--------------------------------------------------------------------
-spec decode_test() -> any().
decode_test() ->
    % decode a valid json message of kind 1
    IN_001 = <<"[\"EVENT\",\"8034879165223001\",{\"id\":\"2f0e96269",
               "b7ece13f63c39a26fd0b3ee1e6a41afd4f8aab0cb8afdcb9ab3",
               "a64e\",\"pubkey\":\"a3eb29554bd27fca7f53f66272e4bb5",
               "9d066f2f31708cf341540cb4729fbd841\",\"created_at\":",
               "1677096834,\"kind\":1,\"tags\":[[\"e\",\"5aec5f6b41",
               "844cd9fd7635f98fcd5ac814ac1618fc8a896d78011973ebc17",
               "6ec\"],[\"p\",\"e623bb2e90351b30818de33debd506aa9ea",
               "e04d8268be65ceb2dcc1ef6881765\"],[\"p\",\"472f440f2",
               "9ef996e92a186b8d320ff180c855903882e59d50de1b8bd5669",
               "301e\"]],\"content\":\"#[2] has convinced me\",\"si",
               "g\":\"430506421243e3cf0e737efd22101d38765ddd5235a9d",
               "47602adf0c5a4dbe63a9c9d6e267375bfcfb4d95d1558135bd9",
               "2ad26b1b86437ff27c63ff713d2825e3\"}]">>,
    OUT_001 = {ok, #subscription{ id = <<"8034879165223001">>
                                , content = #event{ id = <<"2f0e96269b7ece13f63c39a26fd0b3ee1e6a41afd4f8aab0cb8afdcb9ab3a64e">>
                                             , public_key = <<"a3eb29554bd27fca7f53f66272e4bb59d066f2f31708cf341540cb4729fbd841">>
                                             , created_at = 1677096834
                                             , kind = 1
                                             , tags = [[<<"e">>,<<"5aec5f6b41844cd9fd7635f98fcd5ac814ac1618fc8a896d78011973ebc176ec">>]
                                                      ,[<<"p">>,<<"e623bb2e90351b30818de33debd506aa9eae04d8268be65ceb2dcc1ef6881765">>]
                                                      ,[<<"p">>,<<"472f440f29ef996e92a186b8d320ff180c855903882e59d50de1b8bd5669301e">>]
                                                      ]
                                             , content = <<"#[2] has convinced me">>
                                             , signature = <<"430506421243e3cf0e737e"
                                                            ,"fd22101d38765ddd5235a9"
                                                            ,"d47602adf0c5a4dbe63a9c"
                                                            ,"9d6e267375bfcfb4d95d15"
                                                            ,"58135bd92ad26b1b86437f"
                                                            ,"f27c63ff713d2825e3">>}
                           }
              },
    ?assertEqual(OUT_001, decode(IN_001)),

    % decode an unsupported but valid json message
    IN_002 = <<"[]">>,
    OUT_002 = {error, {unsupported, []}},
    ?assertEqual(OUT_002, decode(IN_002)),
    
    % decode another valid json with utf8 symbol.
    IN_003 = <<91,34,69,86,69,78,84,34,44,34,49,53,52,56,48,50,
               48,48,48,57,54,57,53,48,56,51,49,51,53,34,44,123,
               34,105,100,34,58,34,48,52,56,53,98,56,48,97,100,
               98,54,97,102,49,48,100,51,53,56,48,99,52,52,57,97,
               57,98,49,101,50,102,54,57,54,55,55,102,49,52,101,
               54,49,98,49,54,100,102,97,53,100,101,51,51,98,49,
               99,54,54,56,101,102,53,102,97,34,44,34,112,117,98,
               107,101,121,34,58,34,56,97,57,56,49,102,49,97,101,
               51,102,97,98,51,51,48,48,98,53,52,56,99,52,102,50,
               48,54,53,52,99,98,48,102,49,100,51,53,48,52,57,56,
               99,52,98,54,54,56,52,57,98,55,51,101,56,53,52,54,
               48,48,49,100,99,97,48,34,44,34,99,114,101,97,116,
               101,100,95,97,116,34,58,49,54,55,55,51,48,53,56,
               54,49,44,34,107,105,110,100,34,58,49,44,34,116,97,
               103,115,34,58,91,91,34,101,34,44,34,102,57,55,97,
               100,56,54,48,49,51,53,102,102,52,49,50,53,49,51,
               52,98,57,48,52,100,57,49,102,57,53,52,98,55,49,54,
               100,54,50,52,50,51,49,57,54,57,101,50,48,49,97,97,
               49,54,99,99,50,55,54,55,100,55,49,97,57,34,93,44,
               91,34,112,34,44,34,100,101,57,48,99,53,100,98,51,
               54,97,52,48,49,49,102,57,100,53,56,52,100,102,99,
               49,56,100,101,49,97,53,55,50,52,54,56,54,56,54,55,
               57,56,52,55,57,51,101,102,53,50,54,51,51,49,98,53,
               49,102,56,98,52,51,101,57,34,93,93,44,34,99,111,
               110,116,101,110,116,34,58,34,68,97,109,110,32,119,
               101,108,108,44,32,73,32,99,97,110,226,128,153,116,
               32,101,118,101,110,32,99,111,117,110,116,32,116,
               104,97,116,33,34,44,34,115,105,103,34,58,34,52,54,
               52,49,50,102,54,49,99,100,100,49,48,54,102,102,98,
               51,52,101,57,101,55,49,52,99,57,56,99,48,54,100,
               97,55,55,51,51,98,100,51,100,53,55,98,101,100,53,
               98,53,55,100,101,49,97,55,57,97,53,55,50,98,102,
               99,99,101,53,50,98,55,97,102,52,50,97,97,50,54,
               100,53,101,48,50,56,48,52,97,97,53,54,56,102,53,
               55,54,99,49,97,99,56,54,53,100,55,98,53,57,102,97,
               54,51,48,52,100,55,97,53,55,99,101,99,99,57,98,99,
               53,57,49,99,34,125,93>>,
    OUT_003 = {ok,#subscription{ id = <<"1548020009695083135">>
                               , content = #event{ id = <<"0485b80adb6af10d3580c449a9b1e2f69677f14e61b16dfa5de33b1c668ef5fa">>
                                                 , public_key = <<"8a981f1ae3fab3300b548c4f20654cb0f1d350498c4b66849b73e8546001dca0">>
                                                 , created_at = 1677305861
                                                 , kind = 1
                                                 , tags = [[<<"e">>,<<"f97ad860135ff4125134b904d91f954b716d624231969e201aa16cc2767d71a9">>]
                                                          ,[<<"p">>,<<"de90c5db36a4011f9d584dfc18de1a5724686867984793ef526331b51f8b43e9">>]]
                                                 , content = <<68,97,109,110,32,119,101,108,108,44,32,73,32,99,97
                                                              ,110,226,128,153,116,32,101,118,101,110,32,99,111
                                                              ,117,110,116,32,116,104,97,116,33>>
                                                 , signature = <<"46412f61cdd106ffb34e9e714c98c06da"
                                                                ,"7733bd3d57bed5b57de1a79a572bfcce5"
                                                                ,"2b7af42aa26d5e02804aa568f576c1ac8"
                                                                ,"65d7b59fa6304d7a57cecc9bc591c">>
                                                 }
                               }
              },
    ?assertEqual(OUT_003, decode(IN_003)).

    % kind 0
    % <<"[\"EVENT\",\"17771556064953075220123\",{\"id\":\"433e78562c17101284e130026c0b4ac82a41d442576f92fae929b6fbe441cd3b\",\"pubkey\":\"02748827a1016a393c780aec1d96191a3b8df1c397d09351029cbb25b2d83443\",\"created_at\":1676898304,\"kind\":0,\"tags\":[],\"content\":\"{\\\"name\\\":\\\"berean\\\",\\\"nip05\\\":\\\"berean@nostrplebs.com\\\",\\\"picture\\\":\\\"https://nostr.build/i/p/nostr.build_b764966a4970638de60956883c63fc4e0a8d8bf1d7c54e0da5562f402e716c2c.jpg\\\",\\\"banner\\\":\\\"\\\",\\\"about\\\":\\\"Don't Trust. Verify.\\\\n#Bitcoin #Plebchain\\\",\\\"lud06\\\":\\\"\\\",\\\"lud16\\\":\\\"lastingbead65@walletofsatoshi.com\\\",\\\"username\\\":\\\"berean\\\",\\\"display_name\\\":\\\"berean\\\",\\\"displayName\\\":\\\"\\\",\\\"website\\\":\\\"\\\",\\\"Tags\\\":\\\"#bitcoin #nostr #plebchain\\\",\\\"nip05valid\\\":true,\\\"followingCount\\\":157,\\\"followersCount\\\":153}\",\"sig\":\"1252bfa38c4b93a9551ec04e141e57bbaf1b3696446790bb8c8af50d50f3e3c5f903926e9f4114abc9a3e7f44221d504864599447ffa6fb244e00f1929f951cc\"}]">>
    % <<"[\"EVENT\",\"17771556064953075220123\",{\"id\":\"5a18e9add8877b0df36bf1500a16bcacc0e446c7b330b52cda08ecb8f3021656\",\"pubkey\":\"9d96589eee0e57d07a5f1877285f42e8618e40ab2b94546a04dcad5eb8cbd0e8\",\"created_at\":1676898679,\"kind\":0,\"tags\":[],\"content\":\"{\\\"display_name\\\":\\\"\\\",\\\"website\\\":\\\"\\\",\\\"name\\\":\\\"machasm\\\",\\\"about\\\":\\\"Total Nostr Noob\\\",\\\"lud06\\\":\\\"lnbc10n1p3lyhprpp5fnftakvwgxp9vsgn2xdv47n276vksc564juld7umgn79fuzs9xgqdqqcqzpgxqyz5vqsp5c2vzcfgvax0tgzc5smg835g665k3px72yt44qtjfsafjcclkgzvq9qyyssqejmspclfnd8val39mnmy8grjn5hkpxemue4tw4kd94uky8cmk9j9re9fu8vtnv0mjeclatyvsdp7hkzu7vl8uml9rg6m4k866xtkawspx4nc93\\\",\\\"banner\\\":\\\"\\\",\\\"picture\\\":\\\"https://imgur.com/gallery/r6m07Dk\\\",\\\"nip05\\\":\\\"machasm@nostrplebs.com\\\",\\\"nip05_updated_at\\\":1676845714}\",\"sig\":\"8524a862fd059a6dc9d0199888f77b531487c9962e99e3753f9640da0c4680b4361db60d07aa44e03ed23ce71478366137c9b0c40748ed710c3929168637f66a\"}]">>

    % kind 2
    % <<"[\"EVENT\",\"17771556064953075220111\",{\"id\":\"379345c8843c72bbe3c4165df7cb4cf7d88e88964d7b2440a9024ce12ca7140d\",\"pubkey\":\"d0872ed8cd4ef83ab9fc56841dedaee15866aa80eb811959b294e627757a6819\",\"created_at\":1672028822,\"kind\":2,\"tags\":[],\"content\":\"wss://nostr.orangepill.dev\",\"sig\":\"d5b8e7fc9928ec6d40452f9f1be05aca6303f9da85ee128052ce77fc2f27b66629ce4ee5c93231b4f0946d1709f71d3d25c7236dfcbef0a5eb90e86db8683e9a\"}]">>
    % <<"[\"EVENT\",\"17771556064953075220111\",{\"id\":\"3cd9aae254c99e4e31ca4ddaa8cc85cb030dcebcd64088bd490d2118eb19aad7\",\"pubkey\":\"b8060b54a86d9a8fab04328ce134f0f2f20d2e2c67c128932d0bfb3732abf1f6\",\"created_at\":1672044455,\"kind\":2,\"tags\":[],\"content\":\"wss://nostr-pub.wellorder.net\",\"sig\":\"4900c0eef7040c63960a99d75c31d3473fe7afc687e6ad337483c7bc8dcbc1793867da316dfa94bd69fffe3c8ee3f27422f739af9383026ef6c8fa3bbe15c55b\"}]">>

    % kind 7
    % <<"[\"EVENT\",\"17771556064953075220\",{\"id\":\"6bef2129264acfe7fb43d33b418e40f69d511922c7203a1739fff933a80073fc\",\"pubkey\":\"a2d9f796461e3926e82d6ff02661be5fc57d3d6b3b31b6aaf76344db8280e331\",\"created_at\":1677314334,\"kind\":7,\"tags\":[[\"e\",\"7ed274a08644586b6bb369276d58197908dcdec0381d3dfac6328d336a62d69e\"],[\"p\",\"69074169ed68fa74c37d3926359f4100635c37eea5cfece064ed022ed06f792b\"]],\"content\":\"+\",\"sig\":\"bb9aa9df016baf7a8b7c7e302af3f4a9d716aeb734aee22d51e6cfb7df92d1ac2542aceb254533c958169e3f0622ce12b9d6257dac9dd42673b9ab0f21cc3706\"}]">>

%%--------------------------------------------------------------------
%% @doc internal function used to decode every element of a JSON
%% encoded message and convert it to record.
%%
%% @todo creates more test.
%% @end
%%--------------------------------------------------------------------
% decode an event message from client to relay
decode_message([<<"EVENT">>, Event]) ->
    decode_message_event(Event);

% decode an event message from relay to client
decode_message([<<"EVENT">>, SubscriptionId, Event]) ->
    {ok, ParsedSubscriptionId} = decode_message_subscription_id(SubscriptionId),
    {ok, ParsedEvent} = decode_message_event(Event),
    Return = #subscription{ id = ParsedSubscriptionId
                          , content = ParsedEvent
                          },
    {ok, Return};

% decode a subscription request
decode_message([<<"REQ">>, SubscriptionId, Filter]) ->
    {ok, ParsedSubscriptionId} = decode_message_subscription_id(SubscriptionId),
    {ok, ParsedFilter} = decode_message_filter(Filter),
    Return = #subscription{ id = ParsedSubscriptionId
                          , content = ParsedFilter
                          },
    {ok, Return};

% decode a end of subscription request
decode_message([<<"CLOSE">>, SubscriptionId]) ->
    {ok, ParsedSubscriptionId} = decode_message_subscription_id(SubscriptionId),
    Return = #close{ subscription_id = ParsedSubscriptionId },
    {ok, Return};

% decode a notice message
decode_message([<<"NOTICE">>, Notice]) ->
    {ok, ParsedNotice} = decode_message_notice(Notice),
    Return = #notice{ message = ParsedNotice },
    {ok, Return};

% decode an end of subscription message
decode_message([<<"EOSE">>, SubscriptionId]) ->
    {ok, ParsedSubscriptionId} = decode_message_subscription_id(SubscriptionId),
    Return = #eose{ id = ParsedSubscriptionId },
    {ok, Return};

decode_message(Message) ->
    {error, {unsupported, Message}}.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_message_event(#{ <<"id">>         := EventId
                      , <<"pubkey">>     := PublicKey
                      , <<"created_at">> := CreatedAt
                      , <<"kind">>       := Kind
                      , <<"tags">>       := Tags
                      , <<"content">>    := Content
                      , <<"sig">>        := Signature
                      }) ->
    Event = #event{ id = EventId
                  , created_at = CreatedAt
                  , public_key = PublicKey
                  , kind = Kind
                  , tags = Tags
                  , content = Content
                  , signature = Signature
                  },
    {ok, Event}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_message_subscription_id(SubscriptionId)
  when is_bitstring(SubscriptionId) ->
    {ok, SubscriptionId}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_message_notice(Notice)
  when is_bitstring(Notice) ->
    Notice = #notice{ message = Notice },
    {ok, Notice}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
decode_message_filter(#{ <<"ids">>     := EventIds
                       , <<"authors">> := Authors
                       , <<"kinds">>   := Kinds
                       , <<"#e">>      := TagEventIds
                       , <<"#p">>      := TagPublicKey
                       , <<"since">>   := Since
                       , <<"until">>   := Until
                       , <<"limit">>   := Limit
                       }) ->
    Filter = #filter{ event_ids       = EventIds
                    , authors         = Authors
                    , kinds           = Kinds
                    , tag_event_ids   = TagEventIds
                    , tag_public_keys = TagPublicKey
                    , since           = Since
                    , until           = Until
                    , limit           = Limit
                    },
    {ok, Filter}.
