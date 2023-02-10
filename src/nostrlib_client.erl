%%%===================================================================
%%% @doc `nostrlib' contains all functions commonly used by relays and
%%% clients. This is a low level interface and should not be directly
%%% used.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib_client).
-export([event/1]).
-export([request/2]).
-export([close/1]).
-export([create_subscription_id/0]).
-export([create_event_id/1]).
-include_lib("eunit/include/eunit.hrl").
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% extra specifications for eunit.
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc request/2 is a low level interface to generate a request
%%      defined in nip/01.
%% @end
%% @TODO try to find a better solution to encode these values.
%%--------------------------------------------------------------------
-spec request(SubscriptionId, Filters) -> Return when
      SubscriptionId :: subscription_id(),
      Filters :: filters(),
      Return :: to_be_defined().
request(SubscriptionId, Filter)
  when is_map(Filter) ->
    [<<"REQ">>, SubscriptionId, filter(Filter)];
request(SubscriptionId, Filters)
  when is_list(Filters) ->
    [<<"REQ">>, SubscriptionId, filters(Filters)].

-spec request_test() -> any().
request_test() ->
    In_001 = request(<<"721983390570381">>, #{
      kinds => [0,1,2,7],
      since => 1676057052,
      limit => 450
    }),
    Out_001 = [
      <<"REQ">>,
      <<"721983390570381">>,
      #{ kinds => [0,1,2,7], since => 1676057052, limit => 450}
    ],
    ?assertEqual(Out_001, In_001),

    % Check if the conversion from atom to integer is working
    In_002 = request(
      <<"721983390570381">>,
      #{ kinds => [nostrlib:kind(0)
                  ,nostrlib:kind(1)
                  ,nostrlib:kind(2)
                  ,nostrlib:kind(7)],
      since => 1676057052, limit => 450
    }),
    Out_002 = [
      <<"REQ">>,
      <<"721983390570381">>,
      #{ kinds => [0,1,2,7], since => 1676057052, limit => 450
    }],
    ?assertEqual(Out_002, In_002).

%%--------------------------------------------------------------------
%% @TODO this function is used internally and should not be exported
%%--------------------------------------------------------------------
filters(Filters) ->
    filters(Filters, []).

%%--------------------------------------------------------------------
%% @TODO this function is used internally and should not be exported
%%--------------------------------------------------------------------
filters([], Buffer) -> lists:reverse(Buffer);
filters([Filter|Rest], Buffer) ->
    filters(Rest, [filter(Filter)|Buffer]).

%%--------------------------------------------------------------------
%% @TODO this function is used internaly and should not be exported
%% @TODO Create a filter for ids
%% @TODO Create a filter for authors
%% @TODO Modify and update the kinds filter
%% @TODO Create a filter for #e
%% @TODO Create a filter for #p
%% @TODO Create a filter for since
%% @TODO Create a filter for until
%% @TODO Create a filter for limit
%%--------------------------------------------------------------------
filter(Filter) ->
    Kinds = maps:get(kinds, Filter, []),
    Filter#{ kinds => nostrlib:kinds(Kinds) }.

%%--------------------------------------------------------------------
%% @doc event/1 creates a new event like data-structure. An event is
%%      defined in NIP/01 and can have these fields defined:
%%
%% ```
%%     #{ id => Id :: iodata()
%%      , pubkey => Pubkey :: iodata()
%%      , created_at => UnixTimestamp :: pos_integer()
%%      , kinds => Kinds :: [pos_integer(), ...]
%%      , tags => Tags :: [list(), ...]
%%      , content => iodata()
%%      , sig => iodata()
%%      }
%% '''
%%
%% see https://github.com/nostr-protocol/nips/blob/master/01.md
%% @end
%% @TODO extend the features during the event creation.
%%--------------------------------------------------------------------
-spec event(Event) -> Return when
      Event :: event(),
      Return :: any().
event(Event)
  when is_map(Event) ->
    [<<"EVENT">>, Event].

%%--------------------------------------------------------------------
%% @doc close/1 stops a subscription based on provided subscription
%%      identifier. This is a low level interface.
%% @end
%%--------------------------------------------------------------------
-spec close(SubscriptionId) -> Return when
      SubscriptionId :: subscription_id(),
      Return :: any().
close(SubscriptionId)
  when is_bitstring(SubscriptionId) ->
    [<<"CLOSE">>, SubscriptionId].

%%--------------------------------------------------------------------
%% @doc create_subscription_id/0 generate a new subscription id based
%%      on crypto:strong_rand_bytes/1 function and returning a
%%      random integer as string.
%% @end
%% @TODO review the way to generate subscription id.
%%--------------------------------------------------------------------
-spec create_subscription_id() -> Return when
      Return :: pos_integer().
create_subscription_id() ->
    <<SubscriptionIdRaw:64/integer>> = crypto:strong_rand_bytes(8),
    erlang:integer_to_binary(SubscriptionIdRaw).

%%--------------------------------------------------------------------
%% @doc create_event_id/1 generates a compatible checksum based on
%%      the given event.
%%
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#basic-event-kinds
%% @end
%%--------------------------------------------------------------------
-spec create_event_id(Event) -> Return when
      Event :: event(),
      Return :: iodata().
create_event_id(Event) ->
    SerializedEvent = serialize_event(Event),
    SerializedJson = thoas:encode(SerializedEvent),
    BinaryHash = crypto:hash(sha256, SerializedJson),
    UppercaseHash = << <<(erlang:integer_to_binary(X, 16))/bitstring>>
                       || <<X:4>> <= BinaryHash >>,
    string:lowercase(UppercaseHash).

-spec create_event_id_test() -> any().
create_event_id_test() ->
    % a random event from an open relay
    Event = #{ <<"content">> => <<240,159,164,153>>
             , <<"created_at">> => 1676749221
             , <<"id">> => <<"5b5479e7adc2a7902572c2ee5325c2db6c31097fa7f4b86bb7e586d3ee7249ea">>
             , <<"kind">> => 7
             , <<"pubkey">> => <<"52e98835d909f73315eb391faa203506aa30bc533290a937a0c84db3eba16573">>
             , <<"sig">> => <<"1e7e9802c604482e0d9b076bc6d89d37135ac00bbffce4a0bd9162f8d52569ed71db43c6a0c435005b2087fec8af260b86d30437a867b1984bc8cab99e607b30">>
             , <<"tags">> =>[[<<"p">>,<<"7b3f7803750746f455413a221f80965eecb69ef308f2ead1da89cc2c8912e968">>,<<"wss://relay.damus.io">>]
                            ,[<<"e">>,<<"b221a746d78058a7a3403bba4b0b123d36c827635e3eba0dcf8f564e9fc013d4">>,<<"wss://nostr-pub.wellorder.net">>,<<"root">>]
                            ,[<<"e">>,<<"d8ca85941e1aab6a475736f20ef6c7b62c77477899594b5d2ac011eba5282954">>]
                            ,[<<"p">>,<<"7ecd3fe6353ec4c53672793e81445c2a319ccf0a298a91d77adcfa386b52f30d">>]]
             },
    #{ <<"id">> := Id } = Event,
    ?assertEqual(Id, create_event_id(Event)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
serialize_event(#{ <<"pubkey">> := PublicKey
                 , <<"created_at">> := CreatedAt
                 , <<"kind">> := Kind
                 , <<"tags">> := Tags
                 , <<"content">> := Content
                 } = _Event) ->
    [0 ,PublicKey,CreatedAt,Kind,Tags,Content].
