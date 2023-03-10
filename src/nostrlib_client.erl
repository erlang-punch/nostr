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

