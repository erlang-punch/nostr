%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib_tags).
-export([e/2, event/2]).
-export([p/2, public_key/2]).
-include_lib("eunit/include/eunit.hrl").
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% extra-specification for eunit.
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc an alias for event.
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#other-notes
%% @end
%%--------------------------------------------------------------------
-spec e(EventId, RecommendedRelayUrl) -> Return when
      EventId :: iodata(),
      RecommendedRelayUrl :: iodata(),
      Return :: any().

e(EventId, RecommendedRelayUrl) ->
    event(EventId, RecommendedRelayUrl).

%%--------------------------------------------------------------------
%% @doc
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#other-notes
%% @end
%%--------------------------------------------------------------------
-spec event(EventId, RecommendedRelayUrl) -> Return when
      EventId :: iodata(),
      RecommendedRelayUrl :: iodata(),
      Return :: any().

event(EventId, RecommendedRelayUrl) ->
    ["e", EventId, RecommendedRelayUrl].

%%--------------------------------------------------------------------
%% @doc An alias for publickey
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#other-notes
%% @end
%%--------------------------------------------------------------------
-spec p(PublicKey, RecommendedRelayUrl) -> Return when
      PublicKey :: iodata(),
      RecommendedRelayUrl :: iodata(),
      Return :: any().

p(PublicKey, RecommendedRelayUrl) ->
    public_key(PublicKey, RecommendedRelayUrl).

%%--------------------------------------------------------------------
%% @doc
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#other-notes
%% @end
%%--------------------------------------------------------------------
-spec public_key(PublicKey, RecommendedRelayUrl) -> Return when
      PublicKey :: iodata(),
      RecommendedRelayUrl :: iodata(),
      Return :: any().

public_key(PublicKey, RecommendedRelayUrl) ->
    ["p", PublicKey, RecommendedRelayUrl].
