%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib_kind).
-export([metadata/3, text_note/1]).
-export([recommend_server/1, recommend_server/2]).
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#basic-event-kinds
%% @end
%%--------------------------------------------------------------------
-spec metadata(Username, About, Picture) -> Return when
      Username :: to_be_defined(),
      About :: to_be_defined(),
      Picture :: to_be_defined(),
      Return :: map().
metadata(Username, About, Picture) ->
    #{ name => Username
     , about => About
     , picture => Picture
     }.

%%--------------------------------------------------------------------
%% @doc
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#basic-event-kinds
%% @end
%%--------------------------------------------------------------------
-spec text_note(Content) -> Return when
      Content :: to_be_defined(),
      Return :: map().
text_note(Content) ->
    text_note(#{}, Content).

%%--------------------------------------------------------------------
%% @doc
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#basic-event-kinds
%% @end
%%--------------------------------------------------------------------
-spec text_note(Map, Content) -> Return when
      Map :: map(),
      Content :: to_be_defined(),
      Return :: map().
text_note(Map, Content) ->
    maps:put(content, Content, Map).

%%--------------------------------------------------------------------
%% @doc
%% see https://github.com/nostr-protocol/nips/blob/master/01.md#basic-event-kinds
%% @end
%%--------------------------------------------------------------------
-spec recommend_server(ServerUrl) -> Return when
      ServerUrl :: to_be_defined(),
      Return :: map().
recommend_server(ServerUrl) ->
    recommend_server(#{}, ServerUrl).

-spec recommend_server(Map, ServerUrl) -> Return when
      Map :: map(),
      ServerUrl :: to_be_defined(),
      Return :: map().
recommend_server(Map, ServerUrl) ->
    text_note(Map, ServerUrl).
