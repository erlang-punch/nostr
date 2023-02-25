%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib_relay).
-export([notice/1]).
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc notice/1 function returns a notification message to the
%%      client.
%% see https://github.com/nostr-protocol/nips/blob/master/01.md
%% @end
%%--------------------------------------------------------------------
-spec notice(Message) -> Return when
      Message :: iodata(),
      Return :: any().

notice(Message)
  when is_bitstring(Message) ->
    [<<"NOTICE">>, Message].
