%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%% @end
%%%===================================================================
-module(nostr_relay_app).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(Type, Args) -> Return when
      Type :: any(),
      Args :: any(),
      Return :: supervisor:startlink_ret().

start(_StartType, _StartArgs) ->
    % @todo to modify. the current structure is not adapted
    % for an erlang release.
    nostr_relay_sup:start_link([]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> Return when
      State :: any(),
      Return :: ok.

stop(_State) ->
    ok.
