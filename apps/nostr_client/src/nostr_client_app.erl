%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(nostr_client_app).
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
    pg:start(nostr_client),
    nostr_client_sup:start_link([]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> Return when
      State :: any(),
      Return :: ok.
stop(_State) ->
    ok.
