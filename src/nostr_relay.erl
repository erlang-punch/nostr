%%%===================================================================
%%% @doc THIS FILE IS A DRAFT.
%%%
%%% @todo create a start/1 function to start a new relay
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_relay).
-export([start/0, start/1]).
-export([stop/1]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start() -> any().
start() ->
    start([{port, 4000}]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(any()) -> any().
start(Opts) ->
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    nostr_manager_relay_sup:start_relay_sup(Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(any()) -> any().
stop(Opts) ->
    Port = proplists:get_value(port, Opts),
    cowboy:stop_listener({nostr_relay_listener, Port}),
    supervisor:terminate_child(nostr_manager_relay_sup, {nostr_relay_sup,[{port,4000}]}),
    supervisor:delete_child(nostr_manager_relay_sup, {nostr_relay_sup,[{port,4000}]}).

