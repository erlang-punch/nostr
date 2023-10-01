%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc THIS FILE IS A DRAFT.
%%%
%%% To start this relay, you can use the following functions:
%%%
%%% ```
%%% nostr_relay:start([{port, 4000}]).
%%% '''
%%%
%%% By default this module will already listen to port TCP/4000.
%%%
%%% ```
%%% nostr_relay:start().
%%% '''
%%%
%%% @todo create a start/1 function to start a new relay
%%% @end
%%%===================================================================
-module(nostr_relay).
-export([start/0, start/1]).
-export([stop/1]).
-export([get_process/2, get_processes/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostr/include/nostrlib.hrl").

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

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_processes(Name, Args) -> Return when
      Name :: atom(),
      Args :: proplists:proplists(),
      Return :: [pid(), ...].
get_processes(Name, Args) ->
    Port = proplists:get_value(port, Args, 4000),
    Domain = proplists:get_value(domain, Args, '_'),
    pg:get_members(relay, {Name, Port, Domain}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_process(Name, Args) -> Return when
      Name :: atom(),
      Args :: proplists:proplists(),
      Return :: {ok, pid()} | {error, Reason},
      Reason :: any().
get_process(Name, Args) ->
    case get_processes(Name, Args) of
        [] -> {error, not_started};
        [P|_] -> {ok, P}
    end.
