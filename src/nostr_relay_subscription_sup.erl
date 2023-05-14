%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc
%%%
%%% ```
%%% '''
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_subscription_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_relay_subscription/2, spec_relay_subscription/1]).
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startlink_ret().
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok,{SupFlags,[ChildSpec]}} | ignore,
      SupFlags :: supervisor:sup_flags(),
      ChildSpec :: supervisor:child_spec().
init(Args) ->
    Children = children(Args),
    State = {supervisor(), Children},
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
children(Args) ->
    [ spec_relay_subscription(Args) || _ <- lists:seq(0, 10)
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
supervisor() ->
    #{ strategy => one_for_one
     }.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_relay_subscription(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_relay_subscription(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    #{ id => {nostr_relay_subscription, Port, erlang:unique_integer()}
     , start => {nostr_relay_subscription, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_relay_subscription(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_relay_subscription(Pid, Args) ->
    supervisor:start_child(Pid, spec_relay_subscription(Args)).
