%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client_subscription_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([spec_subscription/1, create_subscription/2, terminate_subscription/2]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startlink_ret().
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok,{SupFlags,[ChildSpec]}} | ignore,
      SupFlags :: supervisor:sup_flags(),
      ChildSpec :: [supervisor:child_spec(), ...].
init(_Args) ->
    State = {supervisor(), children()},
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec supervisor() -> Return when
      Return :: supervisor:sup_flags().
supervisor() ->
    #{ strategy => one_for_one
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec children() -> Return when
      Return :: [supervisor:child_spec(), ...].
children() ->
    [].

%%--------------------------------------------------------------------
%% @TODO: many subscriptions can be created, not only one. Need to
%%        add a multi-id support.
%%--------------------------------------------------------------------
-spec spec_subscription(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_subscription(Args) ->
    #{ id => {nostr_client_subscription, erlang:unique_integer()}
     , start => {nostr_client_subscription, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec create_subscription(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
create_subscription(Pid, Args) ->
    Spec = spec_subscription(Args),
    supervisor:start_child(Pid, Spec).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate_subscription(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: ok | {error, any()}.
terminate_subscription(Pid, Id) ->
    Ref = {nostr_client_subscription, Id},
    supervisor:terminate_child(Pid, Ref).
