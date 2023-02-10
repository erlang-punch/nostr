%%%===================================================================
%%% @doc `nostr_manager_relay_sup' creates new group of relay
%%% processes to manage a `nostr' server.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_manager_relay_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link() -> Return when
      Return :: supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok,{SupFlags,[ChildSpec]}} | ignore,
      SupFlags :: supervisor:sup_flags(),
      ChildSpec :: [supervisor:child_spec(), ...].
init([]) ->
    State = {supervisor(), children()},
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec supervisor() -> Return when
      Return :: supervisor:sup_flags().
supervisor() ->
    #{ strategy => one_for_one
     , intensity => 0
     , period => 1
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec children() -> Return when
      Return :: [supervisor:child_spec(), ...].
children() ->
    [].
