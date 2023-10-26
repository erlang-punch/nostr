%%%===================================================================
%%% @doc `nostr_sup' is the top level supervisor of the `nostr'
%%% application. It supervises the critical part of the
%%% infrastructure.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_sup).
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
      ChildSpec :: supervisor:child_spec().
init(_Args) ->
    State = {supervisor(), children_client()},
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
supervisor() ->
    #{ strategy => one_for_one
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
children_client() ->
    [nostr_manager_sup()
    ,nostr_manager()
    ,spec_pg(client)
    ,spec_pg(relay)
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
nostr_manager_sup() ->
    #{ id => nostr_manager_sup
     , start => {nostr_manager_sup, start_link, []}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
nostr_manager() ->
    #{ id => nostr_manager
     , start => {nostr_manager, start_link, []}
     , type => worker
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
spec_pg(Scope) ->
    #{ id => {nostr_pg, Scope}
     , start => {pg, start_link, [Scope]}
     , type => worker
     }.
