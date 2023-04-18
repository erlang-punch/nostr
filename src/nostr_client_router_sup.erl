%%%===================================================================
%%% @doc `nostr_client_router_sup' monitors the `nostr_client_router'
%%% pool of processes.
%%%
%%% == Examples ==
%%%
%%% ```
%%% Host = "relay.nostrich.de".
%%% nostr_client_router_sup:start_link([{host, Host}]).
%%% pg:get_members(client, {Host, router}).
%%% '''
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client_router_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_router/2, spec_router/1]).

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
    State = {supervisor(), children(Args)},
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
supervisor() ->
    #{ strategy => one_for_one }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
children(Args) ->
    [ spec_router(Args) || _ <- lists:seq(0, 5)].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_router(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_router(Pid, Args) ->
    supervisor:start_child(Pid, spec_router(Args)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec spec_router(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
spec_router(Args) ->
    Start = {nostr_client_router, start_link, [Args]},
    #{ id => {nostr_client_router, erlang:unique_integer()}
     , start => Start
     , type => worker
     }.
