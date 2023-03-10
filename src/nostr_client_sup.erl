%%%===================================================================
%%% @doc `nostr_client_sup' module is the top client process
%%% supervisor. All its children present in its supervision tree are
%%% representing a full nostr client.
%%%
%%% == Examples ==
%%%
%%% ```
%%% % start a new client from scratch based on the relay name
%%% nostr_manager_client_sup:start_client_sup([{host, "relay.nostrich.de"}]).
%%% '''
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_controller_sup/2, spec_controller_sup/1]).
-export([start_router_sup/2, spec_router_sup/1]).
-export([start_subscription_sup/2, spec_subscription_sup/1]).
-export([start_connection_sup/2, spec_connection_sup/1]).
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
    [ spec_controller_sup(Args)
    , spec_router_sup(Args)
    , spec_subscription_sup(Args)
    , spec_connection_sup(Args)
    ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
supervisor() ->
    #{ strategy => one_for_one
     }.

%%--------------------------------------------------------------------
%% @doc `spec_connection_sup' function creates a child spec for a
%% connection supervisor.
%%
%% @see start_connection_sup/2
%% @end
%%--------------------------------------------------------------------
-spec spec_connection_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_connection_sup(Args) ->
    #{ id => nostr_client_connection_sup
     , start => {nostr_client_connection_sup, start_link, [Args]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_connection_sup(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_connection_sup(Pid, Args) ->
    supervisor:start_child(Pid, spec_connection_sup(Args)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_subscription_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_subscription_sup(Args) ->
    #{ id => nostr_client_subscription_sup
     , start => {nostr_client_subscription_sup, start_link, [Args]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_subscription_sup(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_subscription_sup(Pid, Args) ->
    supervisor:start_child(Pid, spec_subscription_sup(Args)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_router_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_router_sup(Args) ->
    #{ id => nostr_client_router_sup
     , start => {nostr_client_router_sup, start_link, [Args]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_router_sup(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_router_sup(Pid, Args) ->
    supervisor:start_child(Pid, spec_router_sup(Args)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_controller_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_controller_sup(Args) ->
    #{ id => nostr_client_controller_sup
     , start => {nostr_client_controller_sup, start_link, [Args]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_controller_sup(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_controller_sup(Pid, Args) ->
    supervisor:start_child(Pid, spec_controller_sup(Args)).
