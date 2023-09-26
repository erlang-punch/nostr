%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
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
%%%===================================================================
-module(nostr_client_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_controller_sup/2, spec_controller_sup/1]).
-export([start_router_sup/2, spec_router_sup/1]).
-export([start_subscription_sup/2, spec_subscription_sup/1]).
-export([start_connection_sup/2, spec_connection_sup/1]).
-export([start_contacts/2, spec_contacts/1]).
-export([start_key/2, spec_key/1]).
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
    , spec_contacts(Args)
    , spec_key(Args)
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


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_contacts(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_contacts(Args) ->
    #{ id => nostr_client_contacts
     , start => {nostr_client_contacts, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_contacts(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_contacts(Pid, Args) ->
    supervisor:start_child(Pid, spec_contacts(Args)).

%%--------------------------------------------------------------------
%% @doc `spec_key/1' returns supervision child specification for
%% `nostr_client_key'.
%%
%% @end
%%--------------------------------------------------------------------
-spec spec_key(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_key(Args) ->
    #{ id => nostr_client_key
     , start => {nostr_client_key, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc `start_key/2' starts a new `nostr_client_key' child.
%%
%% @see nostr_client_key/1
%% @end
%%--------------------------------------------------------------------
-spec start_key(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_key(Pid, Args) ->
    supervisor:start_child(Pid, spec_key(Args)).
