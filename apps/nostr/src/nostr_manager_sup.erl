%%%===================================================================
%%% @doc `nostr_manager_sup' manages the manager process but also
%%% `nostr_manager_client_sup' and `nostr_manager_relay_sup' processes
%%% respectively responsible of the client and relay services.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_manager_sup).
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
    State = {supervisor(), children()},
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
children() ->
    [nostr_manager_client_sup()
    ,nostr_manager_relay_sup()
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
nostr_manager_client_sup() ->
    #{ id => nostr_manager_client_sup
     , start => {nostr_manager_client_sup, start_link, [[]]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
nostr_manager_relay_sup() ->
    #{ id => nostr_manager_relay_sup
     , start => {nostr_manager_relay_sup, start_link, []}
     , type => supervisor
     }.
