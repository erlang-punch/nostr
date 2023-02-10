%%%===================================================================
%%% @doc `nostr_client_connection_sup' module is a supervisor to
%%% supervise a client connection process created by using module e
%%% `nostr_client_connection'.
%%%
%%% == Examples ==
%%%
%%% ```
%%% nostr_client_connection_sup:start_link([{host, "relay.nostrich.de"}]).
%%% pg:get_members(client, {"relay.nostrich.de", connection}).
%%% '''
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% ===================================================================
-module(nostr_client_connection_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([create_connection/2, spec_connection/1]).

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
    #{ strategy => one_for_one
     }.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
children(Args) ->
    [spec_connection(Args)
    ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec create_connection(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
create_connection(Pid, Args) ->
    supervisor:start_child(Pid, spec_connection(Args)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec spec_connection(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
spec_connection(Args) ->
    Start = {nostr_client_connection, start_link, [Args]},
    #{ id => nostr_client_connection
     , start => Start
     , type => worker
     }.
