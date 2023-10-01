%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_manager_client_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_client_sup/1, spec_client_sup/1]).
-include_lib("nostr/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startlink_ret().
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc
%% @end
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
children() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_client_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_client_sup(Args) ->
    Host = proplists:get_value(host, Args, undefined),
    #{ id => {nostr_client_sup, Host}
     , start => {nostr_client_sup, start_link, [Args]}
     , type => supervisor
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_client_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_client_sup(Args) ->
    supervisor:start_child(?MODULE, spec_client_sup(Args)).

