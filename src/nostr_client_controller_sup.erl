%%%===================================================================
%%% @doc nostr_client_controller_sup process is in charge of managing
%%% a group of client processes.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client_controller_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_controller/2, spec_controller/1]).

%%--------------------------------------------------------------------
%% @doc
%% @see supervisor:start_link/2
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:startlink_ret().
start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%%--------------------------------------------------------------------
%% @doc
%% @see supervisor:init/1
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
%% @doc internal function returning the supervisor spec.
%% @end
%%--------------------------------------------------------------------
supervisor() ->
    #{ strategy => one_for_one
     }.

%%--------------------------------------------------------------------
%% @doc internal function returning the children specs.
%% @end
%%--------------------------------------------------------------------
children(Args) ->
    [ spec_controller(Args)
    ].

%%--------------------------------------------------------------------
%% @doc `spec_controller/1' function creates a child spec for a
%% controller.
%%
%% @see start_controller/2
%% @end
%%--------------------------------------------------------------------
-spec spec_controller(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_controller(Args) ->
    #{ id => nostr_client_controller
     , start => {nostr_client_controller, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc `start_controller/2' function starts a new controller.
%%
%% @see spec_controller/1
%% @end
%%--------------------------------------------------------------------
-spec start_controller(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_controller(Pid, Args) ->
    supervisor:start_child(Pid, spec_controller(Args)).

