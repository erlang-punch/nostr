%%%===================================================================
%%% @doc DRAFT
%%%
%%% @todo improve start_relay_listener/2 function
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_relay_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_relay_listener/2, spec_relay_listener/1]).
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
    [ spec_relay_listener(Args)
    ].

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
-spec spec_relay_listener(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_relay_listener(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    Name = {nostr_relay_listener, Port},
    TransportOpts = [{port, Port}],
    Routes = [{'_', [{"/", nostr_relay_handler, []}]}],
    Dispatch = cowboy_router:compile(Routes),
    ProtocolOpts = #{ env => #{ dispatch => Dispatch }},
    #{ id => {nostr_relay_listener, Port}
     , start => {cowboy, start_clear, [Name, TransportOpts, ProtocolOpts]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_relay_listener(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_relay_listener(Pid, Args) ->
    supervisor:start_child(Pid, spec_relay_listener(Args)).
