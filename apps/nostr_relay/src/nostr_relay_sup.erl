%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc DRAFT
%%%
%%% @todo improve start_relay_listener/2 function
%%% @end
%%%===================================================================
-module(nostr_relay_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).
-export([start_relay_listener/2, spec_relay_listener/1]).
-export([start_relay_store/2, spec_relay_store/1]).
-export([start_relay_subscription_sup/2, spec_relay_subscription_sup/1]).
-include_lib("nostrlib/include/nostrlib.hrl").

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
    , spec_relay_store(Args)
    , spec_relay_subscription_sup(Args)
    , #{ id => pg
       , start => {pg, start_link, [nostr_relay]}
       , type => worker
       }
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
    #{ id => {nostr_relay_listener, Port}
     , start => {nostr_relay_listener, start_link, [Args]}
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

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_relay_store(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_relay_store(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    #{ id => {nostr_relay_store, Port}
     , start => {nostr_relay_store, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_relay_store(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_relay_store(Pid, Args) ->
    supervisor:start_child(Pid, spec_relay_store(Args)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec spec_relay_subscription_sup(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: supervisor:child_spec().
spec_relay_subscription_sup(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    #{ id => {nostr_relay_subscription_sup, Port}
     , start => {nostr_relay_subscription_sup, start_link, [Args]}
     , type => worker
     }.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_relay_subscription_sup(Pid, Args) -> Return when
      Pid :: supervisor:sup_ref(),
      Args :: proplists:proplists(),
      Return :: supervisor:startchild_ret().
start_relay_subscription_sup(Pid, Args) ->
    supervisor:start_child(Pid, spec_relay_subscription_sup(Args)).
