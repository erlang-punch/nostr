%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client_controller).
-behaviour(gen_statem).
-export([start_link/1]).
-export([start/1]).
-export([callback_mode/0]).
-export([init/1, terminate/3]).
-export([disconnected/3, connected/3]).
-include_lib("nostr/include/nostrlib.hrl").
-record(state, {}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_statem:start_ret().
start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_statem:start_ret().
start(Args) ->
    gen_statem:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec callback_mode() -> Return when
      Return :: state_functions.
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_statem:init_result().
init(_Args) ->
    {ok, disconnected, #state{}}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate(Reason, State, Data) -> Return when
      Reason :: to_be_defined(),
      State :: to_be_defined(),
      Data :: to_be_defined(),
      Return :: to_be_defined().
terminate(_,_,_) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec disconnected(EventType, Message, Data) -> Return when
      EventType :: gen_statem:event_type(),
      Message :: to_be_defined(),
      Data :: to_be_defined(),
      Return :: to_be_defined().
disconnected(_,_,Data) ->
    {keep_state, Data}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec connected(EventType, Message, Data) -> Return when
      EventType :: gen_statem:event_type(),
      Message :: to_be_defined(),
      Data :: to_be_defined(),
      Return :: to_be_defined().
connected(_,_,Data) ->
    {keep_state, Data}.
