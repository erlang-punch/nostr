%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc `nostr_client_subscription' module was created to managed
%%% individual subscription per client. This module will also be in
%%% charge to deal with the actions when a message is coming with a
%%% specific filter.
%%%
%%% == Examples ==
%%%
%%% ```
%%% % start a new subcription connected to an existing client
%%% Filter = [{limit, 10}, {kinds, [0,1]}].
%%% {ok, Subscription} = nostr_client_subscription:start([
%%%   {client, Client}, 
%%%   {filter, Filter}
%%% ]).
%%%
%%% % stop the subscription
%%% ok = nostr_client_subscription:stop(Subscription).
%%% '''
%%%
%%% @end
%%% ===================================================================
-module(nostr_client_subscription).
-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, { subscription_id = undefined :: binary()
               , consumer        = undefined :: pid()
               , filter          = undefined :: #filter{}
               , host            = undefined :: string()
               }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start(Args) ->
    logger:set_module_level(?MODULE, debug),
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, to_be_defined()} | {stop, to_be_defined()}.
init(Args) ->
    Consumer = proplists:get_value(consumer, Args, undefined),
    Host = proplists:get_value(host, Args, undefined),
    Filter = proplists:get_value(filter, Args, undefined),
    SubscriptionId = nostrlib:new_subscription_id(),
    pg:join(client, {Host, {subscription, SubscriptionId}}, self()),
    State = #state{ subscription_id = SubscriptionId
                  , host = Host
                  , filter = Filter
                  , consumer = Consumer
                  },
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: to_be_defined(),
      From :: gen_server:from(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), call, Message, From, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State}.
