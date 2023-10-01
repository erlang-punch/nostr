%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc
%%%
%%% this module defines a subscription on the relay side. What is a
%%% subscription in this context? Well, this is a list of process
%%% receiving potentially the same alerts based on a pattern (a list
%%% of matching terms).
%%%
%%% 1. a client want to received only a subset of all events, it
%%% create a new subscription based on a random generated on his
%%% side. We can't trust him, so, we need to store his id with its
%%% connection identifier, assuming a client can only have one
%%% websocket connection.
%%%
%%%    requirement: tuple containing a client id with a subscription
%%%                 id stored somewhere.
%%%
%%% 2. the subscription filter is created on the relay side. One
%%% client can have many subscription with only one connection and one
%%% identifier. In other hand, clients can have the same subscription
%%% filter.
%%%
%%%    requirement: something to store the subscription filters at the
%%%                 same place with a relation between
%%%                 client_id/subscription_id and the filter_id.
%%%
%%% 3. the relay/router will store the event in a database and will
%%% forward the message to a pool of subscription process.
%%%
%%%    requirement: create a supervisor to store M subscription
%%%                 processes.
%%%
%%% 4. the selected subscription process will check the content of the
%%% event based on the list of all subscription/filters available on
%%% the server. It will return the list of connection.
%%%
%%%    requirement: create a simple module using gen_server behavior,
%%%                 every time a new event is coming, it will check
%%%                 the content of the database containing the
%%%                 client_id/subscription_id/filter_id.
%%%
%%% 5. When done, the list of connection/client is being used to
%%% forward the event.
%%%
%%%    requirement: events are forwarded by the subscription process
%%%                 to the client.
%%%
%%% Note: This module is not in charge to store the events, events
%%%       will be safely stored in a dedicated store. This module and
%%%       all related processes are in charge of safely forward all
%%%       events to a group of subscribers.
%%%
%%% ```
%%% Subscription Diagram
%%% --------------------
%%%                                                  __________
%%%                                                 (          )
%%%                                                 ( database )
%%%                                                 (__________)
%%%                                                    /\
%%%                                                   /__\ (3)
%%%  _________                        [store]         _||___________
%%% |  _______|               _______/              _|_||_________  |
%%% | |        |  (1)        |       |  (2)        |              | |
%%% |_| client |--[events]-->| relay |--[events]-->| subscription |_|
%%%   |________|             |_______|             |______________|
%%%                                                  ||
%%%                                                 _||_ (4)
%%%  (1) from cowboy handler                        \  /
%%%  (2) call ?MODULE:forward/1 (public)           __\/____
%%%  (3) call ?MODULE:lookup/2 (private)          |        |_
%%%  (4) call ?MODULE:send/2 (private)            | client | |
%%%                                               |________| |
%%%                                                |_________|
%%% '''
%%%
%%% == Problems ==
%%%
%%% 1. Every time an event is sent from a client, the message is
%%% checked by N subscription filter. If one filter match, this event
%%% is the forwarded to all processes. If we have an infinite amount
%%% of clients with one subscription, it will take an infinite amount
%%% of time to forward one event. We should probably (1) limit the
%%% number of subscription per client (2) find an efficient way to
%%% check one message to N filters (3) find a way to compute that in
%%% parallel.
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_subscription).
-export([forward/1, forward/2]).
-export([start_link/1]).
-include_lib("nostr/include/nostrlib.hrl").
-include_lib("kernel/include/logger.hrl").
-behavior(gen_server).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-record(state, { id = undefined }).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc `forward/1' forward an event to a subscription process from
%% the current pool of process.
%%
%% @see forward/2
%% @todo this function is not designed correctly, we can set port/domain
%%       when we need to forward an event, except if the event can embed
%%       those information, is it a good idea?
%% @end
%% --------------------------------------------------------------------
-spec forward(Event) -> Return when
      Event :: event(),
      Return :: ok.

forward(Event) ->
    case nostr_relay:get_process(?MODULE, []) of
        {ok, Pid} ->
            forward(Pid, Event);
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc `forward/2' forwards an event to subscription process.
%% @end
%%--------------------------------------------------------------------
-spec forward(Pid, Event) -> Return when
      Pid :: pid(),
      Event :: event(),
      Return :: ok.
forward(Pid, #event{} = Event) ->
    gen_server:cast(Pid, Event).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, #state{}}.
init(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    Domain = proplists:get_value(domain, Args, '_'),
    pg:join(relay, {?MODULE, Port, Domain}, self()),
    State = #state{},
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: any(),
      State :: #state{},
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: any(),
      State :: #state{},
      Return :: {noreply, State}.
handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: any(),
      From :: any(),
      State :: #state{},
      Return :: {reply, ok, State}.
handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_call, Message, From, State}]),
    {reply, Message, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: any(),
      State :: #state{},
      Return :: {noreply, State}.
handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), handle_info, Message, State}]),
    {noreply, State}.
