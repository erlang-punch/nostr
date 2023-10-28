%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc 
%%%
%%% This module is part of nip01 implementation, and add requests
%%% subscriptions support. The goal here is to manage subscriptions
%%% per websocket connections.
%%%
%%% @end
%%%===================================================================
-module(websocket_server_action_request).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().
% A request is received, we fetch what we need and return the result
% to the client. The subscriptions are stored directly into the
% websocket and a timer is created to fetch new events. At this time,
% it seems a bit complex to create a reactive only system, where the
% websocket is directly noticed by other process.
init(#request{ filter = Filter } = Request, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), Request, State}]),
    _Events = get_events(Filter#filter.event_ids),
    Events = [ #subscription{ id = Request#request.subscription_id
                            , content = Event 
                            } || Event <- list_events() 
             ],
    {ok, Ref} = timer:send_interval(1000, self(), subscription),
    Eose = #eose{ id = Request#request.subscription_id},
    Store = #{ Request#request.subscription_id => Request },
    {stop, Events ++ [Eose], State#{ timer => Ref 
                                   , subscriptions => Store
                                   }};

% a close event is received, it contains a subscription id and must
% remove it from the state if present. If not, we can send an invalid
% message. It's quite dirty, but it should work for now.
% @todo: if new subscription store is empty, we must stop the
%        timer. It's completely useless to let a timer without any
%        subscription.
init(#close{ subscription_id = Id } = Close, #{ subscriptions := Subscriptions } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), Close, State}]),
    case maps:get(Id, Subscriptions) of
        undefined ->
            Notice = #notice{ message = <<"invalid subscription id">> },
            {stop, Notice};
        _ ->
            NewSubs = maps:remove(Id, Subscriptions),
            {ok, State#{ subscriptions => NewSubs }}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc return the full list of events.
%% @end
%%--------------------------------------------------------------------
list_events() ->
    mnesia:dirty_select(event, [{'$1', [], ['$1']}]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc return events based on their ids.
%% @end
%%--------------------------------------------------------------------
get_events(Ids) ->
    lists:flatten([ get_event(Id) || Id <- Ids ]).

%%--------------------------------------------------------------------
%% @doc return one event based on its id.
%% @end
%%--------------------------------------------------------------------
get_event(Id) ->
    mnesia:dirty_read(event, Id).
