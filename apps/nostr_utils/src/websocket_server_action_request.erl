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
-export([websocket_info/1, websocket_info/2]).
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
init(#request{ filter = Filter } = Request, #{ ?MODULE := Subscriptions } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), Request, State}]),
    % we are fetching all events based on the filter
    _Events = get_events(Filter#filter.event_ids),
    Events = [ #subscription{ id = Request#request.subscription_id
                            , content = Event 
                            } || Event <- list_events() 
             ],

    % because we don't have any other events, we can prepare an EOSE
    % message
    Eose = #eose{ id = Request#request.subscription_id},

    case Subscriptions of 
        % a timer is already configured and subscriptions is a list
        #{ timer := {interval, _Ref}
         , subscriptions := Subs } ->
            NewSubs = Subs#{ Request#request.subscription_id => Request },
            NewStore = Subscriptions#{ subscriptions => NewSubs 
                                     , updated_at => erlang:system_time()
                                     },
            {stop, Events ++ [Eose], State#{ ?MODULE => NewStore }};

        % or subscriptions is totally empty, then we need to create it
        % and create a new timer
        _Elsewise ->
            {ok, Ref} = timer_create({subscriptions, update}),
            NewSubs = #{ Request#request.subscription_id => Request },
            NewStore = #{ timer => Ref
                        , subscriptions => NewSubs 
                        , updated_at => erlang:system_time() 
                        },
            {stop, Events ++ [Eose], State#{ ?MODULE => NewStore }}
    end;

% (24_38:) by default, we assume a client does not have any
% subscriptions, if a new subscription is valid, then we can add a
%  key with module name in the map.
init(#request{ filter = Filter } = Request, State) ->
    % we are fetching all events based on the filter
    _Events = get_events(Filter#filter.event_ids),
    Events = [ #subscription{ id = Request#request.subscription_id
                            , content = Event 
                            } || Event <- list_events() 
             ],

    % because we don't have any other events, we can prepare an EOSE
    % message
    Eose = #eose{ id = Request#request.subscription_id},

    {ok, Ref} = timer_create({subscriptions, update}),
    NewSub = #{ Request#request.subscription_id => Request},
    NewStore = #{ timer => Ref
                , subscriptions => NewSub 
                , updated_at => erlang:system_time()
                },
    {stop, Events ++ [Eose], State#{ ?MODULE => NewStore }};

% a close event is received, it contains a subscription id and must
% remove it from the state if present. If not, we can send an invalid
% message. It's quite dirty, but it should work for now.
% @todo: if new subscription store is empty, we must stop the
%        timer. It's completely useless to let a timer without any
%        subscription.

% if our module state name as key the subscription id is false.
init(#close{ subscription_id = _Id }, State)
  when not is_map_key(?MODULE, State) ->
    Notice = #notice{ message = <<"invalid subscription id">> },
    {stop, Notice};

% if we have our module state name as key, then we have or had
% subscriptions.
init(#close{ subscription_id = Id } = Close
    ,#{ ?MODULE := #{ timer := Timer
                    , subscriptions := Subs 
                    }
      } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), Close, State}]),
    case maps:get(Id, Subs) of
        undefined ->
            Notice = #notice{ message = <<"invalid subscription id">> },
            {stop, Notice};
        _Request ->
            NewSubs = maps:remove(Id, Subs),
            NewTimer = case erlang:map_size(NewSubs) of
                           0 ->
                               timer:cancel(Timer),
                               undefined;
                           _ -> Timer
                       end,
            NewStore = #{ timer => NewTimer
                        , subscriptions => NewSubs 
                        , updated_at => erlang:system_time()
                        },
            NewState = State#{ ?MODULE => NewStore },
            {ok, NewState}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(any()) -> any().
websocket_info(State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), websocket_info, 1, State}]),
    {[], State}.

%%--------------------------------------------------------------------
%% @hidden
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(any(), any()) -> any().

% when a subscriptions update message is received, we can read all
% requests from the state and update them one by one.
websocket_info({subscriptions, update} = Args, #{ ?MODULE := Store } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), websocket_info, 2, Args, State}]),
    Subs = maps:get(subscriptions, Store, #{}),
    NewSubs = maps:map(fun subscriptions_update/2, Subs),
    {[], State#{ ?MODULE => Store#{ subscriptions => NewSubs 
                                  , updated_at => erlang:system_time()
                                  }}}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc used to update/fetch all elements from subscriptions.
%% @end
%%--------------------------------------------------------------------
subscriptions_update(_Key, Request) -> Request.

%%--------------------------------------------------------------------
%% @hidden
%% @doc create and initialize a new timer. Default interval: 1000ms.
%% @end
%%--------------------------------------------------------------------
timer_create(Term) ->
    timer_create(Term, 1000).
timer_create(Term, Delay) ->
    timer:send_interval(Delay, self(), {callback, ?MODULE, Term}).

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
