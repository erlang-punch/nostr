%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc 
%%%
%%% An example of nip01 implementation as module callback. It has been
%%% created to test nostr relay main behavior.
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_module_nip01).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc Adds support for event, request and close message.
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().

% when an event request is received, we assume it's a valid one and it
% is directly forwarded to another module to be stored in the
% database.
init(#event{} = Event, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Event, State]}]),
    {{next, nostr_relay_module_store}, Event};

% a request for subscription, when it arrives, it should already be a
% valid one and then is forwarded to the module in cahrge of the
% subscriptions.
init(#request{} = Request, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Request, State]}]),
    {{next, nostr_relay_module_request}, Request};

% a close message is received and should then be forwarded to the
% module in charge of the subscriptions
init(#close{} = Close, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Close, State]}]),
    {{next, nostr_relay_module_request}, Close};

% At this time, we just assume we don't support this message.
init(Data, _State) ->
    ?LOG_ERROR("~p", [{self(), ?MODULE, init, [Data]}]),
    Notice = #notice{ message = <<"command not supported">> },
    {stop, Notice}.
