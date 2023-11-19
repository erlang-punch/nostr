%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%%
%%% `nostr_relay_module_init' module is the first module to be called
%%% and will help to filter allowed messages dropping all other
%%% messages.
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_module_init).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc parse a message and if valid go to the next action.
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().

% nostr relay only support text message.
init({text, Message} = Data, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Data, State]}]),
    case nostrlib:decode(Message) of
        {ok, Decoded, _} ->
            {next, Decoded};
        Error ->
            ?LOG_ERROR("~p", [{self(), ?MODULE, init, [Data], Error}]),
            Return = #notice{ message = <<"command not supported">> },
            {stop, Return}
    end;

% binary messages are not supported there.
init({binary, _} = Data, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Data, State]}]),
    Return = #notice{ message = <<"binary not supported">> },
    {stop, Return};

% all other types are just dropped
init(Data, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, init, [Data, State], unsupported}]),
    Return = #notice{ message = <<"unsupported command">> },
    {stop, Return}.
