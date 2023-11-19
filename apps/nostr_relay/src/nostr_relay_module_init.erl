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
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc parse a message and if valid go to the next action.
%% @end
%%--------------------------------------------------------------------
-spec init(any(), any()) -> any().

init({text, Message}, _State) ->
    case nostrlib:decode(Message) of
        {ok, Decoded, _} ->
            {next, Decoded};
        _ ->
            Return = #notice{ message = <<"command not supported">> },
            {stop, Return}
    end;
init({binary, _}, _State) ->
    Return = #notice{ message = <<"binary not supported">> },
    {stop, Return};
init(_Data, _State) ->
    Return = #notice{ message = <<"unsupported command">> },
    {stop, Return}.
