%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%%
%%% `nostr_relay_module' was designed to deal with raw websocket
%%% events and forward them on the correct module containing the
%%% functions required to deal with them.
%%%
%%% @end
%%%====================================================================
-module(nostr_relay_module).
-export([start_modules/1, init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_modules(Modules) -> Return when
      Modules :: [atom()],
      Return :: list().

start_modules(Modules) -> 
    [ try_start(Module) || Module <- Modules ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
try_start(Module) ->
    try 
        ?LOG_INFO("start module ~p", [Module]),
        {Module, Module:start()}
    catch
        E:R -> {Module, E, R}
    end.

%%--------------------------------------------------------------------
%% @doc forward directly client event to our main loop. We assume the
%% data were correctly parsed.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Data, State) -> Return when
      Data :: term(),
      State :: term(),
      Return :: {next, atom(), State}.

init(Data, #{ websocket_pipeline := Pipeline } = State) ->
    do_loop(Data, Pipeline, State).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%%
%% main loop to deal with actions in pipeline. This function is acting
%% as a router/decoder/encoder. Adding a cleanup feature at the end of
%% the loop could probably be quite helpful to deal with missing empty
%% states.
%%
%% @end
%%--------------------------------------------------------------------
-spec do_loop(any(), [atom(), ...], term()) -> Return when 
      Return :: any().

do_loop(Data, [Action|Pipeline], State) ->
    try
        % we should be aware if one module is not implementing init
        % function but if it's the case, then we execute it.
        Action:init(Data, State)
    of
        % just do nothing and continue the execution of the socket.
        {ok, NewState} ->
            {[], NewState};

        % continue the execution with the same state and the result of
        % the previously executed action
        {next, Result} ->
            do_loop(Result, Pipeline, State);

        % continue the execution with the previous result and a new
        % state.
        {next, Result, NewState} ->
            do_loop(Result, Pipeline, NewState);

        % We also need an explicit call to another module, outside of
        % the scope of the pipeline. Storing a data for example is
        % outside of the pipeline check, and should be available to
        % all module by default. This is our final action.
        {{next, Module}, Result}
          when is_atom(Module) ->
            do_loop(Result, [Module], State);

        {{next, Module}, Result, NewState}
          when is_atom(Module) ->
            do_loop(Result, [Module], NewState);

        % stop the loop execution with a good result without closing
        % the connection
        {stop, Response} ->
            stop_exchange(Response, State);
        {stop, Response, NewState} ->
            stop_exchange(Response, NewState);

        % stop and close the loop execution and the websocket
        {error, Reason} ->
            error_exchange(Reason, State)
    catch
        % when the function is undefined
        error:undef:Stack ->
            Return = [{module, Action}
                     ,{function, init}
                     ,{error, undef}
                     ,{stack, Stack}
                     ],
            ?LOG_ERROR("~p", [{?MODULE, Return}]),
            {error, Return};

        % in case of any other errors
        _:Reason:Stack ->
            Return = [{reason, Reason}
                     ,{stack, Stack}
                     ],
            ?LOG_ERROR("~p", [{?MODULE, Return}]),
            {error, Return}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc function to format error messages, a notice should probably be
%%      used there.
%% @end
%%--------------------------------------------------------------------
error_exchange(Reason, State)
  when is_binary(Reason) ->
    {[{close, 1000, Reason}], State};
error_exchange(_, State) ->
    {[{close, 1000, <<"error">>}], State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
stop_exchange({raw, RawMessages}, State)
  when is_list(RawMessages) ->
    {RawMessages, State};
stop_exchange(Message, State) ->
    stop_encoder(Message, State).

%%--------------------------------------------------------------------
%% @hidden
%% @doc wrapper around nostrlib:encode/2
%% @end
%%--------------------------------------------------------------------
stop_encoder(Messages, State)
  when is_list(Messages) ->
    Return = [ encoder(Message) || Message <- Messages ],
    {Return, State};
stop_encoder(Message, State) ->
    case encoder(Message) of
        {text, _} = Encoded ->
            {[Encoded], State};
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc main encoder for ok, notice, eose and subscription record.
%% @end
%%--------------------------------------------------------------------
encoder(Message)
  when is_record(Message, ok) orelse
       is_record(Message, notice) orelse
       is_record(Message, eose) orelse
       is_record(Message, subscription) ->
    case nostrlib:encode(Message) of
        {ok, Encoded} ->
            {text, Encoded};
        Elsewise ->
            throw(Elsewise)
    end.
