%%%===================================================================
%%% @doc
%%%
%%% websocket_server_module implement a new behavior to deal with
%%% websocket messages/events received. This is a currently a test.
%%%
%%% @end
%%%====================================================================
-module(websocket_server_module).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
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
%% @doc main loop to deal with actions in pipeline.
%% @end
%%--------------------------------------------------------------------
-spec do_loop(any(), [atom(), ...], term()) -> any().

do_loop(Data, [Action|Pipeline], State) ->
    try
        % we should be aware if one module is not implementing init
        % function but if it's the case, then we execute it.
        Action:init(Data, State)
    of
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
%%
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
    
