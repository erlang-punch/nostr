%%%===================================================================
%%% @doc DRAFT: `nostr_client_router' module is in charge of parsing,
%%% validating, checking and routing the messages coming from
%%% `nostr_client_connection' process. The goal is to isolate all
%%% these actions in a pool of these process tagged with `pg' with the
%%% tag `{ClientId, router}'.
%%%
%%% ```
%%% % Create a new router process
%%% {ok, R} = nostr_router:start_link([]).
%%%
%%% % Create a new event
%%% {ok, EE} = nostrlib:encode(#event{ kind = text_note, content = <<"test">> }
%%%                            [{as_record, true}, {private_key, <<1:256>>}]).
%%%
%%% % send a message to the process
%%% erlang:spawn(fun() -> gen_server:call(R, {"wss://test.com", EE}, 1000) end), 
%%%   % wait 10 millisecond
%%%   timer:sleep(10), 
%%%   % send the answer
%%%   gen_server:cast(R, {"wss://test.com", {ok, {EE#event.id, data}}}).
%%% '''
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_router).
-vsn("0.0.1").
-behavior(gen_server).
-export([start_link/1]).
-export([init/1]).
-export([terminate/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-export([call/1, call/2, cast/1]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, { store = undefined }).

%%--------------------------------------------------------------------
%% @doc
%% @see gen_server:start_link/3
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, State} | {stop, to_be_defined()},
      State :: any().
init(_Args) ->
    logger:set_module_level(?MODULE, debug),
    pg:join(client, router_test, self()),
    Store = maps:new(),
    State = #state{ store = Store },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: to_be_defined(),
      From :: gen_server:from(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_call({Relay, #event{ id = <<Id:256/bitstring>> }} = Message, From, #state{ store = Store } = State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, handle_call, Message, From, State}]),
    NewStore = maps:put({Id, Relay}, {From, erlang:monotonic_time()}, Store),
    {noreply, State#state{ store = NewStore }};
    
handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, handle_call, Message, From, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_cast({Relay, {ok, Id, _Message}} = Message, #state{ store = Store } = State) ->
    case Store of
        #{{Relay, Id} := {From, _}} ->
            ?LOG_DEBUG("~p", [{self(), ?MODULE, handle_cast, Message, State}]),
            gen_server:reply(From, ok),
            NewStore = maps:delete({Relay, Id}, Store),
            {noreply, State#state{ store = NewStore }};
        _ ->
            {noreply, State}
    end;

handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, handle_cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{self(), ?MODULE, handle_info, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @see cast/3
%% @end
%%--------------------------------------------------------------------
-spec call(term()) -> any().
call(Message) ->
    call(Message, 1000).

%%--------------------------------------------------------------------
%% @doc
%% @see cast/3
%% @end
%%--------------------------------------------------------------------
-spec call(term(), integer()) -> any().
call(Message, Timeout) ->
    case get_router() of
        {ok, Router} ->
            call(Router, Message, Timeout);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec call(pid(), term(), integer()) -> any().
call(Pid, Message, Timeout) ->
    gen_server:call(Pid, {call, self(), Message}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% @see cast/2
%% @end
%%--------------------------------------------------------------------
-spec cast(term()) -> ok.
cast(Message) ->
    case get_router() of
        {ok, Router} ->
            cast(Router, Message);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec cast(pid(), term()) -> ok.
cast(Pid, Message) ->
    gen_server:cast(Pid, {cast, self(), Message}).

-spec  get_router() -> {ok, pid()}.
get_router() ->
    case pg:get_members(client, router_test) of
        Routers when is_list(Routers) ->
            RoutersLength = length(Routers),
            RouterIndex = rand:uniform(RoutersLength),
            Router = lists:nth(RouterIndex, Routers),
            {ok, Router};
        [] ->
            {error, not_started}
    end.
