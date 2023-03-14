%%%===================================================================
%%% @doc `nostr_client_router' module is in charge of parsing,
%%% validating, checking and routing the messages coming from
%%% `nostr_client_connection' process. The goal is to isolate all
%%% these actions in a pool of these process tagged with `pg' with the
%%% tag `{ClientId, router}'.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client_router).
-vsn("0.0.1").
-behavior(gen_server).
-export([start_link/1]).
-export([init/1]).
-export([terminate/2]).
-export([raw/2, raw_pool/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, {}).
-type nostr_client_router_state() :: #state{}.

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
%% @doc `raw/2' function sends a raw message to a specific pid.

%% @see gen_server:cast/2
%% @end
%%--------------------------------------------------------------------
-spec raw(Pid, Data) -> Return when
      Pid :: atom() | pid(),
      Data :: iodata(),
      Return :: ok.
raw(Pid, Data) ->
    gen_server:cast(Pid, {raw, Data}).

%%--------------------------------------------------------------------
%% @doc `raw/2' function sends a raw message to a a pool of
%% `nostr_client_router' based on a client identifier.
%%
%% @end
%% --------------------------------------------------------------------
-spec raw_pool(ClientId, Data) -> Return when
      ClientId :: to_be_defined(),
      Data :: iodata(),
      Return :: ok.
raw_pool(ClientId, Data) ->
    Routers = pg:get_members(client, {ClientId, router}),
    RoutersLength = length(Routers),
    RouterIndex = rand:uniform(RoutersLength),
    Router = lists:nth(RouterIndex, Routers),
    raw(Router, Data).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, State} | {stop, to_be_defined()},
      State :: nostr_client_router_state().
init(Args) ->
    % @TODO remove debug mode
    logger:set_module_level(?MODULE, debug),
    Host = proplists:get_value(host, Args, undefined),
    pg:join(client, {Host, router}, self()),
    {ok, #state{}}.

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
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

% receive a raw message and do all the parser/router magic
handle_cast({connection, Host, Data} = _Message, State) ->
    case nostrlib:decode(Data) of
        {ok, #subscription{ id = SubscriptionId
                          , content = #event{ id = _Id
                                            , kind = _Kind }} = Parsed, Labels} ->
            {ok, Subscription} = nostr_client:get_process(Host, {subscription, SubscriptionId}),
            Subscription ! Parsed,
            ?LOG_DEBUG("~p", [{?MODULE, self(), parsed, {Parsed, Labels}, State}]);
            % file:write_file(filename:join(<<"_data">>, [Kind, <<"-">>, base64:encode(Id)]), Data);

        Elsewise ->
            ?LOG_DEBUG("~p", [{?MODULE, self(), parsed, Elsewise, State}])
    end,
    {noreply, State};

    
handle_cast({raw, _Data} = Message, State) ->
    ?LOG_DEBUG("old: ~p", [{?MODULE, self(), cast, Message, State}]),
    {noreply, State};

handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message, State}]),
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
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: to_be_defined(),
      From :: gen_server:from(),
      State :: to_be_defined(),
      Return :: to_be_defined().
handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), call, Message, From, State}]),
    {reply, ok, State}.
