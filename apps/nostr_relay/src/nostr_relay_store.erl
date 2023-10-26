%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc THIS IS A DRAFT
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_store).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([add/3, export/1]).
-include_lib("nostrlib/include/nostrlib.hrl").
-record(state, { ets = undefined }).

%%--------------------------------------------------------------------
%% @doc start a new store.
%% @todo fix the specification
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, pid()}.
start_link(Args) ->
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc initialize a new store.
%% @todo fix the specifications
%% @todo add multi port/domain support
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, #state{ ets :: reference() }}.
init(Args) ->
    Port = proplists:get_value(port, Args, 4000),
    Domain = proplists:get_value(domain, Args, '_'),
    pg:join(relay, {?MODULE, Port, Domain}, self()),
    State = #state{ ets = ets:new(?MODULE, [set]) },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc stop a store
%% @todo fix the specifications
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: any(),
      State :: #state{ ets :: reference() },
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: any(),
      State :: #state{ ets :: reference() },
      Return :: {noreply, State}.
handle_cast({add, #event{ id = Id } = Message}, #state{ ets = Ets } = State) ->
    ets:insert(Ets, {Id, Message}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: any(),
      From :: any(),
      State :: #state{ ets :: reference() },
      Return :: {reply, ok, State}.
handle_call(export, _From, #state{ ets = Ets} = State) ->
    Return = ets:tab2list(Ets),
    {reply, Return, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: any(),
      State :: #state{ ets :: reference() },
      Return :: {noreply, State}.
handle_info(_Message, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc add a new message in ets store.
%% @end
%%--------------------------------------------------------------------
-spec add(Message, Labels, Args) -> Return when
      Message :: any(),
      Labels :: any(),
      Args :: proplists:proplists(),
      Return :: ok.
add(Message, _Labels, Args) ->
    Port = proplists:get_value(port, Args),
    Domain = proplists:get_value(domain, Args),
    case pg:get_members(relay, {?MODULE, Port, Domain}) of
        [Pid] ->
            gen_server:cast(Pid, {add, Message});
        [] ->
            {error, ?MODULE}
    end.

%%--------------------------------------------------------------------
%% @doc exports the content of the ets as list.
%% @end
%%--------------------------------------------------------------------
-spec export(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: any().
export(Args) ->
    Port = proplists:get_value(port, Args),
    Domain = proplists:get_value(domain, Args),
    case pg:get_members(relay, {?MODULE, Port, Domain}) of
        [Pid] ->
            gen_server:call(Pid, export);
        [] ->
            {error, ?MODULE}
    end.
