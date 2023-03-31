%%%===================================================================
%%% @doc THIS IS A DRAFT
%%%
%%% ```
%%% rr(nostrlib).
%%% mnesia:start().
%%%
%%% % create a new table
%%% mnesia:create_table(event, [{attributes, record_info(fields, event)}]).
%%%
%%% % create a new event
%%% {ok, E2} 
%%%   = nostrlib:encode(#event{content = <<"test">>, kind = text_note},
%%%      [{private_key, <<1:256>>}, {as_record, true}]).
%%%
%%% % insert this event in the table
%%% _ = mnesia:transaction(fun() -> mnesia:write(E2) end).
%%%
%%% % search the value by the id of the event
%%% % returns only the content of this event
%%% mnesia:transaction(fun() ->
%%%   Search = #event{ id = E2#event.id
%%%                  , content = '$1'
%%%                  , created_at = '_'
%%%                  , signature = '_'
%%%                  , kind = '_'
%%%                  , public_key = '_' }
%%%   mnesia:select(event, [{Search,[],['$1']}])
%%% end)
%%%
%%% % search the value by the id of the event
%%% % returns only the content of this event
%%% mnesia:transaction(fun() ->
%%%   Search = #event{ id = E2#event.id
%%%                  , content = '$1'
%%%                  , created_at = '_'
%%%                  , signature = '_'
%%%                  , kind = '_'
%%%                  , public_key = '$2' }
%%%   mnesia:select(event, [{Search,[],[['$1', '$2']]}])
%%% end)
%%%
%%% Prefix =:= erlang:binary_part(E2#event.id, {0, 8}).
%%%
%%% % convert a fun to a match spec
%%% ets:fun2ms(
%%%    fun(#event{ id = Id
%%%              , content = Content
%%%              , created_at = _
%%%              , signature = _
%%%              , kind = _, public_key = _}) 
%%%      when Prefix =:= binary_part(Id, {0,2}) -> 
%%%        Content 
%%%    end).
%%%
%%% % Pattern Spec to extract the whole content
%%% % [{#event{id = '$1', public_key = '_', created_at = '_',
%%% %          kind = '_', tags = '_', content = '$2', signature = '_'},  
%%% %  [{'=:=',{const,<<202,244,196,122,235,105,92,155>>},
%%% %          {binary_part,'$1',{{0,8}}}
%%% %   }],
%%%
%%% % lambda function to extract a part of an id.
%%% fun(Prefix) ->
%%%   mnesia:transaction(fun() -> 
%%%     Event = #event{ id = '$2', content = '$1', created_at = '_'
%%%                   , signature = '_', kind = '_', public_key = '_'},
%%%     Guard = [{'=:=',{const, Prefix}
%%%                    ,{binary_part,'$2',{{0,8}}}
%%%             }],
%%%     Select = ['$1'],
%%%     mnesia:select(event, [{Event, Guard, Select}]) 
%%%   end)
%%% end.
%%% '''
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_store).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([add/3, export/1]).
-include("nostrlib.hrl").
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

