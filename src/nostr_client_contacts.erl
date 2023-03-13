%%%===================================================================
%%% @doc `nostr_client_contacts' stores contact list from the server,
%%% it's an implementation of NIP/02.
%%%
%%% @end
%%%===================================================================
-module(nostr_client_contacts).
-behavior(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, { store = undefined }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start(Args) ->
    logger:set_module_level(?MODULE, debug),
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, to_be_defined()} | {stop, to_be_defined()}.
init(Args) ->
    logger:set_module_level(?MODULE, debug),
    Host = proplists:get_value(host, Args, undefined),
    pg:join(client, {Host, contacts}, self()),
    Store = sets:new([{version, 2}]),
    {ok, #state{ store = Store}}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Return :: ok.
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_cast({add, {{Author, PublicKey}, _Extra} = Data}, #state{ store = Store } = State) 
  when is_binary(Author) andalso is_binary(PublicKey) ->
    {noreply, State#state{ store = sets:add_element(Data, Store) }};
handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: to_be_defined(),
      From :: gen_server:from(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_call(get, _, #state{ store = Store } = State) ->
    {reply, Store, State};
handle_call(Message, From, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), call, Message, From, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State}.

