%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc `nostr_client_connection' module is responsible to maintain
%%% the HTTP and Websocket connections alive. This module will is
%%% currently using `gen_server' behavior but will be migrated to
%%% `gen_statem'.
%%%
%%% == Examples ==
%%%
%%% ```
%%% % create a new connection to relay.nostrich.de
%%% {ok, Pid} = nostr_client_connection:start([{host, "relay.nostrich.de"}]).
%%%
%%% % craft a request
%%% Subscription = nostrlib_client:create_subscription_id().
%%% Req = [<<"REQ">>, Subscription, #{kinds => [0,1],limit => 10}].
%%%
%%% % send a raw payload to the relay converted with thoas.
%%% nostr_client_connection:send_raw(Pid, thoas:encode(Req)).
%%%
%%% % use send_json/2 to convert automatically the payload to json
%%% % object (the request should be compatible with Erlang terms).
%%% nostr_client_connection:send_json(Pid, Req).
%%%
%%% % close the connection
%%% nostr_client_connection:stop(Pid).
%%% '''
%%%
%%% == Connections ==
%%%
%%% A connection is created from configuration file. A list of is
%%% given by the user and then automatically created in mnesia
%%% database.
%%%
%%% ```
%%% {nostr_client_relays, [
%%%    #{ host => "relay.nostrss.re" 
%%%     , connected => false 
%%%     },
%%%    #{ host => "relay.n057r.club"
%%%     , port => 443
%%%     , tls => true
%%%     , connected => true 
%%%     }
%%% ]}.
%%% '''
%%%
%%% - host (mandatory): hostname of the relay
%%% - port (optional, default `443'): tcp port of the relay
%%% - tls (optional, default `true'): connection uses ssl/tls
%%% - connected (optional, default `true'): automatically connect 
%%%   to the relay
%%%
%%% ```
%%%
%%% These information are then loaded into mnesia table
%%% `nostr_client_connection' or manually using
%%% `nostr_client_connection:add/1'.
%%%
%%% ```
%%% Relay = #{ host => "relay.nostrss.re" 
%%%          , connected => true },
%%% ok = nostr_client_connection:add(Relay).
%%% '''
%%%
%%% nostr_client_connection or `nostr_client_connection_manager' will
%%% be noticed by this modification and start a new
%%% `nostr_client_connection' worker running in the state connected or
%%% disconnected.
%%%
%%% @todo create more examples
%%% @end
%%%===================================================================
-module(nostr_client_connection).
-vsn("0.0.1").
-behaviour(gen_statem).
-export([start/1, start/2]).
-export([start_link/1, start_link/2]).
-export([stop/1]).
-export([send_raw/2, send_json/2]).
-export([get_state/1]).
-export([callback_mode/0, init/1, terminate/3]).
-export([connected/3]).
-include_lib("kernel/include/logger.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").
-record(?MODULE, { process         = undefined
                 , connection      = undefined
                 , websocket       = undefined
                 , subscriptions   = #{}
                 , arguments       = []
                 , http_link       = false
                 , websocket_link  = false
                 , port            = 443
                 , host            = undefined
                 , transport       = undefined
                 , path            = undefined
                 , websocket_opts  = undefined
                 , tls             = true
                 , tls_opts        = undefined
                 , connection_opts = undefined
                 % by default we forward to nostr_client_router
                 % process. creating a registry could be helpful
                 % here.
                 , forward         = nostr_client_router
                 }).
-type nostr_client_connection_state() :: #?MODULE{}.

%%--------------------------------------------------------------------
%% @doc `start/2' function creates a `nostr_client_connection'
%% process.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @todo create examples
%% @see start/3
%% @end
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().

start(Args) ->
    start(Args, []).

%%--------------------------------------------------------------------
%% @doc `start/2' function creates a new `nostr_client_connection'
%% process.
%%
%% This function has been created mainly to help developer to deal
%% with the connection in an Erlang shell. It should be used for test
%% only.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @todo create examples
%% @see start/2
%% @see gen_server:start/3
%% @end
%%--------------------------------------------------------------------
-spec start(Args, Opts) -> Return when
      Args :: proplists:proplists(),
      Opts :: any(),
      Return :: gen_server:start_ret().

start(Args, Opts) ->
    gen_statem:start(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc `start_link/1' function creates a new linked
%% `nostr_client_connection' process.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @todo create examples
%% @see start_link/3
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: gen_server:start_ret().

start_link(Args) ->
    start_link(Args, []).

%%--------------------------------------------------------------------
%% @doc `start_link/2' function creates a new linked
%% `nostr_client_connection' process.
%%
%% This function is usually called by a supervisor like
%% `nostr_client_sup'.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @see start_link/2
%% @see nostr_client_sup
%% @see gen_server:start_link/3
%% @todo create examples
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args, Opts) -> Return when
      Args :: proplists:proplists(),
      Opts :: any(),
      Return :: gen_server:start_ret().

start_link(Args, Opts) ->
    gen_statem:start_link(?MODULE, Args, Opts).

%%--------------------------------------------------------------------
%% @doc `stop/1' stops the current connection.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @see gen_server:stop/1
%% @todo create examples
%% @end
%%--------------------------------------------------------------------
-spec stop(Pid) -> Return when
      Pid :: pid() | atom(),
      Return :: ok.

stop(Pid) ->
    gen_statem:stop(Pid).

%%--------------------------------------------------------------------
%% @doc `send_raw/2' function sends a raw payload to the relay.
%%
%% This function does not control, valid or verify the content of the
%% payload.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @see gen_server:cast/2
%% @todo create examples
%% @end
%%--------------------------------------------------------------------
-spec send_raw(Pid, RawMessage) -> Return when
      Pid :: pid() | atom(),
      RawMessage :: iodata(),
      Return :: ok.

send_raw(Pid, RawMessage) ->
    gen_statem:cast(Pid, {raw, RawMessage}).

%%--------------------------------------------------------------------
%% @doc `send_json/2' sends a JSON object to the relay.
%%
%% The JSON object is converted from a valid Erlang Term with
%% `thoas:encode/1' without any kind of validation or verification.
%%
%% == Examples ==
%%
%% ```
%% '''
%%
%% @see send_raw/2
%% @see thoas:encode/1
%% @see gen_server:cast/2
%% @todo create examples
%% @end
%%--------------------------------------------------------------------
-spec send_json(Pid, Term) -> Return when
      Pid :: pid() | atom(),
      Term :: term(),
      Return :: to_be_defined().
send_json(Pid, Term) ->
    Json = thoas:encode(Term),
    send_raw(Pid, Json).

%%--------------------------------------------------------------------
%% @doc a way to extract process state, debug only.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid) -> Return when
      Pid :: pid() | atom(),
      Return :: #?MODULE{}.

get_state(Pid) ->
    gen_statem:call(Pid, {get, state}, 1000).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec callback_mode() -> any().

callback_mode() -> [state_functions, state_enter].

%%--------------------------------------------------------------------
%% @doc `init/1' callback create and initizalize the state of a new
%% `nostr_client_connection' process.
%%
%% This function is getting a list of arguments with default value to
%% create the state of the running process. If all arguments are
%% correct, an HTTP connection is created and directly upgraded to a
%% Websocket one using `gun'.
%%
%% @see proplists:get_value/3
%% @see gen_server:init/1
%% @see public_key:cacert_get/0
%% @see gun:open/3
%% @see gun:ws_upgrade/3
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, State} | {stop, to_be_defined()},
      State :: nostr_client_connection_state().

init(Args) ->
    State = #?MODULE{},
    init_mnesia(Args, State).

%%--------------------------------------------------------------------
%% @doc Create Mnesia table to store all connection states.
%% @end
%%--------------------------------------------------------------------
init_mnesia(Args, State) ->
    _ = mnesia:create_table(?MODULE, [{attributes, record_info(fields, ?MODULE)}]),
    init_host(Args, State).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_host(Args, State)->
    case proplists:get_value(host, Args) of
        Host when is_list(Host) -> 
            NewState = State#?MODULE{ process = self()
                                    , host = Host },
            init_port(Args, NewState);
        Host ->
            {stop, {invalid, Host}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_port(Args, State) ->
    case proplists:get_value(port, Args, 443) of
        Port when is_integer(Port) ->
            NewState = State#?MODULE{ port = Port },
            init_transport(Args, NewState);
        Port ->
            {stop, {invalid, Port}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_transport(Args, State) ->    
    case proplists:get_value(transport, Args, tls) of
        tls ->
            NewState = State#?MODULE{ transport = tls },
            init_path(Args, NewState);
        Transport ->
            {stop, {invalid, Transport}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_path(Args, State) ->
    case proplists:get_value(path, Args, "/") of
        Path when is_list(Path) ->
            NewState = State#?MODULE{ path = Path },
            init_websocket_options(Args, NewState);
        Path ->
            {stop, {invalid, Path}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_websocket_options(Args, State) ->
    case proplists:get_value(websocket_opts, Args, []) of
        WebsocketOpts when is_list(WebsocketOpts) ->
            NewState = State#?MODULE{ websocket_opts = WebsocketOpts },
            init_tls(Args, NewState);
        WebsocketOpts -> 
            {stop, {invalid, WebsocketOpts}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_tls(Args, State) ->
    case proplists:get_value(tls, Args, true) of
        true ->
            init_tls_opts(Args, State);
        _ ->
            init_connection(Args, State)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_tls_opts(Args, State) ->
    % CACerts = proplists:get_value(cacerts, Args, public_key:cacerts_get()),
    TLSOpts = [ {verify, verify_none}
              % , {cacerts, CACerts}
              ],
    ConnectionOpts = #{ transport => State#?MODULE.transport
                      , tls_opts => TLSOpts
                      },
    NewState = State#?MODULE{ tls_opts = TLSOpts
                          , connection_opts = ConnectionOpts
                          },
    init_tls_connection(Args, NewState).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_tls_connection(Args, State) ->
    case gun:open( State#?MODULE.host
                 , State#?MODULE.port
                 , State#?MODULE.connection_opts) of
        {ok, Connection} ->
            NewState = State#?MODULE{ connection = Connection },
            init_websocket_connection(Args, NewState);
        Elsewise -> 
            Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_websocket_connection(Args, State) ->
    Ref = gun:ws_upgrade( State#?MODULE.connection
                        , State#?MODULE.path
                        , State#?MODULE.websocket_opts),    
    NewState = State#?MODULE{ websocket = Ref
                            , arguments = Args
                            },
    init_final(Args, NewState).

init_final(_Args, State) ->
    Transaction = fun() -> mnesia:write(State) end,
    mnesia:transaction(Transaction),
    init_notify_pg(),
    {ok, connected, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_notify_pg() ->
    pg:join(nostr_client, ?MODULE, self()).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
init_connection(Args, State) ->
    {stop, {invalid, Args, State}}.

%%--------------------------------------------------------------------
%% @doc `terminate/2' function terminates the current running process.
%%
%% This function closes the Websocket and the HTTP connection to the
%% remote host.
%%
%% @see gen_server:terminate/2
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State, Data) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Data :: any(),
      Return :: ok.

terminate(Reason, connected, #?MODULE{ process = P, connection = Connection } = Data) ->
    ?LOG_INFO("~p", [{?MODULE, self(), terminate, Reason, Data}]),
    mnesia:transaction(fun() -> mnesia:delete(?MODULE, P, write) end),
    gun:shutdown(Connection),
    ok;
terminate(Reason, _, Data) -> 
    ?LOG_INFO("~p", [{?MODULE, self(), terminate, Reason, Data}]),
    ok.

%%--------------------------------------------------------------------
%% @doc `handle_cast/2' callback is used to send messages to the
%% relay.
%%
%% @see gen_server:handle_cast/2
%% @end
%%--------------------------------------------------------------------
-spec connected(EventType, Message, Data) -> Return when
      EventType :: cast | info,
      Message :: to_be_defined(),
      Data :: to_be_defined(),
      Return :: to_be_defined().

% the connection should be defined here
connected(enter, _, Data) ->
    {next_state, connected, Data};

% send a bitstring to the relay
connected(cast, {raw, Raw} = Message
           ,#?MODULE{ connection = Pid, websocket = Ref } = Data) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message}]),
    gun:ws_send(Pid, Ref, {text, Raw}),
    {keep_state, Data};

% get the complete state of the connection
connected({call, From} = EventType, {get, state} = Message, Data) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), EventType, Message}]),
    {keep_state, Data, [{reply, From, Data}]};

connected(info, {gun_ws,_Pid,_Ref,{text, Data}} = Message, Data) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message}]),
    % websocket_message_router(Data, Data),
    {keep_state, Data};

connected(info, {gun_ws,_Pid,_Ref,_Data} = Message, Data) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message}]),
    {keep_state, Data};

% gun module is telling us the websocket is now available
connected(info, {gun_upgrade,_Pid,_Ref,[<<"websocket">>],_Headers} = Message, Data) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message}]),
    {keep_state, Data#?MODULE{ websocket_link = true }};

% gun module is telling us the connection http is up
connected(info, {gun_up,_,http} = Message, Data) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message}]),
    {keep_state, Data#?MODULE{ http_link = true }};

% when an error occurs, just shutdown the connection
connected(info, {gun_error, _, _, _Reason}, Data) ->
    {stop, normal, Data};

% when the websocket is closed, we stop the process
connected(info, {gun_down, _, ws, closed, _} = _Message, Data) ->
    {stop, normal, Data};

% wildcard
connected(_, Message, Data) ->
    ?LOG_WARNING("~p", [{?MODULE, self(), info, Message}]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% `websocket_message_router/2' is an internal function used to routes
%% the websocket message to another process.
%%
%% @end
%%--------------------------------------------------------------------
%% -spec websocket_message_router(Message, State) -> Return when
%%       Message :: iodata(),
%%       State :: nostr_client_connection_state(),
%%       Return :: any().

%% websocket_message_router(Message, State) ->
%%     Host = proplists:get_value(host, State#?MODULE.arguments, undefined),
%%     nostr_client_router:raw_pool(Host, Message).
