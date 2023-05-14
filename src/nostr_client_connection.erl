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
%%% @todo convert this module from `gen_server' to `gen_statem'
%%% @todo create more examples
%%% @end
%%%===================================================================
-module(nostr_client_connection).
-vsn("0.0.1").
-behaviour(gen_server).
-export([start/1, start/2]).
-export([start_link/1, start_link/2]).
-export([stop/1]).
-export([send_raw/2, send_json/2]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, { connection     = undefined
               , websocket      = undefined
               , subscriptions  = #{}
               , arguments      = []
               , http_link      = false
               , websocket_link = false
               }).
-type nostr_client_connection_state() :: #state{}.

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
    gen_server:start(?MODULE, Args, Opts).

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
    gen_server:start_link(?MODULE, Args, Opts).

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
    gen_server:stop(Pid).

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
    gen_server:cast(Pid, {raw, RawMessage}).

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
    % @TODO remove debug mode
    logger:set_module_level(?MODULE, debug),
    Host = proplists:get_value(host, Args, undefined),
    Port = proplists:get_value(port, Args, 443),
    Transport = proplists:get_value(transport, Args, tls),
    Path = proplists:get_value(path, Args, "/"),
    WebSocketOptions = proplists:get_value(websocket_opts, Args, []),

    case proplists:get_value(tls, Args, true) of
        true ->
            CACerts = proplists:get_value(cacerts, Args, public_key:cacerts_get()),
            TLSOpts = [ {verify, verify_peer}
                      , {cacerts, CACerts}
                      ],
            ConnectionOpts = #{ transport => Transport
                              , tls_opts => TLSOpts
                      },
            {ok, Connection} = gun:open(Host, Port, ConnectionOpts),
            Ref = gun:ws_upgrade(Connection, Path, WebSocketOptions),

            % @TODO the id must be defined with something different
            pg:join(client, {Host, connection}, self()),

            State = #state{ connection = Connection
                          , websocket = Ref
                          , arguments = Args
                          },
            ?LOG_INFO("~p", [{?MODULE, self(), init, Args, State}]),
            {ok, State};
        false ->
            ConnectionOpts = #{},
            {ok, Connection} = gun:open(Host, Port, ConnectionOpts),
            Ref = gun:ws_upgrade(Connection, Path, WebSocketOptions),

            % @TODO the id must be defined with something different
            pg:join(client, {Host, connection}, self()),

            State = #state{ connection = Connection
                          , websocket = Ref
                          , arguments = Args
                          },
            ?LOG_INFO("~p", [{?MODULE, self(), init, Args, State}]),
            {ok, State}
        end.

%%--------------------------------------------------------------------
%% @doc `terminate/2' function terminates the current running process.
%%
%% This function closes the Websocket and the HTTP connection to the
%% remote host.
%%
%% @see gen_server:terminate/2
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason, State) -> Return when
      Reason :: term(),
      State :: to_be_defined(),
      Return :: ok.

terminate(Reason, #state{connection = Connection} = State) ->
    ?LOG_INFO("~p", [{?MODULE, self(), terminate, Reason, State}]),
    gun:shutdown(Connection),
    ok.

%%--------------------------------------------------------------------
%% @doc `handle_cast/2' callback is used to send messages to the
%% relay.
%%
%% @see gen_server:handle_cast/2
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

% send a bitstring to the relay
handle_cast({raw, Data} = Message
           ,#state{ connection = Pid, websocket = Ref } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message, State}]),
    gun:ws_send(Pid, Ref, {text, Data}),
    {noreply, State};

% wildcard function to remove unknown messages and cleanup the mailbox
handle_cast(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), cast, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc `handle_call/3' function is not used yet.
%%
%% @see gen_server:handle_call/3
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

%%--------------------------------------------------------------------
%% @doc `handle_info/2' is a callback receiving messages coming from
%% the `gun' websocket.
%%
%% At this time, only websocket messages are supported.
%%
%% @see gen_server:handle_info/2
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) -> Return when
      Message :: to_be_defined(),
      State :: to_be_defined(),
      Return :: to_be_defined().

% gun forward us all its messages from the connection
handle_info({gun_ws,_Pid,_Ref,{text, Data}} = Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    websocket_message_router(Data, State),
    {noreply, State};

% websocket wilcard
handle_info({gun_ws,_Pid,_Ref,_Data} = Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State};

% gun module is telling us the websocket is now available
handle_info({gun_upgrade,_Pid,_Ref,[<<"websocket">>],_Headers} = Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State#state{ websocket_link = true }};

% gun module is telling us the connection http is up
handle_info({gun_up,_,http} = Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State#state{ http_link = true }};

% wildcard
handle_info(Message, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), info, Message, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% `websocket_message_router/2' is an internal function used to routes
%% the websocket message to another process.
%%
%% @end
%%--------------------------------------------------------------------
-spec websocket_message_router(Message, State) -> Return when
      Message :: iodata(),
      State :: nostr_client_connection_state(),
      Return :: any().

websocket_message_router(Message, State) ->
    Host = proplists:get_value(host, State#state.arguments, undefined),
    nostr_client_router:raw_pool(Host, Message).
