%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc nostr_client module is the main interface to communicate with
%%% all the nostr Erlang client application. It will offer all
%%% important function to be used by anyone.
%%%
%%% == Examples ==
%%%
%%% ```
%%% % load records
%%% rr(nostrlib).
%%%
%%% % set some variables
%%% Host = "relay.nostrich.de".
%%% Filter = #filter{ limit = 1 }.
%%%
%%% % create a new connection
%%% {ok, Connection} = nostr_client:connect(Host).
%%%
%%% % create a new subscription
%%% {ok, Subscription} = nostr_client:request(Host, Filter).
%%%
%%% % close the current active connection
%%% ok = nostr_client:close(Host, Subscription).
%%%
%%% % send and event
%%% Opts = [{private_key, PrivateKey}].
%%% ok = nostr_client:event(Host, text_note, <<"hello">>, Opts).
%%% '''
%%%
%%% @todo replace host by another ID.
%%% @end
%%%===================================================================
-module(nostr_client).
-export([connect/1, connect/2]).
-export([event/4]).
-export([request/2, request/3]).
-export([close/2, close/3]).
% -export([add_contact/2, add_contact/3, del_contact/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").

% @TODO document types
-type host() :: bitstring() | string().
-type options() :: proplists:proplists().

%%--------------------------------------------------------------------
%% required for eunit
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc connect/1 creates a new nostr_client connection to a remote
%%      relay with default options. see connect/2.
%% @end
%%--------------------------------------------------------------------
-spec connect(Host) -> Return when
      Host :: host(),
      Return :: any(). % TODO: check the return function of gun module.

connect(Host) ->
    DefaultOptions = [],
    connect(Host, DefaultOptions).

%%--------------------------------------------------------------------
%% @doc connect/2 creates a new nostr client connection to a remote
%%      relay with options defined by the users
%% @end
%%--------------------------------------------------------------------
-spec connect(Host, Options) -> Return when
      Host :: host(),
      Options :: options(),
      Return :: any(). % TODO: check the return function of gun module.

connect(Host, Options) ->
    nostr_manager_client_sup:start_client_sup([{host, Host}, {options, Options}]).

%%--------------------------------------------------------------------
%% @doc `event/4' function send an event to an active connection.
%% @end
%%--------------------------------------------------------------------
-spec event(Host, Kind, Content, Opts) -> Return when
      Host :: string(),
      Kind :: atom(),
      Content :: bitstring(),
      Opts :: proplists:proplists(),
      Return :: ok.

event(Host, set_metadata, Content, Opts)
  when is_map(Content) ->
    case get_process(Host, connection) of
        {ok, Connection} ->
            Metadata = thoas:encode(Content),
            Event = #event{ kind = set_metadata, content = Metadata },
            {ok, Payload} = nostrlib:encode(Event, Opts),
            nostr_client_connection:send_raw(Connection, Payload);
        Elsewise -> Elsewise
    end;
event(Host, text_note, Content, Opts) 
  when is_binary(Content) ->
    case get_process(Host, connection) of
        {ok, Connection} ->
            Event = #event{ kind = text_note, content = Content},
            {ok, Payload} = nostrlib:encode(Event, Opts),
            nostr_client_connection:send_raw(Connection, Payload);
        Elsewise -> Elsewise
    end;
event(Host, recommend_server, Content, Opts) ->
    case nostrlib_url:check(Content, Opts) of
        {ok, Url} ->
            case get_process(Host, connection) of
                {ok, Connection} ->
                    Event = #event{ kind = recommend_server, content = Url },
                    {ok, Payload} = nostrlib:encode(Event, Opts),
                    nostr_client_connection:send_raw(Connection, Payload);
                Elsewise -> Elsewise
            end;
        Elsewise -> Elsewise
    end;
event(Host, contact_list, Tags, Opts)
  when is_list(Tags) ->
    case get_process(Host, connection) of
        {ok, Connection} ->
            Event = #event{ kind = contact_list, content = <<>>, tags = Tags },
            {ok, Payload} = nostrlib:encode(Event, Opts),
            nostr_client_connection:send_raw(Connection, Payload);
        Elsewise -> Elsewise
    end;
event(_,Kind,_,_) ->
    {error, [{kind, Kind},{message, unsupported}]}.

%%--------------------------------------------------------------------
%% @doc `request/2'
%%
%% @see request/3
%% @end
%%--------------------------------------------------------------------
-spec request(Host, Filter) -> Return when
      Host :: string(),
      Filter :: decoded_filter(),
      Return :: {ok, bitstring()}.

request(Host, Filter) ->
    request(Host, Filter, []).

%%--------------------------------------------------------------------
%% @doc `request/3' function send a request to an active connection.
%% @end
%%--------------------------------------------------------------------
-spec request(Host, Filter, Opts) -> Return when
      Host :: string(),
      Filter :: decoded_filter(),
      Opts :: proplists:proplists(),
      Return :: {ok, bitstring()}.

request(Host, Filter, Opts) ->
    case get_process(Host, connection) of
        {ok, Connection} ->
            SubscriptionId = nostrlib:new_subscription_id(),
            Request = #request{ subscription_id = SubscriptionId
                                    , filter = Filter },
            {ok, Payload} = nostrlib:encode(Request, Opts),
            ok = nostr_client_connection:send_raw(Connection, Payload),
            {ok, SubscriptionId};
        Elsewise -> Elsewise
    end.
              
%%--------------------------------------------------------------------
%% @doc `close/2'
%%
%% @see close/3
%% @end
%%--------------------------------------------------------------------
-spec close(Host, SubscriptionId) -> Return when
      Host :: string(),
      SubscriptionId :: binary(),
      Return :: ok.

close(Host, SubscriptionId) ->
    close(Host, SubscriptionId, []).

%%--------------------------------------------------------------------
%% @doc `close/3' function closes an active subscription.
%%
%% @end
%%--------------------------------------------------------------------
-spec close(Host, SubscriptionId, Opts) -> Return when
      Host :: string(),
      SubscriptionId :: iodata(),
      Opts :: proplists:proplists(),
      Return :: ok.

close(Host, SubscriptionId, Opts) ->
    case get_process(Host, connection) of
        {ok, Connection} ->
            Close = #close{ subscription_id = SubscriptionId },
            {ok, Payload} = nostrlib:encode(Close, Opts),
            nostr_client_connection:send_raw(Connection, Payload);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
get_process(Host, Identifier) 
  when is_atom(Identifier) ->
    case pg:get_members(client, {Host, Identifier}) of
        [] -> {error, [{host, Host}, {connection, not_connected}]};
        [Process] -> {ok, Process};
        [Process|_] -> {ok, Process}
    end.
