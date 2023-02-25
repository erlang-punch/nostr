%%%===================================================================
%%% @doc nostr_client module is the main interface to communicate with
%%% all the nostr Erlang client application. It will offer all
%%% important function to be used by anyone.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_client).
-export([connect/1, connect/2]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").

% @TODO document types
-type host() :: bitstring() | string().
-type options() :: proplists:proplists().

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
    nostr_client_connection:start([{host, Host}, {options, Options}]).
