%%%===================================================================
%%% @doc
%%%
%%% `nostr' Erlang application. This application has been created to
%%% offer a fully complient implementation of the nostr protocol in
%%% Erlang with OTP. This implementation includes a client and a relay
%%% and can be used in these two modes at the same time.
%%%
%%% == Introduction ==
%%%
%%% TODO
%%%
%%% === Library ===
%%%
%%% A library called `nostrlib' is available with all primitives, and
%%% data-structures used to create and validate nostr data.
%%%
%%% === Client ===
%%%
%%% A client application called `nostr_client' is available to managed
%%% a client connection to a relay with all features needed features
%%% to communicate and share information with it.
%%%
%%% Different kind of interfaces will be available to control this
%%% client and manage the users.
%%%
%%% === Relay ===
%%%
%%% A relay application called `nostr_relay' is available to create a
%%% relay server allowed client to communicate together.
%%%
%%% == Definitions and Terms Used ==
%%%
%%% TODO
%%%
%%% == Conventions ==
%%%
%%% TODO
%%%
%%% == Usage ==
%%%
%%% ```
%%% nostr_client:list().
%%% nostr_client:connect(ClientId).
%%% nostr_client:subscribe(ClientId, Filter).
%%% nostr_client:unsubscribe(ClientId, SubscribtionId).
%%% nostr_client:send(ClientId, short_text_note, <<"my message">>).
%%% nostr_client:send(ClientId, reaction, MessageId, like).
%%% nostr_client:send(ClientId, reaction, MessageId, dislike).
%%% nostr_client:disconnect(ClientId).
%%% '''
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(Type, Args) -> Return when
      Type :: any(),
      Args :: any(),
      Return :: supervisor:startlink_ret().
start(_StartType, _StartArgs) ->
    public_key:cacerts_load(),
    nostr_sup:start_link().

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> Return when
      State :: any(),
      Return :: ok.
stop(_State) ->
    ok.
