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
%%% == Client Usage ==
%%%
%%% Client connection management.
%%%
%%% ```
%%% % connect to a server
%%% nostr_client:connect(<<"wss://nostr.com">>).
%%% nostr_client:connect(<<"wss://nostr.com">>, []).
%%% nostr_client:connect(<<"ws://nostr.com">>, []).
%%% nostr_client:disconnect(<<"wss://nostr.com">>).
%%% '''
%%%
%%% Identity management.
%%%
%%% ```
%%% % create a new identity
%%% Identifier = <<"testuser">>.
%%% IdentityOpts = [{name, <<>>}, {about, <<"">>}
%%%                ,{picture, <<>>}, {server, <<>>}].
%%% nostr_client:new_identity(Identifier).
%%% nostr_client:new_identity(Identifier, IdentityOpts).
%%%
%%% % list identity
%%% [Identity|_] = nostr_client:list_identity().
%%% '''
%%%
%%% Subscription management.
%%%
%%% ```
%%% % create a new subscription
%%% Relay = <<"wss://nostr.com">>.
%%% Filter = #filter{}.
%%% {ok, SubId} = nostr_client:new_subscription(Relay, Filter).
%%% {ok, [_|_]} = nostr_client:list_subscription().
%%% {ok, [_|_]} = nostr_client:list_subscription(Relay).
%%% ok = nostr_client:delete_subscription(SubId).
%%% '''
%%%
%%% Contact management.
%%%
%%% ```
%%% % create a new contact_list
%%% nostr_client:add_contact(Identity, PubKey).
%%% nostr_client:add_contact(Identity, PubKey, []).
%%% nostr_client:delete_contact(Identity, PubKey).
%%% nostr_client:publish_contact(Identity).
%%% nostr_client:publish_contact(Identity, Relay).
%%% nostr_client:import_contact(Identity, Relay).
%%% '''
%%%
%%% Event management.
%%%
%%% ```
%%% {ok, TextNote} = nostr_client:new_event(text_note, Identity, <<"note">>).
%%% {ok, _} = nostr_client:publish_event(TextNote, Relay).
%%% {ok, [_|_]} = nostr_client:publish_event(Text_event, [Relay1, Relay2]).
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

    % load certificates from default system path
    public_key:cacerts_load(),

    % start mnesia and initialize the tables
    nostr_mnesia:start(),

    % start main nostr supervisor
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
