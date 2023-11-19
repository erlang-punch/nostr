%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(nostr_relay_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([single/0, single/1, single_demo/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("nostrlib/include/nostrlib.hrl").
-spec suite() -> any().
-spec init_per_suite(any()) -> any().
-spec end_per_suite(any()) -> any().
-spec init_per_group(any(), any()) -> any().
-spec end_per_group(any(), any()) -> any().
-spec init_per_testcase(any(), any()) -> any().
-spec end_per_testcase(any(), any()) -> any().
-spec groups() -> any().
-spec all() -> any().

suite() -> [{timetrap,{seconds,30}}].
init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.
groups() -> [].
all() -> [single].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec single() -> [].
single() -> [].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec single(any()) -> ok.
single(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc 
%%
%% here we are testing to deploy one relay from scratch. by default a
%% relay is public and accept everything without restriction. It will
%% also only enable nip_01
%%
%% @end
%%--------------------------------------------------------------------
-spec single_demo(any()) -> ok.
single_demo(_Config) -> 
    % generate the key used by the relay. This is a demo key,
    % it should not be used in production...
    RelayPrivateKey = <<1:256>>,
    {ok, RelayPublicKey} = nostrlib_schnorr:new_publickey(RelayPrivateKey),

    % create a configuration, this is a partial nip-11 support.
    % the name of the server is used as default id
    Name = <<"nostr.localhost">>,

    Relay = #{ % mandatory: domain name used.
               domain         => <<"nostr.localhost">>
               % optional: default websocket path used
             , websocket_path => <<"/">>
               % optional: description of the server, will be available
               % in nip-11
             , description    => <<"A test relay for localhost users.">>
               % optional: public key used by this relay, will be available
               % in nip-11
             , public_key     => RelayPublicKey
               % optional: contact of the administrator, will be available
               % in nip-11
             , contact        => <<"nostr@localhost">>
               % optional: list of NIPs used by this relay. If not configured
               % only nip_01 is activated
             , nips           => [nip_01]
             },

    % extra parameters used for cowboy, tls and other applications.
    Opts = [],

    % create a new relay
    ok = nostr_relay:create_relay(Name, Relay, Opts),
    
    % list available relays
    _ = nostr_relay:list_relays(Relay),

    % get information about one relay
    _ = nostr_relay:get_relay(Relay),

    % start the new relay
    ok = nostr_relay:start_relay(<<"nostr.localhost">>),

    % get nostr_relay information
    _ = nostr_relay:info(),

    % stop the relay
    ok = nostr_relay:stop_relay(<<"nostr.localhost">>),
    
    % remove the relay from the configuration
    ok = nostr_relay:delete_relay(<<"nostr.localhost">>).

