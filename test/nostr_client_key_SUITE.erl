%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan <contact at erlang-punch dot com>
%%% @doc
%%% @ed
%%%-------------------------------------------------------------------
-module(nostr_client_key_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([simple/0, simple/1]).
-include_lib("common_test/include/ct.hrl").
-include("nostrlib.hrl").
-spec suite() -> any().
-spec init_per_suite(any()) -> any().
-spec end_per_suite(any()) -> any().
-spec init_per_group(any(), any()) -> any().
-spec end_per_group(any(), any()) -> any().
-spec init_per_testcase(any(), any()) -> any().
-spec end_per_testcase(any(), any()) -> any().
-spec groups() -> any().
-spec all() -> any().

suite() -> [{timetrap,{minutes,10}}].

init_per_suite(Config) -> 
    application:ensure_all_started(nostr),
    Config.

end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, _Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

groups() -> [].

all() -> [simple].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec simple() -> list().
simple() -> [].

-spec simple(any()) -> any().
simple(_Config) ->
    % modify the home temporarily
    _C = temporary_home(),

    % start a new key store process
    {ok, Pid} = nostr_client_key:start([{name, <<"test">>}]),
    {ok, PrivateKey} = nostr_client_key:private_key(Pid),
    {ok, PublicKey} = nostr_client_key:public_key(Pid),

    % set metadata name
    ok = nostr_client_key:set_metadata(Pid, name, <<"test_name">>),
    {ok, <<"test_name">>} = nostr_client_key:get_metadata(Pid, name),

    % set metadata about
    ok = nostr_client_key:set_metadata(Pid, about, <<"test_about">>),
    {ok, <<"test_about">>} = nostr_client_key:get_metadata(Pid, about),

    % set metadata picture
    ok = nostr_client_key:set_metadata(Pid, picture, <<"test_picture">>),
    {ok, <<"test_picture">>} = nostr_client_key:get_metadata(Pid, picture),

    % start a new process with the same name
    {ok, Pid2} = nostr_client_key:start([{name, <<"test">>}]),

    % the keys must be identicals.
    {ok, PrivateKey} = nostr_client_key:private_key(Pid2),
    {ok, PublicKey} = nostr_client_key:public_key(Pid2),

    % cleanup
    gen_server:stop(Pid),
    gen_server:stop(Pid2).
    

%%--------------------------------------------------------------------
%% @doc internal. Modify the home directory with a random one.
%% @end
%%--------------------------------------------------------------------
temporary_home() ->
    OldHome = os:getenv("HOME"),
    <<Random:(12*8)>> = crypto:strong_rand_bytes(12),
    Name = erlang:integer_to_list(Random, 32),
    DirectoryPath = filename:join("/tmp", Name),
    ok = file:make_dir(DirectoryPath),
    os:putenv("HOME", DirectoryPath),
    [{old_home, OldHome}, {new_home, DirectoryPath}].
