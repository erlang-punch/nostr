%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2023 Erlang Punch
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(websocket_server_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([single/0, single/1]).
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
    % ensure mnesia, cowboy and gun are correctly started
    application:ensure_all_started(mnesia),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),

    % init mnesia table
    websocket_server_nip01:start(),

    % start a local server
    {ok, _Server} = websocket_server:start("localhost", 23921),
    
    % let start a client as well
    {ok, Client} = websocket_client:start("ws://localhost:23921"),
    
    % craft an event
    {ok, Event} = nostrlib:encode(#event{ content = <<"test">>
                                        , kind = text_note } 
                                 ,[{private_key, <<1:256>>}]),
    {ok, _Raw, _} = nostrlib:decode(Event),
    
    % send this event to the server
    ok = websocket_client:send(Client, binary_to_list(Event)),

    % just way a moment... That's a hack, the client return something
    % if the message was correctly received.
    timer:sleep(100).

    % @TODO fix this part of the code with new version
    % now we can check if this event is present in the database.
    % {ok, _} = websocket_server_nip01:get_event(Raw),
    % true = websocket_server_nip01:exist_event(Raw),
    % {atomic, _} = websocket_server_nip01:list_events().

