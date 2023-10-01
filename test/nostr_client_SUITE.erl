%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch dot com>
%%% @doc
%%% @end
%%%===================================================================
-module(nostr_client_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([connection_simple/0, connection_simple/1]).
-export([connection_with_options/0, connection_with_options/1]).
-export([contact_list/0, contact_list/1]).
-include_lib("common_test/include/ct.hrl").
-include_lib("nostr/include/nostrlib.hrl").
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

all() -> [connection_simple
         ,connection_with_options
         ,contact_list
         ].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec connection_simple() -> list().
connection_simple() -> [].

-spec connection_simple(any()) -> any().
connection_simple(_Config) ->
    % {ok, Reference} = nostr_client:connect("localhost"),
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec connection_with_options() -> list().
connection_with_options() -> [].

-spec connection_with_options(any()) -> any().
connection_with_options(_Config) ->
    % Options = [{port, 443}, {tls, []}],
    % {ok, Reference} = nostr_client:connect("localhost", Options),
    ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec contact_list() -> list().
contact_list() -> [].

-spec contact_list(any()) -> any().
contact_list(_Config) ->
    {ok, Pid} = nostr_client_contacts:start([{host, test}]),

    % add a new public key
    ok = nostr_client_contacts:add(Pid, <<1:256>>),

    % add a new public key with a name
    ok = nostr_client_contacts:add(Pid, <<2:256>>, [{name, <<"two">>}]),

    % add a new public key with a name and a relay
    ok = nostr_client_contacts:add(Pid, <<3:256>>, [{name, <<"three">>}
                                                   ,{relay, <<"wss://three.local">>}]),

    % list all contacts
    List = nostr_client_contacts:list(Pid),
    [{<<1:256>>, <<>>, <<>>}
    ,{<<2:256>>, <<"two">>, <<>>}
    ,{<<3:256>>, <<"three">>, <<"wss://three.local">>}]
        = lists:sort(List),

    % delete a public ey
    ok = nostr_client_contacts:delete(Pid, <<1:256>>),
    List2 = nostr_client_contacts:list(Pid),
    [{<<2:256>>, <<"two">>, <<>>}
    ,{<<3:256>>, <<"three">>, <<"wss://three.local">>}]
        = lists:sort(List2),

    % export the contacts
    #event{} = nostr_client_contacts:export(Pid).
