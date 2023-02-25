%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-module(nostrlib_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-include_lib("common_test/include/ct.hrl").
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

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, _Config) -> ok.

init_per_testcase(_TestCase, Config) -> Config.

end_per_testcase(_TestCase, _Config) -> ok.

groups() -> [].

all() -> [].
