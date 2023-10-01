%%%===================================================================
%%% Copyright 2023 Mathieu Kerjouan
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the “Software”), to deal in the Software without
%%% restriction, including without limitation the rights to use, copy,
%%% modify, merge, publish, distribute, sublicense, and/or sell copies
%%% of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(segwit_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([valid/0, valid/1]).
-export([invalid/0, invalid/1]).
-export([invalid_input/0, invalid_input/1]).
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

all() -> [valid, invalid, invalid_input].

%%--------------------------------------------------------------------
%% @doc check valid inputs. 
%% @see valid_fixture/0
%% @end
%%--------------------------------------------------------------------
-spec valid() -> any().
valid() -> [].

-spec valid(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().

valid(_Config) ->
    Test = fun (#{ version := Version, value := Value, hrp := HRP, address := Address } = Data) ->
                   ct:pal(info, "valid: ~p", [Data]),
                   Lower = string:lowercase(Address),
                   {ok, _} = segwit:decode(Lower),
                   {ok, _} = segwit:decode(list_to_binary(Lower)),
                   {ok, Lower} = segwit:encode(HRP, Version, Value),
                   {ok, Lower} = segwit:encode(list_to_binary(HRP), Version, list_to_binary(Value)),
                   {ok, #{ version := Version , value := Value, hrp := HRP } = Map }
                       = segwit:decode(HRP, Address),
                   {ok, Lower} = segwit:encode(Map)
           end,
    lists:map(Test, valid_data_fixture()).

%%--------------------------------------------------------------------
%% @doc check invalid segwit address.
%% @see invalid_fixture/0
%% @end
%%--------------------------------------------------------------------
-spec invalid() -> any().
invalid() -> [].

-spec invalid(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().

invalid(_Config) ->
    TestData = fun(#{ error := _Error, hrp := HRP, address := Address} = Data) ->
                       Ignore = maps:get(ignore, Data, false),
                       Return = segwit:decode(HRP, Address),
                       ct:pal(info, "invalid data: ~p", [{Data, Return}]),
                       {error, _} = Return,
                       case Ignore =:= true of
                           true -> ok;
                           false -> 
                               {error, _} = segwit:decode(Address),
                               ok
                       end
               end,
    lists:map(TestData, invalid_data_fixture()).

%%--------------------------------------------------------------------
%% @doc check invalid input.
%% @see invalid_input_fixture/0
%% @end
%%--------------------------------------------------------------------
-spec invalid_input() -> any().
invalid_input() -> [].

-spec invalid_input(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().

invalid_input(_Config) ->
    TestInput = fun(#{ error := _Error, hrp := HRP, length := Length, version := Version } = Input) ->
                        Data = [ 0 || _ <- lists:seq(1, Length) ],
                        Encoded = segwit:encode(HRP, Version, Data),
                        ct:pal(info, "invalid input: ~p", [{Input, Data, Encoded}]),
                        {error, _} = Encoded
                end,
    lists:map(TestInput, invalid_input_fixture()).

%%--------------------------------------------------------------------
%% @doc
%% source: https://github.com/sipa/bech32/blob/master/ref/python/tests.py#L88
%% @end
%%--------------------------------------------------------------------
valid_data_fixture() ->
    [#{ version => 0
      , value => [117, 30, 118, 232, 25, 145, 150, 212,
                  84, 148, 28, 69, 209, 179, 163, 35,
                  241, 67, 59, 214]
      , hrp => "bc"
      , address => "BC1QW508D6QEJXTDG4Y5R3ZARVARY0C5XW7KV8F3T4"
      }
    ,#{ version => 0
      , value => [24, 99, 20, 60, 20, 197, 22, 104, 4, 189, 25, 32,
                  51, 86, 218, 19, 108, 152, 86, 120, 205, 77, 39, 161,
                  184, 198, 50, 150, 4, 144, 50, 98]
      , hrp => "tb"
      , address => "tb1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q0sl5k7"
      }
    ,#{ version => 1
      , value => [117, 30, 118, 232, 25, 145, 150, 212, 84, 148, 28,
                  69, 209, 179, 163, 35, 241, 67, 59, 214, 117, 30, 118,
                  232, 25, 145, 150, 212, 84, 148, 28, 69, 209, 179,
                  163, 35, 241, 67, 59, 214]
      , hrp => "bc"
      , address => "bc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7kt5nd6y"
      }
    ,#{ version => 16
      , value => [117, 30]
      , hrp => "bc"
      , address => "BC1SW50QGDZ25J"
      }
    ,#{ version => 2
      , value => [117, 30, 118, 232,
                  25, 145, 150, 212,
                  84, 148, 28, 69, 209,
                  179, 163, 35]
      , hrp => "bc"
      , address => "bc1zw508d6qejxtdg4y5r3zarvaryvaxxpcs"
      }
    ,#{ version => 0
      , value => [0, 0, 0, 196, 165, 202, 212, 98, 33, 178, 161, 135,
                 144, 94, 82, 102, 54, 43, 153, 213, 233, 28, 108,
                 226, 77, 22, 93, 171, 147, 232, 100, 51]
      , hrp => "tb"
      , address => "tb1qqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesrxh6hy"
      }
    ,#{ version => 1
      , value => [0, 0, 0, 196, 165, 202, 212, 98, 33, 178, 161, 135,
                 144, 94, 82, 102, 54, 43, 153, 213, 233, 28, 108,
                 226, 77, 22, 93, 171, 147, 232, 100, 51]
      , hrp => "tb"
      , address => "tb1pqqqqp399et2xygdj5xreqhjjvcmzhxw4aywxecjdzew6hylgvsesf3hn0c"
      }
    ,#{ version => 1
      , value => [121, 190, 102, 126, 249, 220, 187, 172, 85, 160, 98,
                  149, 206, 135, 11, 7, 2, 155, 252, 219, 45, 206, 40,
                  217, 89, 242, 129, 91, 22, 248, 23, 152]
      , hrp => "bc"
      , address => "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
      }
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% source: https://github.com/sipa/bech32/blob/master/ref/python/tests.py#L104
%% @end
%%--------------------------------------------------------------------
invalid_data_fixture() ->
    [% Invalid HRP
     #{ error => [{reason, "Invalid hrp"}]
      , hrp => "bc"
      , address => "tc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq5zuyut"
      , ignore => true
      }

     % Invalid checksum algorithm (bech32 instead of bech32m)
    ,#{ error => [{reason, "Invalid version and format"}]
      , hrp => "bc"
      , address => "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqh2y7hd"
      }

     % Invalid checksum algorithm (bech32 instead of bech32m)
    ,#{ error => [{reason, "Invalid version and format"}]
      , hrp => "tb"
      , address => "tb1z0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqglt7rf"
      }

     % Invalid checksum algorithm (bech32 instead of bech32m)
    ,#{ error => [{reason, "Invalid version and format"}]
      , hrp => "bc"
      , address => "BC1S0XLXVLHEMJA6C4DQV22UAPCTQUPFHLXM9H8Z3K2E72Q4K9HCZ7VQ54WELL"
      }

     % Invalid checksum algorithm (bech32m instead of bech32)
    ,#{ error => [{reason, "Invalid version and format"}]
      , hrp => "bc"
      , address => "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kemeawh"
      }

     % Invalid checksum algorithm (bech32m instead of bech32)
    ,#{ error => [{reason, "Invalid version and format"}]
      , hrp => "tb"
      , address => "tb1q0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq24jc47"
      }

     % Invalid character in checksum
    ,#{ error => [{reason, "Invalid data character"}
                 ,{position, 60}]
      , hrp => "bc"
      , address => "bc1p38j9r5y49hruaue7wxjce0updqjuyyx0kh56v8s25huc6995vvpql3jow4"
      }

     % Invalid witness version
    ,#{ error => [{reason, "Invalid version"}]
      , hrp => "bc"
      , address => "BC130XLXVLHEMJA6C4DQV22UAPCTQUPFHLXM9H8Z3K2E72Q4K9HCZ7VQ7ZWS8R"
      }

     % Invalid program length (1 byte)
    ,#{ error => [{reason, "Invalid program length for witness version 0 (per BIP141)"}]
      , hrp => "bc"
      , address => "bc1pw5dgrnzv"
      }

     % Invalid program length (41 bytes)
    ,#{ error => [{reason, "Invalid program length for witness version 0 (per BIP141)"}]
      , hrp => "bc"
      , address => "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7v8n0nx0muaewav253zgeav"
      }

     % Invalid program length for witness version 0 (per BIP141)
    ,#{ error => [{reason, "Invalid data length"}]
      , hrp => "bc"
      , address => "BC1QR508D6QEJXTDG4Y5R3ZARVARYV98GJ9P"
      }

     % Mixed case
    ,#{ error => [{reason, "wrong case"}
                 ,{data, "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq47Zagq"}]
      , hrp => "tb"
      , address => "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq47Zagq"
      }

     % TOFIX: More than 4 padding bits
    ,#{ error => [{reason, ""}]
      , hrp => "bc"
      , address => "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7v07qwwzcrf"
      }

     % TOFIX: Non-zero padding in 8-to-5 conversion
    ,#{ error => [{reason, ""}]
      , hrp => "tb"
      , address => "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vpggkg4j"
      }

     % Empty data section
    ,#{ error => [{reason, "Empty data section"}]
      , hrp => "bc"
      , address => "bc1gmk9yu"
      }
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% source: https://github.com/sipa/bech32/blob/master/ref/python/tests.py#L137
%% @end
%%--------------------------------------------------------------------
%% TOFIX: check if the values are correct
invalid_input_fixture() ->
    [#{ hrp => "BC"
      , version => 0
      , length => 19
      , error => [{reason, ""}]
      }
    ,#{ hrp => "bc"
      , version => 0
      , length => 22
      , error => [{reason, ""}]
      }
    ,#{ hrp => "bc"
      , version => 17
      , length => 31
      , error => [{reason, ""}]
      }
    ,#{ hrp => "bc"
      , version => 1
      , length => 1
      , error => [{reason, ""}]
      }
    ,#{ hrp => "bc"
      , version => 16
      , length => 41
      , error => [{reason, ""}]
      }
    ].
