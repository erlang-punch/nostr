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
%%% @author Mathieu Kerjouan aka Niamtokik
%%% @author Maartz
%%% @doc
%%%
%%% `bech32' module implements bech32 format from bitcoin in pure
%%% Erlang. One of the goal of this module is to reimplement python <a
%%% href="https://github.com/sipa/bech32/blob/master/ref/python/segwit_addr.py">`segwit_addr'</a>
%%% module in Erlang.
%%%
%%% == Encoding Usage ==
%%%
%%% 4 functions are available to encode data. `encode_bech32/2' and
%%% `encode_bech32m/2' automatically encode data in the right format
%%% using indexed data. In case of issue, these functions raise an
%%% exception. `encode/2' and `encode/3' can accept many options and
%%% will use ok/error patterns. Here some example:
%%%
%%% ```
%%% % Define an HRP
%%% HRP = "test".
%%%
%%% % Define an indexed string using base32. This data can be
%%% % generated using convertbits function.
%%% IndexedData = [0,1,2,3,4].
%%%
%%% % Define an unindexed string (a raw one)
%%% UnindexedData = [$n,$o,$s,$t,$r].
%%% UnindexedData = "nostr".
%%%
%%% % bech32 encoding using bech32 format with indexed string.
%%% "test1qpzryyr0hjg" 
%%%    = bech32:encode_bech32(HRP, IndexedData).
%%% {ok, "test1qpzryyr0hjg"} 
%%%    = bech32:encode(HRP, IndexedData, [{format, bech32}]}).
%%%
%%% % bech32 encoding using bech32m format with indexed string.
%%% "test1qpzry3llmh2" = bech32:encode_bech32m(HRP, IndexedData).
%%% {ok, "test1qqqsyqcyzsv7qk"} 
%%%    = bech32:encode(HRP, IndexedData, [{format, bech32m}]}).
%%%
%%% % bech32 encoding using unindexed data
%%% {ok, "test1dehhxarjyzxdzp"} 
%%%    = bech32:encode(HRP, UnindexedData, [{format, bech32}, {indexed, false}]).
%%%
%%% % bech32 encoding using binary or bitstring as input
%%% "test1qqqsyqcyzsv7qk" 
%%%    = bech32:encode(<<"test">>, <<0,1,2,3,4>>, [{format, bech32m}]).
%%% {ok,"test1qpzry3llmh2"} 
%%%    = bech32:encode(<<"test">>, <<0,1,2,3,4>>, [{format, bech32m}]).
%%%
%%% % bech32 encoding with binary output
%%% {ok, "test1qpzry3llmh2"} 
%%%    = bech32:encode(HRP, IndexedData, [{format, bech32m}, {as_binary, true}]).
%%% '''
%%%
%%% == Decoding Usage ==
%%%
%%% Only one function is available to decode bech32 string:
%%% `decode/1'. Here few example.
%%%
%%% ```
%%% % decode a string as list()
%%% {ok, #{ checksum => [26,30,20,18,15,4]
%%%       , data     => [31,28]
%%%       , format   => bech32m
%%%       , hrp      => "test"
%%%       , origin   => "test1lu675j0y"
%%%       }
%%% } = bech32:decode("test1lu675j0y").
%%%
%%% % decode a string as binary()
%%% {ok, #{ checksum => [26,30,20,18,15,4]
%%%       , data     => [31,28]
%%%       , format   => bech32m
%%%       , hrp      => "test"
%%%       , origin   => "test1lu675j0y"
%%%       }
%%% } = bech32:decode(<<"test1lu675j0y">>)
%%% '''
%%%
%%% == Convert bits ==
%%%
%%% Functions to convert bits from different base called
%%% `convertbits/3' and `convertbits/4' are also provided.
%%%
%%% ```
%%% {ok,[14,17,18,23,6,29,0]}
%%%    = bech32:convertbits("test", 8, 5).
%%%
%%% {ok, [116,101,115,116,0]}
%%%    = bech32:convertbits([14,17,18,23,6,29,0], 5, 8).
%%%
%%% {ok,[14,17,18,23,6,29]}
%%%    = bech32:convertbits("test", 8, 5, [{padding, false}]).
%%%
%%% {ok,"test"}
%%%    = bech32:convertbits([14,17,18,23,6,29], 5, 8, [{padding, true}]).
%%% '''
%%%
%%% @todo add debug mode
%%% @todo creates errors (and specifies them)
%%% @todo creates types and specification.
%%% @end
%%%===================================================================
-module(bech32).
-export([encode/2, encode/3]).
-export([encode_bech32/2, encode_bech32m/2]).
-export([decode/1, decode/2]).
-export([create_checksum/3, verify_checksum/2]).
-export([convertbits/3, convertbits/4]).
-include_lib("eunit/include/eunit.hrl").
-include("bech32.hrl").

% charset() type defines all bech32 charset allowed values.
-type charset() :: $0 | $2 | $3 | $4 | $5 | $6 | $7 | $8 | $9 | $a
                 | $c | $d | $e | $f | $g | $h | $j | $k | $l | $m
                 | $n | $p | $q | $r | $s | $t | $u | $v | $w | $x
                 | $y | $z.

% charset_index() type define the index from 0 to 31 (32 values).
-type charset_index() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
                       | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19
                       | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28
                       | 29 | 30 | 31.

% local record
-record(bech32, { hrp      = undefined :: undefined | [integer()]
                , data     = undefined :: undefined | [integer()]
                , checksum = undefined :: undefined | [integer()]
                , format   = undefined :: format()
                , origin   = undefined :: undefined | [integer()]
                }).

% define the bech32m constant used for the checksum/format.
-define(BECH32M_CONST, 16#2BC8_30A3).

%%--------------------------------------------------------------------
%% This macro is used to generate the different steps used in polymod
%% function and simulate a kind of fixed size loop between each steps.
%%--------------------------------------------------------------------
-define(POLYMOD_GENERATOR(INDEX, GENERATOR),
    polymod_generator(INDEX, Checksum, Top) ->
        case (Top bsr INDEX) band 1 of
            0 ->
                polymod_generator(INDEX+1, Checksum bxor 0, Top);
            _Value ->
                polymod_generator(INDEX+1, Checksum bxor GENERATOR, Top)
        end
    ).

% required by eunit
-spec test() -> any().

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. Convert an index into a chart present in charset.
%%
%% ```
%% % generate with:
%% Index = lists:seq(0,31),
%% Charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l",
%% [ io:format("charset(~p) -> $~c;~n", [I, L])
%%   || {I, L} <- lists:zip(Index, Charset)
%% ].
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec index_to_charset(Index) -> Char when
      Index :: charset_index(),
      Char :: charset().

index_to_charset(0)  -> $q;
index_to_charset(1)  -> $p;
index_to_charset(2)  -> $z;
index_to_charset(3)  -> $r;
index_to_charset(4)  -> $y;
index_to_charset(5)  -> $9;
index_to_charset(6)  -> $x;
index_to_charset(7)  -> $8;
index_to_charset(8)  -> $g;
index_to_charset(9)  -> $f;
index_to_charset(10) -> $2;
index_to_charset(11) -> $t;
index_to_charset(12) -> $v;
index_to_charset(13) -> $d;
index_to_charset(14) -> $w;
index_to_charset(15) -> $0;
index_to_charset(16) -> $s;
index_to_charset(17) -> $3;
index_to_charset(18) -> $j;
index_to_charset(19) -> $n;
index_to_charset(20) -> $5;
index_to_charset(21) -> $4;
index_to_charset(22) -> $k;
index_to_charset(23) -> $h;
index_to_charset(24) -> $c;
index_to_charset(25) -> $e;
index_to_charset(26) -> $6;
index_to_charset(27) -> $m;
index_to_charset(28) -> $u;
index_to_charset(29) -> $a;
index_to_charset(30) -> $7;
index_to_charset(31) -> $l.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. convert charset value to index.
%% @end
%%--------------------------------------------------------------------
-spec charset_to_index(Char) -> Index when
      Char :: charset(),
      Index :: charset_index().

charset_to_index($q) -> 0;
charset_to_index($p) -> 1;
charset_to_index($z) -> 2;
charset_to_index($r) -> 3;
charset_to_index($y) -> 4;
charset_to_index($9) -> 5;
charset_to_index($x) -> 6;
charset_to_index($8) -> 7;
charset_to_index($g) -> 8;
charset_to_index($f) -> 9;
charset_to_index($2) -> 10;
charset_to_index($t) -> 11;
charset_to_index($v) -> 12;
charset_to_index($d) -> 13;
charset_to_index($w) -> 14;
charset_to_index($0) -> 15;
charset_to_index($s) -> 16;
charset_to_index($3) -> 17;
charset_to_index($j) -> 18;
charset_to_index($n) -> 19;
charset_to_index($5) -> 20;
charset_to_index($4) -> 21;
charset_to_index($k) -> 22;
charset_to_index($h) -> 23;
charset_to_index($c) -> 24;
charset_to_index($e) -> 25;
charset_to_index($6) -> 26;
charset_to_index($m) -> 27;
charset_to_index($u) -> 28;
charset_to_index($a) -> 29;
charset_to_index($7) -> 30;
charset_to_index($l) -> 31.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. check if a character is present in charset or not.
%% @end
%%--------------------------------------------------------------------
-spec is_charset(Char) -> Return when
      Char :: charset(),
      Return :: boolean().

is_charset($0) -> true;
is_charset($2) -> true;
is_charset($3) -> true;
is_charset($4) -> true;
is_charset($5) -> true;
is_charset($6) -> true;
is_charset($7) -> true;
is_charset($8) -> true;
is_charset($9) -> true;
is_charset($a) -> true;
is_charset($c) -> true;
is_charset($d) -> true;
is_charset($e) -> true;
is_charset($f) -> true;
is_charset($g) -> true;
is_charset($h) -> true;
is_charset($j) -> true;
is_charset($k) -> true;
is_charset($l) -> true;
is_charset($m) -> true;
is_charset($n) -> true;
is_charset($p) -> true;
is_charset($q) -> true;
is_charset($r) -> true;
is_charset($s) -> true;
is_charset($t) -> true;
is_charset($u) -> true;
is_charset($v) -> true;
is_charset($w) -> true;
is_charset($x) -> true;
is_charset($y) -> true;
is_charset($z) -> true;
is_charset(_)  -> false.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. check if a string is indexed or not.
%% @end
%%--------------------------------------------------------------------
is_index(C) when C >= 0 andalso C < 32 -> true;
is_index(_) -> false.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. check if its' a valid bech32 string.
%% @end
%%--------------------------------------------------------------------
valid_indexed_string(List)
  when is_list(List) ->
    valid_indexed_list(List, [], 0).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
valid_indexed_list([], Buffer, _Position) ->
    {ok, lists:reverse(Buffer)};
valid_indexed_list([H|T], Buffer, Position) ->
    case is_index(H) of
        true -> valid_indexed_list(T, [H|Buffer], Position+1);
        false -> {error, [{reason, "Invalid char"}
                         ,{char, [H]}
                         ,{position, Position}
                         ,{head, Buffer}
                         ,{rest, T}]}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. check bech32 data strings.
%% @end
%%--------------------------------------------------------------------
-spec valid_string(String) -> Return when
      String :: [integer()] | iodata(),
      Return :: {ok, [integer()] | iodata()} | {error, Reason},
      Reason :: proplists:proplist().

valid_string(List)
  when is_list(List) ->
    valid_charset_list(List, [], 0);
valid_string(Binary)
  when is_binary(Binary) ->
    String = binary_to_list(Binary),
    valid_string(String);
valid_string(_) ->
    {error, [{reason, "Unsupported data type"}]}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
valid_charset_list([], Buffer, _Position) ->
    {ok, lists:reverse(Buffer)};
valid_charset_list([H|T], Buffer, Position) ->
    case is_charset(H) of
        true -> valid_charset_list(T, [H|Buffer], Position+1);
        false -> {error, [{reason, "Invalid char"}
                         ,{char, [H]}
                         ,{position, Position}
                         ,{head, Buffer}
                         ,{rest, T}]}
    end.

% @hidden
-spec valid_string_test() -> any().
valid_string_test() ->
    [?assertEqual({ok, "a"}
                 , valid_string("a"))
    ,?assertEqual({error, [{reason,"Invalid char"},{char,"b"},{position,1},{head,"a"},{rest,[]}]}
                 , valid_string("ab"))
    ,?assertEqual({ok, "a"}
                 , valid_string(<<"a">>))
    ,?assertEqual({error, [{reason,"Invalid char"},{char,"b"},{position,1},{head,"a"},{rest,[]}]}
                 , valid_string(<<"ab">>))
    ,?assertEqual({error, [{reason, "Unsupported data type"}]}
                 , valid_string(test))
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. generate bech32 polymod.
%% @end
%%--------------------------------------------------------------------
-spec polymod(Values) -> Return when
    Values :: string() | binary(),
    Return :: pos_integer().

polymod(String)
  when is_binary(String) ->
    List = erlang:binary_to_list(String),
    polymod_loop(List, 1);
polymod(String)
  when is_list(String) ->
    polymod_loop(String, 1).

% @hidden
-spec polymod_test() -> any().
polymod_test() ->
    [?assertEqual(1, polymod(<<>>))
    ,?assertEqual(32, polymod(<<0>>))
    ,?assertEqual(33, polymod(<<1>>))
    ,?assertEqual(7103263, polymod(<<16#FFFF_FFFF:32>>))
    ,?assertEqual(71120775, polymod(<< <<X:32>> || X <- lists:seq(0,9) >>))
    ,?assertEqual(1, polymod([]))
    ,?assertEqual(32, polymod([0]))
    ,?assertEqual(33, polymod([1]))
    ,?assertEqual(4294967263, polymod([16#FFFF_FFFF]))
    ,?assertEqual(610366851, polymod([ X || X <- lists:seq(0,9) ]))
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. main polymod loop.
%% @end
%%--------------------------------------------------------------------
-spec polymod_loop(String, Checksum) -> Return when
      String :: [pos_integer()],
      Checksum :: pos_integer(),
      Return :: pos_integer().

polymod_loop([], Checksum) -> Checksum;
polymod_loop([Head|Tail], Checksum) ->
    Top = Checksum bsr 25,
    ChecksumStep1 = Checksum band 16#01FF_FFFF,
    ChecksumStep2 = ChecksumStep1 bsl 5,
    ChecksumStep3 = ChecksumStep2 bxor Head,
    ChecksumFinal = polymod_generator(0, ChecksumStep3, Top),
    polymod_loop(Tail, ChecksumFinal).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. generate bech32 polymod using POLYMOD_GENERATOR macro.
%% @end
%%--------------------------------------------------------------------
-spec polymod_generator(Index, Checksum, Top) -> Return when
    Index :: 0 | 1 | 2 | 3 | 4 | 5,
    Checksum :: pos_integer(),
    Top :: pos_integer(),
    Return :: pos_integer().

?POLYMOD_GENERATOR(0, 16#3B6A_57B2);
?POLYMOD_GENERATOR(1, 16#2650_8E6D);
?POLYMOD_GENERATOR(2, 16#1EA1_19FA);
?POLYMOD_GENERATOR(3, 16#3D42_33DD);
?POLYMOD_GENERATOR(4, 16#2A14_62B3);
polymod_generator(5, Checksum, _Top) -> Checksum.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. generate hrp expansion.
%% @end
%%--------------------------------------------------------------------
-spec hrp_expand(HRP) -> Return when
    HRP :: hrp(),
    Return :: hrp().

hrp_expand(HRP)
  when is_list(HRP) ->
    Head = [ (X bsr 5) || X <- HRP ],
    Tail = [ (X band 31) || X <- HRP ],
    Head ++ [0] ++ Tail.

% @hidden
-spec hrp_expand_test() -> any().
hrp_expand_test() ->
    [?assertEqual([0], hrp_expand([]))
    ,?assertEqual([3,3,3,3,0,20,5,19,20], hrp_expand("test"))
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% verify bech32 checksum from data.
%% @end
%%--------------------------------------------------------------------
-spec verify_checksum(HRP, Data) -> Return when
      HRP :: hrp(),
      Data :: data(),
      Return :: format().

verify_checksum(HRP, Data) ->
    Expand = hrp_expand(HRP),
    Const = polymod(Expand ++ Data),
    case Const of
        1 -> bech32;
        ?BECH32M_CONST -> bech32m;
        _ -> undefined
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% create a new bech32 checksum.
%% @end
%%--------------------------------------------------------------------
-spec create_checksum(HRP, Data, Opts) -> Return when
      HRP :: hrp(),
      Data :: data(),
      Opts :: [{format, format()}],
      Return :: [integer()].

create_checksum(HRP, Data, Opts)
  when is_list(HRP) andalso is_list(Data) ->
    Spec = proplists:get_value(format, Opts),
    Expand = hrp_expand(HRP),
    Values = Expand ++ Data,
    Polymod = polymod(Values ++ [0,0,0,0,0,0]),
    case Spec of
        bech32m -> create_checksum_final(Polymod, ?BECH32M_CONST);
        bech32 -> create_checksum_final(Polymod, 1);
        _ -> throw({error, [{reason, "unsupported format"}]})
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
create_checksum_final(Polymod, Const) ->
    Polymod2 = Polymod bxor Const,
    Ret = fun(P, I) -> (P bsr 5 * (5 - I)) band 31 end,
    [ Ret(Polymod2, Index) || Index <- lists:seq(0,5) ].

% @hidden
-spec create_checksum_test() -> any().
create_checksum_test() ->
    [?assertEqual([2,13,27,24,0,28]
                 ,create_checksum("a", [2], [{format, bech32}]))
    ,?assertEqual([23,17,11,20,5,30]
                 ,create_checksum("a", [2], [{format, bech32m}]))
    ,?assertException(throw, _
                     ,create_checksum("a", [2], [{format, bech32z}]))
    ].

%%--------------------------------------------------------------------
%% @doc `encode_bech32/2' function encodes indexed data with padding
%% using bech32 format.
%% @see encode/3
%% @end
%%--------------------------------------------------------------------
-spec encode_bech32(HRP, Data) -> Return when
      HRP :: [integer()],
      Data :: [integer()],
      Return :: {ok, [pos_integer()]}.

encode_bech32(HRP, Data) ->
    Opts = [{format, bech32}, {indexed, true}],
    case encode(HRP, Data, Opts) of
        {ok, Result} -> Result;
        Elsewise -> throw(Elsewise)
    end.

%%--------------------------------------------------------------------
%% @doc `encode_bech32m/2' function encodes indexed data with padding
%% using to bech32m format.
%% @see encode/3
%% @end
%%--------------------------------------------------------------------
-spec encode_bech32m(HRP, Data) -> Return when
      HRP :: [integer()],
      Data :: [integer()],
      Return :: {ok, [pos_integer()]}.

encode_bech32m(HRP, Data) ->
    Opts = [{format, bech32m},{indexed, true}],
    case encode(HRP, Data, Opts) of
        {ok, Result} -> Result;
        Elsewise -> throw(Elsewise)
    end.

%%--------------------------------------------------------------------
%% @doc `encode/2' function encodes indexed data with padding using
%% bech32 format.
%% @see encode_bech32/2
%% @see encode/3
%% @end
%%--------------------------------------------------------------------
-spec encode(HRP, Data) -> Return when
      HRP :: [integer()],
      Data :: [integer()],
      Return :: {ok, [pos_integer()]}.

encode(HRP, Data) ->
    encode(HRP, Data, [{format, bech32}]).

%%--------------------------------------------------------------------
%% @doc `encode/3' function is used to encode indexed or unindexed
%% data in bech32 or bech32m format. It supports `list()' or
%% `binary()' types in input and few options.
%% @end
%%--------------------------------------------------------------------
-spec encode(HRP, Data, Opts) -> Return when
      HRP :: [integer()],
      Data :: [integer()],
      Opts :: [Option, ...],
      Option :: {format, format()} 
              | {binary, boolean()}
              | {padding, boolean()},
      Return :: {ok, [pos_integer()]}.

encode(HRP, Data, Opts)
  when is_binary(HRP) ->
    encode(binary_to_list(HRP), Data, Opts);
encode(HRP, Data, Opts)
  when is_binary(Data) ->
    encode(HRP, binary_to_list(Data), Opts);
encode(HRP, Data, Opts) ->
    encode_check_format(HRP, Data, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
encode_check_format(HRP, Data, Opts) ->
    case proplists:get_value(format, Opts) of
        bech32 -> encode_check_data(HRP, Data, Opts);
        bech32m -> encode_check_data(HRP, Data, Opts);
        _ -> {error, [{reason, "Unsupported format"}]}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
encode_check_data(HRP, Data, Opts) ->
    Indexed = proplists:get_value(indexed, Opts, true),
    case valid_indexed_string(Data) of
        {ok, _} ->
            Checksum = create_checksum(HRP, Data, Opts),
            encode_final(HRP, Data, Checksum, Opts);
        {error, _} when Indexed =:= false ->
            {ok, Converted} = convertbits(Data, 8, 5, Opts),
            Checksum = create_checksum(HRP, Converted, Opts),
            encode_final(HRP, Converted, Checksum, Opts);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
encode_final(HRP, Data, Checksum, Opts) ->
    Binary = proplists:get_value(binary, Opts, false),
    Combined = Data ++ Checksum,
    Result = HRP ++ "1" ++ [ index_to_charset(C) || C <- Combined ],
    Final = case Binary of
                true -> list_to_binary(Result);
                false -> Result
            end,
    {ok, Final}.

% @hidden
-spec encode_test() -> any().
encode_test() ->
    [% encode/3
     ?assertEqual({ok, "a12uel5l"}
                 ,encode("a", [], [{format, bech32}]))
    ,?assertEqual({ok, "a12uel5l"}
                 ,encode(<<"a">>, [], [{format, bech32}]))
    ,?assertEqual({ok, "a12uel5l"}
                 ,encode(<<"a">>, <<>>, [{format, bech32}]))
    ,?assertEqual({error, [{reason, "Unsupported format"}]}
                 ,encode(<<"a">>, <<>>, [{format, bech32z}]))
    ,?assertEqual({ok, "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"}
                 ,encode("an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio", [], [{format, bech32}]))
    ,?assertEqual({ok,"test1lu0zy72x"}
                 ,encode("test", [255], [{indexed, false},{format, bech32}]))
    ,?assertEqual({ok,"test1lu675j0y"}
                 ,encode("test", [255], [{indexed, false},{format, bech32m}]))
    ,?assertEqual({ok,<<"test1lu675j0y">>}
                 ,encode("test", [255], [{indexed, false},{format, bech32m},{binary, true}]))
     % encode/2
    ,?assertEqual({ok,"test1pzryfsa92x"}
                 , bech32:encode("test", [1,2,3,4]))
     % encode_bech32/2
    ,?assertEqual("test1pzryfsa92x"
                 ,encode_bech32("test", [1,2,3,4]))
    ,?assertException(throw, {error,[{reason,"Invalid char"},{char,[255]},{position,0},{head,[]},{rest,[]}]}
                     ,encode_bech32("test", [255]))
     % encode_bech32m/2
    ,?assertEqual("test1pzryuvdf0y"
                 ,encode_bech32m("test", [1,2,3,4]))
    ,?assertException(throw, {error,[{reason,"Invalid char"},{char,[255]},{position,0},{head,[]},{rest,[]}]}
                     ,encode_bech32m("test", [255]))
    ].


%%--------------------------------------------------------------------
%% @doc`decode/1' function decodes bech32 encoded data.
%% @see decode/2
%% @end
%%--------------------------------------------------------------------
-spec decode(Bech) -> Return when
      Bech :: list(),
      Return :: {ok, map()} | {error, Reason},
      Reason :: term().

decode(Bech) ->
    decode(Bech, []).

%%--------------------------------------------------------------------
%% @doc `decode/2' function decodes bech32 encoded data.
%%
%% == Examples ==
%%
%% This is the default behavior, it will output the raw value from
%% base 32 (2^5):
%%
%% ```
%% {ok, #{ checksum => [4,18,23,26,14,26]
%%       , data => [7,15,24,12,12,15,30,11,18,13,3,3,8,1,29,15,18,30,18,30,11,27|...]
%%       , format => bech32
%%       , hrp => "npub"
%%       , origin => "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
%%       }
%% } = decode("npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6").
%% '''
%%
%% One can change this behavior by using a custom base using the
%% converter option and `{base, Base}'where `Base' is a strictly
%% positive integer representing a base 2.
%% 
%% ```
%% Address = "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6".
%% {ok, #{ checksum => [4,18,23,26,14,26]
%%       , data => [59,240,198,63,203,147,70,52,7,175,151,165,229,238,100,250|...]
%%       , format => bech32
%%       , hrp => "npub"
%%       , origin => "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
%%       }
%% } = bech32:decode(Address, [{converter, {base, 8}}]).
%% '''
%%
%% One can also create a lambda function to deal with the output and
%% converted the final data in another customer format.
%%
%% ```
%% % create a new lambda function
%% Converter = fun(Data) -> 
%%   Binary = erlang:list_to_binary(Data),
%%   Hex = binary:encode_hex(Binary),
%%   {ok, Hex}
%% end.
%%
%% {ok, #{ checksum => [4,18,23,26,14,26]
%%       , data => <<"3BF0C63FCB93463407AF97A5E5EE64FA883D107EF9E558472C4EB9AAAEFA459D00">>
%%       , format => bech32
%%       , hrp => "npub"
%%       , origin => "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
%%       }
%% } = bech32:decode(Address, [{converter, Converter}])
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(Bech, Opts) -> Return when
      Bech :: list(),
      Opts :: [Option, ...],
      Option :: {converter, Converter},
      Converter :: function() 
                 | {base, Base},
      Base :: pos_integer(),
      Return :: {ok, map()} | {error, Reason},
      Reason :: term().

decode(Bech, Opts) 
  when is_binary(Bech) ->
    decode(binary_to_list(Bech), Opts);
decode(Bech, _Opts) 
  when length(Bech) > 90 ->
    {error, [{reason, "Overall max length exceeded"}]};
decode(Bech, Opts) ->
    State = #bech32{ origin = Bech },
    decode_check1(Bech, State, Opts).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
decode_check1(Bech, State, Opts) ->
    case decode_check_characters(Bech) of
        {ok, _} -> decode_check2(Bech, State, Opts);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
decode_check2(Bech, State, Opts) ->
    Lower = string:lowercase(Bech),
    Upper = string:uppercase(Bech),
    case  Lower =/= Bech andalso Upper =/= Bech of
        false -> decode_split(Lower, State, Opts);
        true -> {error, [{reason, "wrong case"},{data,Bech}]}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
decode_split(Bech, #bech32{} = State, Opts) ->
    Reverse = lists:reverse(Bech),
    decode_split(Bech, Reverse, [], 1, State, Opts).

% @hidden
decode_split(_Bech, [], _Data, _Position, _State, _Opts) ->
    {error, [{reason, "No separator character"}]};
decode_split(_Bech, [$1], _Data, _Position, _State, _Opts) ->
    {error, [{reason, "Empty HRP"}]};
decode_split(_Bech, [$1|_HRP], Data, _Position, _State, _Opts)
  when length(Data) < 6 ->
    {error, [{reason, "Too short checksum"}]};
decode_split(Bech, [$1|HRP], Data, _Position, State, Opts) ->
    FinalHRP = lists:reverse(HRP),
    NewState = State#bech32{ hrp = FinalHRP },
    decode_data(Bech, Data, NewState, Opts);
decode_split(Bech, [Head|Tail], Buffer, Position, State, Opts) ->
    case is_charset(Head) of
        true -> decode_split(Bech, Tail, [Head|Buffer], Position+1, State, Opts);
        false ->
            P = length(Bech)-Position+1,
            {error, [{reason, "Invalid data character"}
                    ,{position, P}]}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
decode_data(Bech, RawData, State, Opts) ->
    case decode_data2(RawData, [], 1) of
        {ok, Data, Checksum} ->
            NewState = State#bech32{ data = Data },
            decode_checksum(Bech, Checksum, NewState, Opts);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
decode_checksum(Bech, Checksum, #bech32{ data = Data, hrp = HRP } = State, Opts) ->
    case decode_format(HRP, Data ++ Checksum) of
        {ok, Format} ->
            NewState = State#bech32{ checksum = Checksum, format = Format },
            decode_output(Bech, Checksum, NewState, Opts);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @todo cleanup this function. It has been created to help to convert
%%       data in another base than base32 using a power of 2 or custom
%%       function.
%% @end
%%--------------------------------------------------------------------
decode_output(_Bech, _Checksum, #bech32{ data = Data } = State, Opts) ->
    BaseOutput = proplists:get_value(converter, Opts, {base, 5}),
    case BaseOutput of
        {base, 5} -> 
            {ok, to_map(State)};
        {base, B} when B > 0 -> 
            case convertbits(Data, 5, B) of
                {ok, NewData} ->
                    {ok, to_map(State#bech32{ data = NewData })};
                Elsewise -> Elsewise
            end;
        {base, B} when is_number(B) -> 
            {error, [{reason, "Invalid base"},{base, B}]};
        B when is_function(B) -> 
            try B(Data)
            of
                {ok, NewData} -> {ok, to_map(State#bech32{ data = NewData })};
                Elsewise -> Elsewise
            catch
                _E:R -> {error, R}
            end;
        Elsewise -> Elsewise
    end.

% @hidden
-spec decode_test() -> any().
decode_test() ->
    [
    % bech32 valid
     ?assertEqual({ok, #{ hrp => "a"
                        , data => []
                        , checksum => [10,28,25,31,20,31]
                        , format => bech32
                        , origin => "A12UEL5L"
                        }
                  }
                  ,decode(<<"A12UEL5L">>))
    ,?assertEqual({ok, #{ hrp => "a"
                        , data => []
                        , checksum => [10,28,25,31,20,31]
                        , format => bech32
                        , origin => "A12UEL5L"
                        }
                  }
                  ,decode("A12UEL5L"))
    ,?assertEqual({ok, #{ hrp => "a"
                        , data => []
                        , checksum => [10,28,25,31,20,31]
                        , format => bech32
                        , origin => "a12uel5l"
                        }
                  }
                  ,decode("a12uel5l"))
     ,?assertEqual({ok, #{ hrp => "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio"
                         , data => []
                         , checksum => [11,11,20,11,8,16]
                         , format => bech32
                         , origin => "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
                         }
                   }
                  ,decode("an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"))
    ,?assertEqual({ok, #{ hrp => "abcdef"
                        , data => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                  10, 11, 12, 13, 14, 15, 16,
                                  17, 18, 19, 20, 21, 22, 23,
                                  24, 25, 26, 27, 28, 29, 30,
                                  31]
                        , checksum => [27,0,0,0,6,14]
                        , format => bech32
                        , origin => "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
                        }
                  }
                 ,decode("abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"))
    ,?assertEqual({ok, #{ hrp => "1"
                        , data => [ 0 || _ <- lists:seq(1,82) ]
                        , checksum => [24,7,10,21,30,18]
                        , format => bech32
                        , origin => "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
                        }
                  }
                 ,decode("11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"))
    ,?assertEqual({ok, #{ hrp => "split"
                        , data => [24, 23, 25, 24, 22, 28, 1, 16,
                                  11, 29, 8, 25, 23, 29, 19, 13,
                                  16, 23, 29, 22, 25, 28, 1, 16,
                                  11, 3, 25, 29, 27, 25, 3, 3,
                                  29, 19, 11, 25, 3, 3, 25, 13,
                                  24, 29, 1, 25, 3, 3, 25, 13]
                        , checksum => [10,4,5,25,17,14]
                        , format => bech32
                        , origin => "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
                        }
                  }
                 ,decode("split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"))
    ,?assertEqual({ok, #{ hrp => "?"
                        , data => []
                        , checksum => [25,2,4,9,24,31]
                        , format => bech32
                        , origin => "?1ezyfcl"
                        }
                  }
                 ,decode("?1ezyfcl"))

    % bech32m valid
    ,?assertEqual({ok, #{ hrp => "a"
                        , data => []
                        , checksum => [31,0,9,19,17,29]
                        , format => bech32m
                        , origin => "A1LQFN3A"
                        }
                  }
                 ,decode("A1LQFN3A"))
    ,?assertEqual({ok, #{ hrp => "a"
                        , data => []
                        , checksum => [31,0,9,19,17,29]
                        , format => bech32m
                        , origin => "a1lqfn3a"
                        }
                  }
                 ,decode("a1lqfn3a"))
    ,?assertEqual({ok, #{ hrp => "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber1"
                        , data => []
                        , checksum => [16,8,30,23,8,26]
                        , format => bech32m
                        , origin => "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6"
                        }
                  }
                 ,decode("an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6"))
     ,?assertEqual({ok, #{ hrp => "abcdef"
                         , data => [31, 30, 29, 28, 27, 26, 25,
                                   24, 23, 22, 21, 20, 19, 18,
                                   17, 16, 15, 14, 13, 12, 11,
                                   10, 9, 8, 7, 6, 5, 4, 3, 2,
                                   1, 0]
                         , checksum => [2,13,17,3,4,6]
                         , format => bech32m
                         , origin => "abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx"
                         }
                   }
                  ,decode("abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx"))
     ,?assertEqual({ok, #{ hrp => "1"
                         , data => [ 31 || _ <- lists:seq(1,82) ]
                         , checksum => [31,28,13,16,3,7]
                         , format => bech32m
                         , origin => "11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8"
                         }
                   }
                  ,decode("11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8"))
    ,?assertEqual({ok, #{ hrp => "split"
                        , data => [24, 23, 25, 24, 22, 28, 1, 16,
                                  11, 29, 8, 25, 23, 29, 19, 13,
                                  16, 23, 29, 22, 25, 28, 1, 16,
                                  11, 3, 25, 29, 27, 25, 3, 3,
                                  29, 19, 11, 25, 3, 3, 25, 13,
                                  24, 29, 1, 25, 3, 3, 25, 13]
                        , checksum => [31,24,21,21,20,12]
                        , format => bech32m
                        , origin => "split1checkupstagehandshakeupstreamerranterredcaperredlc445v"
                        }
                   }
                 ,decode("split1checkupstagehandshakeupstreamerranterredcaperredlc445v"))
    ,?assertEqual({ok, #{ hrp => "?"
                        , data => []
                        , checksum => [12,30,20,5,29,29]
                        , format => bech32m
                        , origin =>  "?1v759aa"
                        }
                 }
                ,decode("?1v759aa"))
    % bech32 invalid
     ,?assertEqual({error, [{reason, "HRP character out of range"},{position, 1}]}
                 ,decode(" 1nwldj5"))
    ,?assertEqual({error, [{reason, "HRP character out of range"},{position, 1}]}
                 ,decode([16#7F] ++ "1axkwrx"))
    ,?assertEqual({error, [{reason, "HRP character out of range"},{position, 1}]}
                 ,decode([16#80] ++ "1eym55h"))
    ,?assertEqual({error, [{reason, "Overall max length exceeded"}]}
                 ,decode("an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx"))
    ,?assertEqual({error, [{reason, "No separator character"}]}
                 ,decode("pzry9x0s0muk"))
    ,?assertEqual({error, [{reason, "Empty HRP"}]}
                 ,decode("1pzry9x0s0muk"))
    ,?assertEqual({error, [{reason, "Invalid data character"},{position,3}]}
                 ,decode("x1b4n0q5v"))
    ,?assertEqual({error, [{reason, "Too short checksum"}]}
                 ,decode("li1dgmt3"))
    ,?assertEqual({error, [{reason,"HRP character out of range"},{position,9}]}
                 ,decode("de1lg7wt" ++ [16#FF]))
    ,?assertEqual({error, [{reason, "Empty HRP"}]}
                 ,decode("10a06t8"))
    ,?assertEqual({error, [{reason, "Empty HRP"}]}
                 ,decode("1qzzfhee"))
    ,?assertEqual({error, [{reason, "Invalid checksum"}]}
                 ,decode("A1G7SGD8"))

    % bech32m invalid
    ,?assertEqual({error, [{reason, "HRP character out of range"},{position, 1}]}
                 ,decode(" 1xj0phk"))
    ,?assertEqual({error, [{reason, "HRP character out of range"},{position, 1}]}
                 ,decode([16#7F] ++ "1g6xzxy"))
    ,?assertEqual({error, [{reason, "HRP character out of range"},{position, 1}]}
                 ,decode([16#80] ++ "1vctc34"))
    ,?assertEqual({error, [{reason, "Overall max length exceeded"}]}
                 ,decode("an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4"))
    ,?assertEqual({error, [{reason, "No separator character"}]}
                 ,decode("qyrz8wqd2c9m"))
    ,?assertEqual({error, [{reason, "Empty HRP"}]}
                 ,decode("1qyrz8wqd2c9m"))
    ,?assertEqual({error, [{reason, "Invalid data character"},{position,3}]}
                 ,decode("y1b0jsk6g"))
    ,?assertEqual({error, [{reason, "Invalid data character"},{position,4}]}
                 ,decode("lt1igcx5c0"))
    ,?assertEqual({error, [{reason, "Too short checksum"}]}
                 ,decode("in1muywd"))
    ,?assertEqual({error, [{reason, "Invalid data character"},{position,9}]}
                 ,decode("mm1crxm3i"))
    ,?assertEqual({error, [{reason, "Invalid data character"},{position,8}]}
                 ,decode("au1s5cgom"))
    ,?assertEqual({error, [{reason, "Invalid checksum"}]}
                 ,decode("M1VUXWEZ"))
    ,?assertEqual({error, [{reason, "Empty HRP"}]}
                 ,decode("16plkw9"))
    ,?assertEqual({error, [{reason, "Empty HRP"}]}
                 ,decode("1p2gdwpf"))
    ,?assertEqual({ok, #{ checksum => [4,18,23,26,14,26]
                        , data => [59,240,198,63,203,147,70,52,7,175,151,165,229,238,100,250,136,61,16,126,249,
                                   229,88,71,44,78,185,170,174,250,69,157,0]
                        , format => bech32
                        , hrp => "npub"
                        , origin => "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
                        }}
                 , decode("npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6", [{converter, {base, 8}}]))
    ,?assertEqual({error,[{reason,"Invalid base"},{base,-1}]}
                 , decode("npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6", [{converter, {base, -1}}]))
     ,?assertEqual({ok, #{ checksum => [4,18,23,26,14,26]
                         , data => <<"3BF0C63FCB93463407AF97A5E5EE64FA883D107EF9E558472C4EB9AAAEFA459D00">>
                         , format => bech32
                         , hrp => "npub"
                         , origin => "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
                         }}
                  , bech32:decode("npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
                                 ,[{converter, fun(Data) -> 
                                                       {ok, Base8} = convertbits(Data, 5, 8),
                                                       Binary = erlang:list_to_binary(Base8),
                                                       Hex = binary:encode_hex(Binary),
                                                       {ok, Hex}
                                               end}]))
    ,?assertEqual({error, [{reason, "custom error"}]}
                 ,bech32:decode("npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
                               ,[{converter, fun(_) -> {error, [{reason, "custom error"}]} end}]))
    ,?assertEqual({error, badarith}
                 ,bech32:decode("npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
                               ,[{converter,  fun(X) -> a/X end}]))
    ].

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. check if each characters are correct and valid.
%% @end
%%--------------------------------------------------------------------
decode_check_characters(Bech) ->
    decode_check_characters(Bech, [], 1).

% @hidden
decode_check_characters([], Buffer, _) ->
    {ok, lists:reverse(Buffer)};
decode_check_characters([Head|_Tail], _Buffer, Position)
  when Head < 33 orelse Head > 126 ->
    {error, [{reason,"HRP character out of range"}
            ,{position, Position}]};
decode_check_characters([Head|Tail], Buffer, Position) ->
    decode_check_characters(Tail, [Head|Buffer], Position+1).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. decode the data and checksum part.
%% @end
%%--------------------------------------------------------------------
decode_data2([], [C0,C1,C2,C3,C4,C5|RawData], _Position) ->
    RawChecksum = [C0,C1,C2,C3,C4,C5],
    Checksum = [ charset_to_index(Char) || Char <- lists:reverse(RawChecksum) ],
    Data = [ charset_to_index(Char) || Char <- lists:reverse(RawData) ],
    {ok, Data, Checksum};
decode_data2([Head|Tail], Buffer, Position) ->
    case is_charset(Head) of
        true ->
            decode_data2(Tail, [Head|Buffer], Position+1);
        false ->
            {error, [{reason, "Invalid charset"},{position, Position}]}
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. decode the format using the checksum.
%% @end
%%--------------------------------------------------------------------
decode_format(HRP, Data) ->
    case verify_checksum(HRP, Data) of
        bech32 -> {ok, bech32};
        bech32m -> {ok, bech32m};
        _ -> {error, [{reason, "Invalid checksum"}]}
    end.

%%--------------------------------------------------------------------
%% @doc `convertbits/3' function implements a power of 2 base
%% conversion with padding support.
%% @see convertbits/4
%% @end
%%--------------------------------------------------------------------
-spec convertbits(Data, From, To) -> Return when
      Data :: list() | binary(),
      From :: integer(),
      To :: integer(),
      Return :: {ok, list()} | {error, Reason},
      Reason :: proplists:proplist().

convertbits(Data, From, To) ->
    convertbits(Data, From, To, []).

% @hidden
-spec convertbits_test() -> any().
convertbits_test() ->
    [?assertEqual({ok, [0, 4, 1, 0, 6, 1, 0, 5]}
                 ,convertbits(<<1,2,3,4,5>>, 8, 5))
    ,?assertEqual({ok, [0, 4, 1, 0, 6, 1, 0, 5]}
                 ,convertbits([1,2,3,4,5], 8, 5))
    ,?assertEqual({ok, [31, 28]}
                 ,convertbits([255], 8, 5))
    ,?assertEqual({ok, [1, 1, 1, 1, 1, 1, 1, 1]}
                 ,convertbits([255], 8, 1))
    ,?assertEqual({ok, [255]}
                 ,convertbits([1, 1, 1, 1, 1, 1, 1, 1], 1, 8))
    ,?assertEqual({error, [{reason,"Value greater than source"},{position,1},{value,1024}]}
                 ,bech32:convertbits([1024], 8, 5))
    ,?assertEqual({error,[{reason,"Negative value"},{position,1},{value,-1}]}
                 ,bech32:convertbits([-1], 8, 5))
    ,?assertEqual({error,[{reason,"Remaining bits issue"},{position,2},{value,3}]}
                 ,bech32:convertbits([123], 8, 5, [{padding, false}]))
    ,?assertEqual({ok,[15,12]}
                 ,convertbits([123], 8, 5, [{padding, true}]))
    ].

%%--------------------------------------------------------------------
%% @doc `convertbits/4' function implements a power of 2 base
%% conversion with or without padding support.
%% @end
%%--------------------------------------------------------------------
-spec convertbits(Data, From, To, Opts) -> Return when
      Data :: list() | binary(),
      From :: integer(),
      To :: integer(),
      Opts :: [Option, ...],
      Option :: {padding, boolean()},      
      Return :: {ok, list()} | {error, Reason},
      Reason :: proplists:proplist().

convertbits(Data, From, To, Opts)
  when is_binary(Data) ->
    convertbits(binary_to_list(Data), From, To, Opts);
convertbits(Data, From, To, Opts) ->
    Maxv = (1 bsl To) - 1,
    Maxa = (1 bsl (From + To - 1)) -1,
    State = #{ max_value => Maxv
             , max_accumulator => Maxa
             , accumulator  => 0
             , bits => 0
             , ret => []
             , padding => proplists:get_value(padding, Opts, true)
             , position => 1
             },
    convertbits1(Data, From, To, State).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. main convertbits loop.
%% @end
%%--------------------------------------------------------------------
convertbits1([], _From, To, #{ max_value := Maxv, accumulator := Acc, bits := Bits, ret := Ret, padding := true })
  when Bits > 0 ->
    {ok, lists:reverse([(Acc bsl (To-Bits)) band Maxv|Ret])};
convertbits1([], _From, _To, #{ ret := Ret, padding := true }) ->
    {ok, lists:reverse(Ret)};
convertbits1([], From, To, #{ bits := Bits, position := Position, accumulator := Acc, max_value := Maxv })
  when Bits >= From orelse ((Acc bsl (To-Bits)) band Maxv) > 0 ->
    {error, [{reason, "Remaining bits issue"},{position, Position},{value, Bits}]};
convertbits1([], _From, _To, #{ ret := Ret }) ->
    {ok, lists:reverse(Ret)};
convertbits1([Head|_], _From, _To, #{ position := Position })
  when Head < 0 ->
    {error, [{reason, "Negative value"},{position, Position},{value, Head}]};
convertbits1([Value|_], From, _To, #{ position := Position })
  when (Value bsr From) > 0 ->
    {error, [{reason, "Value greater than source"},{position, Position},{value, Value}]};
convertbits1([Value|_] = Data, From, To, #{ max_accumulator := Maxa, accumulator := Acc, bits := Bits } = State) ->
    Acc2 = ((Acc bsl From) bor Value) band Maxa,
    Bits2 = Bits + From,
    NewState = State#{ accumulator => Acc2, bits => Bits2 },
    convertbits2(Data, From, To, NewState).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. bits conversion.
%% @end
%%--------------------------------------------------------------------
convertbits2(Data, From, To, #{ bits := Bits, ret := Ret, accumulator := Acc, max_value := Maxv } = State)
  when Bits >= To ->
    Bits2 = Bits - To,
    Ret2 = [(Acc bsr Bits2) band Maxv|Ret],
    NewState = State#{ bits => Bits2, ret => Ret2 },
    convertbits2(Data, From, To, NewState);
convertbits2([_|Tail], From, To, #{ position := Position } = State) ->
    convertbits1(Tail, From, To, State#{ position => Position + 1}).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal. used to convert bech32 record.
%% @end
%%--------------------------------------------------------------------
-spec to_map(Record) -> Return when
      Record :: #bech32{},
      Return :: map().

to_map(#bech32{ hrp = HRP, data = Data, checksum = Checksum, format = Format, origin = Origin}) ->
    #{ hrp => HRP
     , data => Data
     , checksum => Checksum
     , format => Format
     , origin => Origin
     }.
