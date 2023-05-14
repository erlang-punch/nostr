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
%%% @doc
%%%
%%% == Encoding Usage ==
%%%
%%% `encode/1' function is used to encode `map()' data like the one
%%% returned by `decoded' functions. `encode/3' function is taking the
%%% HRP, the segwit address version and the content to encode the full
%%% address. Both functions support `list()' or `binary()' strings.
%%%
%%% ```
%%% % Using segwit addressing version 0
%%% X0 = [ X || X <- lists:seq(1,20) ].
%%% {ok,"test1qqypqxpq9qcrsszg2pvxq6rs0zqg3yyc5uskwrt"}
%%%    = segwit:encode("test", 0, X0).
%%%
%%% X1 = <<242,1,222,236,34,170,153,51,244,185,250,46,
%%%        97,15,13,60,200,225,235,73>>.
%%% {ok,"test1q7gqaampz42vn8a9elghxzrcd8nywr66fupy0m7"}
%%%    = segwit:encode("test", 0, X1).
%%%
%%% % Using segwit addressing version 1
%%% Y0 = [ X || X <- lists:seq(0,1) ].
%%% {ok,"test1pqqqstnxe9u"}
%%%    = segwit:encode("test", 1, Y0).
%%%
%%% Y1 = [ X || X <- lists:seq(0,39) ].
%%% {ok,"test1pqqqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqgfzyvjz2f38ykjdds"}
%%%    =  segwit:encode("test", 1, Y1).
%%%
%%% Y3 = <<185,45,39,5,190,182,64,200,255,162,155,35,
%%%        222,94,176,194,235,116,165,19,179,96,83>>.
%%% {ok,"test1phykjwpd7keqv3laznv3auh4sct4hffgnkds9x8m9qgt"}
%%%    = segwit:encode("test", 1, Y3).
%%% '''
%%%
%%% == Decoding Usage ==
%%%
%%% `decode/1' and `decode/2' are used to decode segwit address as
%%% `list()' or `binary()'. They will both return a `map()' containing
%%% the decoded values.
%%%
%%% ```
%%% {ok, #{ hrp => "test"
%%%       , value => [185,45,39,5,190,182,64,200,
%%%                   255,162,155,35,222,94,176,194,
%%%                   235,116,165,19,179,96,83]
%%%       , version => 1
%%%       }
%%% } = segwit:decode("test1phykjwpd7keqv3laznv3auh4sct4hffgnkds9x8m9qgt").
%%% '''
%%%
%%% @end
%%%===================================================================
-module(segwit).
-export([encode/1, encode/3]).
-export([decode/1, decode/2]).
-include("bech32.hrl").

% local record
-record(segwit_address, { version = undefined :: undefined | integer()
                        , value = undefined :: undefined | binary() | list()
                        , hrp = undefined :: undefined | list() | binary()
                        }).

%%--------------------------------------------------------------------
%% @doc encode data with a sigwit address from a map.
%% @end
%%--------------------------------------------------------------------
-spec encode(Map) -> Return when
      Map :: #{ hrp => hrp(), version => integer(), value => list() },
      Return :: list().

encode(#{ hrp := HRP, version := Version, value := Value }) ->
    encode(HRP, Version, Value).

%%--------------------------------------------------------------------
%% @doc encode data with a sigwit address.
%% @end
%%--------------------------------------------------------------------
-spec encode(HRP, Witver, Witprog) -> Return when
      HRP :: hrp(),
      Witver :: integer(),
      Witprog :: binary() | string(),
      Return :: {ok, list()} | {error, Reason},
      Reason :: proplists:proplist().

encode(HRP, Witver, Witprog)
  when is_binary(HRP) ->
    encode(binary_to_list(HRP), Witver, Witprog);
encode(HRP, Witver, Witprog)
  when is_list(HRP) ->
    encode1(HRP, Witver, Witprog).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
encode1(HRP, Witver, Witprog)
  when is_integer(Witver) andalso Witver >= 0 ->
    encode2(HRP, Witver, Witprog).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
encode2(HRP, Witver, Witprog)
  when is_binary(Witprog) ->
    encode2(HRP, Witver, binary_to_list(Witprog));
encode2(HRP, Witver, Witprog)
  when is_list(Witprog) ->
    encode_format(#{ hrp => HRP, witver => Witver, witprog => Witprog }).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
encode_format(#{ witver := Witver } = State) ->
    Spec = case Witver of
               0 -> bech32;
               _ -> bech32m
           end,
    encode_conversion(State#{ format => Spec }).

% @hidden
encode_conversion(#{ witprog := Witprog } = State) ->
    {ok, Converted} = bech32:convertbits(Witprog, 8, 5),
    encode_bech32(State#{ converted => Converted }).

% @hidden
encode_bech32(#{ hrp := HRP, witver := Witver, converted := Converted, format := Spec } = State) ->
    Data = [Witver] ++ Converted,
    case bech32:encode(HRP, Data, [{format, Spec}]) of
        {ok, Encoded} -> encode_final(State#{ encoded => Encoded });
        Elsewise -> Elsewise
    end.

% @hidden
encode_final(#{ hrp := HRP, encoded := Encoded }) ->
    case decode(HRP, Encoded) of
        {ok, _} -> {ok, Encoded};
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
extract_hrp(Address)
  when is_binary(Address) ->
    extract_hrp(binary_to_list(Address));
extract_hrp(Address)
  when is_list(Address) ->
    extract_hrp(Address, lists:reverse(Address)).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
extract_hrp(Address, [$1|Rest]) ->
    HRP = lists:reverse(Rest),
    {ok, HRP, Address};
extract_hrp(Address, [_|T]) ->
    extract_hrp(Address, T).

%%--------------------------------------------------------------------
%% @doc decode a sigwit address. This function can easily crash
%% because it splits the string in two parts, the HRP, and the full
%% address.
%%
%% @end
%%--------------------------------------------------------------------
-spec decode(Address) -> Return when
      Address :: list() | binary(),
      Return :: {ok, map()}.

decode(String) ->
    {ok, HRP, Address} = extract_hrp(String),
    decode(HRP, Address).

%%--------------------------------------------------------------------
%% @doc decode a sigwit address.
%% @end
%%--------------------------------------------------------------------
-spec decode(any(), any()) -> any().
decode(HRP, Address) ->
    case bech32:decode(Address) of
        {ok, #{ hrp := BechHRP}}
          when HRP =/= BechHRP ->
            {error, [{reason, "Invalid hrp"}]};
        {ok, Bech} ->
            decode2(Bech);
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
decode2(#{ data := [] } = _Bech) ->
    {error, [{reason, "Empty data section"}]};
decode2(#{ data := [Version|Data], format := Spec, hrp := HRP } = _Bech) ->
    case bech32:convertbits(Data, 5, 8, [{padding, false}]) of
        {ok, Decoded} ->
            case Decoded of
                _ when length(Decoded) < 2 ->
                    {error, [{reason, "Invalid program length for witness version 0 (per BIP141)"}]};
                _ when length(Decoded) > 40 ->
                    {error, [{reason, "Invalid program length for witness version 0 (per BIP141)"}]};
                _ when Version > 16 ->
                    {error, [{reason, "Invalid version"}]};
                _ when Version =:= 0 andalso length(Decoded) =/= 20 andalso length(Decoded) =/= 32 ->
                    {error, [{reason, "Invalid data length"}]};
                _ when Version =:= 0 andalso Spec =/= bech32 ->
                    {error, [{reason, "Invalid version and format"}]};
                _ when Version =/= 0 andalso Spec =/= bech32m ->
                    {error, [{reason, "Invalid version and format"}]};
                Decoded ->
                    Segwit = #segwit_address{ version = Version, value = Decoded, hrp = HRP},
                    {ok, segwit_address_to_map(Segwit)}
            end;
        Elsewise -> Elsewise
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
segwit_address_to_map(#segwit_address{ version = Version, value = Decoded, hrp = HRP }) ->
    #{ version => Version
     , value => Decoded
     , hrp => HRP
     }.
