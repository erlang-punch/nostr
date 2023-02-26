%%%====================================================================
%%% @doc
%%%
%%% @end
%%%====================================================================
-module(nostrlib_schnorr_SUITE).
-export([suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([groups/0, all/0]).
-export([verification_vectors/0, verification_vectors/1]).
-export([signature_vectors/0, signature_vectors/1]).
-export([common/0, common/1]).
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

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_per_suite(_Config) -> [].

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
end_per_suite(_Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->  Config.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) -> Config.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
groups() -> [{vectors, [parallel], [verification_vectors
                                   ,signature_vectors
                                   ,common]}
            ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
all() -> [{group, vectors, [parallel]}].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec common() -> any().
common() -> [].

-spec common(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
common(_Config) ->
    {ok, <<PrivateKey:256>>} = nostrlib_schnorr:new_privatekey(),
    {ok, <<_:256>>} = nostrlib_schnorr:new_publickey(PrivateKey).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec verification_vectors() -> any().
verification_vectors() -> [].

-spec verification_vectors(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
verification_vectors(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    VerificationTest = fun (#{ <<"index">> := I
                             , <<"message">> := M
                             , <<"public key">> := P
                             , <<"signature">> := S
                             , <<"verification result">> := V }) ->
                               Info = [{index, I}
                                      ,{message, M}
                                      ,{public_key, P}
                                      ,{signature, S}
                                      ,{result, V}
                                      ],
                               V = nostrlib_schnorr:verify(M, P, S),
                               ct:pal(info, "verify (ok): ~p", [Info])
                       end,
    lists:map(VerificationTest, test_vectors(DataDir)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec signature_vectors() -> any().
signature_vectors() -> [].

-spec signature_vectors(Config) -> Return when
      Config :: proplists:proplists(),
      Return :: any().
signature_vectors(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    SignatureTest = fun (#{ <<"secret key">> := <<>> }) ->
                            ignored;
                        (#{ <<"index">> := I
                          , <<"message">> := M
                          , <<"aux_rand">> := A
                          , <<"signature">> := S
                          , <<"secret key">> := K }) ->
                            Info = [{index, I}
                                   ,{message, M}
                                   ,{secret_key, K}
                                   ,{aux_rand, A}
                                   ,{signature, S}
                                   ],
                            {ok, S} = nostrlib_schnorr:sign(M, K, A),
                            ct:pal(info, "signature (ok): ~p", [Info])
                       end,
    lists:map(SignatureTest, test_vectors(DataDir)).

%%--------------------------------------------------------------------
%% @doc internal function. read the test-vectors.csv file from
%% BIP-0340 and converts it in an Erlang like format.
%%
%% @end
%%--------------------------------------------------------------------
test_vectors(Directory) ->
    {ok, File} = file:read_file(filename:join(Directory, "test-vectors.csv")),
    Lines = re:split(File, "\r\n"),
    Fields = lists:map(fun(X) ->
                      re:split(X, ",")
              end, Lines),
    Cleaned = lists:filter(fun _F([<<>>]) -> false; _F(_) -> true end, Fields),
    [Header|Content] = Cleaned,
    Zip = fun _Zip ([<<>>]) -> undefined;
              _Zip (C) -> Z = lists:zip(Header, C),
                         M = maps:from_list(Z),
                         maps:map(fun _F(<<"aux_rand">>, Y) when Y =/= <<>> -> <<(binary_to_integer(Y, 16)):256>>;
                                      _F(<<"secret key">>, Y) when Y =/= <<>> -> <<(binary_to_integer(Y, 16)):256>>;
                                      _F(<<"signature">>, Y) when Y =/= <<>> -> <<(binary_to_integer(Y, 16)):512>>;
                                      _F(<<"public key">>, Y) when Y =/= <<>> -> <<(binary_to_integer(Y, 16)):256>>;
                                      _F(<<"message">>, Y) when Y =/= <<>> -> <<(binary_to_integer(Y, 16)):256>>;
                                      _F(<<"verification result">>, <<"FALSE">>) -> false;
                                      _F(<<"verification result">>, <<"TRUE">>) -> true;
                                      _F(_X, Y) -> Y
                                  end, M)
          end,
    lists:map(Zip, Content).
