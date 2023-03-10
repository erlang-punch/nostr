%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2023 Mathieu Kerjouan
%%% @since nostr/0.1.0 (2023)
%%% @doc
%%%
%%% This module implement Schnorr signature like defined in the <a
%%% href="https://github.com/bitcoin/bips/blob/master/bip-0340/reference.py">Bitcoin
%%% BIP-0340 specification</a> using only Erlang.
%%%
%%% Indeed, the Schnorr signature scheme is used by Bitcoin but also
%%% by nostr. All messages coming from clients are signed using it,
%%% and the relay should be able to validate them. This feature is
%%% mandatory and critical for the nostr project.
%%%
%%% The code present in this module has been optimized using
%%% `crypto:mod_pow/3' function from `crypto' module. More tests can
%%% be found in `extra' directory at the root of this project.
%%%
%%% == Examples ==
%%%
%%% ```
%%% % generate a private key using secp256k1 cipher
%%% {ok, PrivateKey} = nostrlib_schnorr:new_privatekey().
%%%
%%% % generate the public key from the private key
%%% {ok, PublicKey} = nostrlib_schnorr:new_publickey(PrivateKey).
%%%
%%% % Create a message
%%% Message = <<"Hello Joe, how are you?">>.
%%%
%%% % Create the SHA256 checksum of this message
%%% Hash = crypto:hash(sha256, Message).
%%%
%%% % Sign the hash of the message
%%% % Note: nostrlib_schnorr:sign/2 can also be used
%%% {ok, Signature} = nostrlib_schnorr:sign(Hash, PrivateKey, <<0:256>>).
%%%
%%% % Valid the signature with the PublicKey
%%% true = nostrlib_schnorr:verify(Hash, PublicKey, Signature).
%%% '''
%%%
%%% == modulo function ==
%%%
%%% `erlang:rem/2' and `erlang:div/2' are not compatible with Schnorr
%%% signature -- at least the one implemented by Bitcoin. A modulo
%%% function needs to be created, a functional was found on stack
%%% overflow. The `crypto:mod_pow/3' function can be used to generate
%%% modulo from positive integer using the second argument (`P') to 1.
%%%
%%% == pow function ==
%%%
%%% `crypto:mod_pow/3' does not support negative integer, it must
%%% support negative integer like the pow function provided by
%%% python. This function is working for positive numbers though.
%%%
%%% ```
%%% # Python
%%% 33 == pow(-123456789, 1, 123)
%%% 23664 == pow(-987654321, 1, 65535)
%%%
%%% # Erlang
%%% <<"F">> = crypto:mod_pow(-123456789, 1, 123).
%%% <<X2:16>> = crypto:mod_pow(-987654321, 1, 65535).
%%% X2 = 23665.
%%% '''
%%%
%%% @end
%%%===================================================================
-module(nostrlib_schnorr).
-export([new_privatekey/0, new_publickey/1]).
-export([sign/2, sign/3]).
-export([verify/3]).
-export([mod/2, pow/3]).
% only exported during debug
% -export([point_add/2, point_mul/2]).
% -export([point_to_bitstring/1, bytes_from_int/1, lift_x/1]).
% -export([integer_to_bitstring/1, bitstring_to_integer/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-define(P, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F).
-define(N, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141).
-define(GX, 16#79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798).
-define(GY, 16#483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8).
-define(G, #point{ x = ?GX, y = ?GY }).

-record(point, { x = 0 :: pos_integer()
               , y = 0 :: pos_integer()
               }).
-type point() :: #point{ x :: pos_integer()
                       , y :: pos_integer()
                       }
               | infinity.

-type private_key() :: <<_:256>>.
-type public_key() :: <<_:256>>.
-type message() :: <<_:256>>.
-type aux_rand() :: <<_:256>>.
-type signature() :: <<_:512>>.

%%--------------------------------------------------------------------
%% required for eunit.
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc internal function. `point_add/2' adds two points. This
%% function is not optimized and will have huge impact on performance,
%% in particular during the exponentiation.
%%
%% @end
%%--------------------------------------------------------------------
-spec point_add(Point1, Point2) -> Return when
      Point1 :: point(),
      Point2 :: point(),
      Return :: point().

point_add(infinity, #point{} = P2) -> P2;
point_add(#point{} = P1, infinity) -> P1;
point_add(#point{x = XP1, y = YP1} = _P1, #point{x = XP2, y = YP2} = _P2)
  when XP1 =:= XP2 andalso YP1 =/= YP2 ->
    infinity;
point_add(#point{x = XP1, y = YP1} = P1, #point{x = XP2, y = _YP2} = P2)
  when P1 =:= P2 ->
    Pow = pow(2*YP1, ?P-2, ?P),
    Lam = mod((3 * XP1 * XP1 * Pow), ?P),
    X3 = mod( (Lam*Lam-XP1-XP2), ?P),
    Y3 = Lam * (XP1-X3) - YP1,
    ModY3 = mod(Y3, ?P),
    #point{ x = X3, y = ModY3 };
point_add(#point{x = XP1, y = YP1} = _P1, #point{x = XP2, y = YP2} = _P2) ->
    Pow = pow(XP2-XP1, ?P-2, ?P),
    Lam = mod((YP2-YP1)*Pow, ?P),
    X3 = mod(Lam*Lam-XP1-XP2, ?P),
    Y3 = Lam * (XP1-X3) - YP1,
    ModY3 = mod(Y3, ?P),
    #point{ x = X3, y = ModY3 }.

% @hidden
-spec point_add_test() -> any().
point_add_test() ->
    [?assertEqual(#point{x = 7, y = 11}
                 ,point_add(infinity, #point{ x = 7, y = 11})
                 )
    ,?assertEqual(#point{x = 7, y = 11}
                 ,point_add(#point{ x = 7, y = 11 }, infinity)
                 )
    ,?assertEqual(#point{ x = 19378428157484735184523243358891984578749728838671251419826579141819453736404
                        , y = 44205000258235380766042078705889964821895425212793847935799688109705992950649
                        }
                 ,point_add(#point{x = 7, y = 11}, #point{x = 7, y = 11})
                 )
    ,?assertEqual(#point{ x = 115792089237316195423570985008687907853269984665640564039457584007908834671650
                        , y = 115792089237316195423570985008687907853269984665640564039457584007908834671636
                        }
                 ,point_add(#point{x = 3, y = 11}, #point{ x = 11, y = 3})
                 )
    ,?assertEqual(#point{ x = 28948022309329048855892746252171976963317496166410141009864396001977208667908
                        , y = 14474011154664524427946373126085988481658748083205070504932198000988604333969
                        }
                 ,point_add(#point{x = 3, y = 5}, #point{ x = 7, y = 11})
                 )
    % random numbers from crypto:strong_rand_bytes
    % <<X1:(16*8), X2:(16*8), Y1:(16*8), Y2:(16*8), _/bitstring>> = crypto:strong_rand_bytes(1024).
    ,?assertEqual(#point{ x = 91135211804769037289181822946249074482714108779522925302592554235721783701018
                        , y = 99818033513876009352157021758714302832184620200363201766386068538701879468152
                        }
                 ,point_add(#point{ x = 141593973711105839125463102363107836958
                                  , y = 329489256900559930863793082810166473960
                                  }
                           ,#point{ x = 234618553877540191490899838365674539395
                                  , y = 89765200862939297360272288140060207649
                                  }
                           )
                 )
    % <<X1:(32*8), X2:(32*8), Y1:(32*8), Y2:(32*8), _/bitstring>> = crypto:strong_rand_bytes(1024).
    ,?assertEqual(#point{ x = 40492007733098845228301650448206050367870738761190187486523881458865268785647
                        , y = 7382127217435614743656716445293306338341893567227196890616741654871654947558
                        }
                 ,point_add(#point{ x = 99149246872437019612261312727375821210201487809380798340591244968847435110958
                                  , y = 75919448780511037489073435567726392226548250036757700517067622240324034566273
                                  }
                           ,#point{ x = 41628706388483764512884286855448126539167618097769771973505584034538908295376
                                  , y = 6436500457272134772481425159511058557726236442881064554112153466415715261526
                                  }
                           )
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal function. `point_mul/2' multiplies a point with an
%% integer.
%%
%% @end
%%--------------------------------------------------------------------
-spec point_mul(Point, N) -> Return when
      Point :: point(),
      N :: pos_integer(),
      Return :: point().

point_mul(#point{} = Point, N) ->
    point_mul(Point, N, infinity, 0).

%%--------------------------------------------------------------------
%% @doc internal function.
%% @end
%%--------------------------------------------------------------------
point_mul(_Point, _N, R, 256) -> R;
point_mul(Point, N, R, I) ->
    case (N bsr I) band 1 of
        0 ->
            P = point_add(Point, Point),
            point_mul(P, N, R, I+1);
        _T ->
            R2 = point_add(R, Point),
            P = point_add(Point, Point),
            point_mul(P, N, R2, I+1)
    end.

% @hidden
-spec point_mul_test() -> any().
point_mul_test() ->
    [?assertEqual(#point{ x = 7, y = 7 }
                 ,point_mul(#point{ x = 7, y = 7 }, 1)
                 )
    ,?assertEqual(#point{ x = 28948022309329048855892746252171976963317496166410141009864396001977208668012
                        , y = 101318078082651670995624611882601919371611236582435493534525386006920230336761
                        }
                 ,point_mul(#point{ x = 7, y = 7 }, 2)
                 )
    ,?assertEqual(#point{ x = 113983170870405637072057152569562644400358531773776591071171880583320981987473
                        , y = 2397548289122980910071032066274387186495158503098094242655101888740367798284
                        }
                 ,point_mul(#point{ x = 3, y = 11 }, 23)
                 )
    ,?assertEqual(#point{ x = 7290227233335871060690792451044222428311888608390440755173487812836481398552
                        , y = 10409433877825173591058732575755507734177582538799672995274584861774214108018
                        }
                 ,point_mul(#point{x = 98721398621, y = 7987213}, 6784312)
                 )

    % random numbers from crypto:strong_rand_bytes
    % <<X1:(16*8), X2:(16*8), Y1:(16*8), Y2:(16*8), _/bitstring>> = crypto:strong_rand_bytes(1024).
    ,?assertEqual(#point{ x = 14653810375998584450416643383712972377040960434358203450412140382669449377871
                        , y = 101615459546039329619859114806133454376029017792297498784803970473114837648526
                        }
                 ,point_mul(#point{ x = 141593973711105839125463102363107836958
                                  , y = 329489256900559930863793082810166473960
                                  }
                           ,89765200862939297360272288140060207649
                           )
                 )

    % <<X1:(32*8), X2:(32*8), Y1:(32*8), Y2:(32*8), _/bitstring>> = crypto:strong_rand_bytes(1024).
    ,?assertEqual(#point{ x = 109145713281741604754885712729628231331329135401249140446457991728192817682717
                        , y = 98912812288377654917975538622885692727087477773812060039032656542892668293152
                        }
                 ,point_mul(#point{ x = 99149246872437019612261312727375821210201487809380798340591244968847435110958
                                  , y = 75919448780511037489073435567726392226548250036757700517067622240324034566273
                                  }
                           , 6436500457272134772481425159511058557726236442881064554112153466415715261526
                           )
                 )

    ,?assertEqual(#point{ x = 87919591022248410098849921382602642724620778780113673678898283161568451288436
                        , y = 82771203004193098477539928827975513383082404516251638949313570671552988484250
                        }
                 ,point_mul(#point{ x = 112711660439710606056748659173929673102114977341539408544630613555209775888121
                                  , y = 25583027980570883691656905877401976406448868254816295069919888960541586679410
                                  }
                           , 67071769870995747205322053780378091752498503321064312642056330450148171233907
                           )
                 )
    ].

%%--------------------------------------------------------------------
%% @doc `modular_pow/3' is an alternative to `crypto:mod_pow/3' and
%% `math:pow/2' modular pow implementation using right to left
%% exponentiation.
%%
%% This code is not cryptographicaly safe. When a negative number is
%% given, the time used to compute it take twice the
%% time. `crypto:mod_pow/3' needs to be modified accordingly.
%%
%% The exponentiation methods given by Erlang/OTP do not fit the
%% requirement provided by the main implementation of BIP-0340. This
%% function was created to temporarily fix this issue. Many better
%% implementation and algorithms exist. See:
%%
%% <li>[https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method]
%% </li>
%% <li>[https://www.johndcook.com/blog/2008/12/10/fast-exponentiation]
%% </li>
%% <li>[https://mathstats.uncg.edu/sites/pauli/112/HTML/secfastexp.html]
%% </li>
%% <li>[https://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.99.1460]
%% </li>
%%
%% Different implementations have been tested without great success
%% for the moment.
%%
%% @end
%%--------------------------------------------------------------------
-spec pow(X, Y, Modulo) -> Return when
      X :: integer(),
      Y :: integer(),
      Modulo :: pos_integer(),
      Return :: integer().

pow(1,0,_) -> 1;
pow(0,1,_) -> 0;
pow(_,_,1) -> 0;
pow(Base, Exponent, Modulus) when Base > 0 ->
    bitstring_to_integer(crypto:mod_pow(Base, Exponent, Modulus));
pow(Base, Exponent, Modulus) when Base < 0 ->
    % @todo this part of the code is impacted by crypto:mod_pow/3 when
    % a negative value is added, the output is not compatible with the
    % implementation. Question: is it possible to compute all those
    % values without negative numbers?
    case 1 band Exponent of
        1 ->
            modular_pow(Base, Exponent, Modulus, Base);
        0 ->
            modular_pow(Base, Exponent, Modulus, 1)
    end.

%%--------------------------------------------------------------------
%% @doc internal function.
%% @end
%%--------------------------------------------------------------------
modular_pow(_Base, 0, _Modulus, Return) -> Return;
modular_pow(Base, Exponent, Modulus, Return) ->
    E2 = Exponent bsr 1,
    B2 = mod(Base*Base, Modulus),
    case E2 band 1 of
        1 ->
            modular_pow(B2, E2, Modulus, mod(Return*B2, Modulus));
        _ ->
            modular_pow(B2, E2, Modulus, Return)
    end.

% @hidden
-spec pow_test() -> any().
pow_test() ->
    [?assertEqual(0, pow(0,0,1))
    ,?assertEqual(0, pow(?P, ?P, ?P))
    ,?assertEqual(1, pow(1,0,1))
    ,?assertEqual(0, pow(0,1,1))
    ,?assertEqual(88966338620437832068624569303567783236486510711125279810536434685834011611583
                 ,pow(-109145713281741604754885712729628231331329135401249140446457991728192817682717
                             ,28897801297922945330451835637322792013828657623029579235286557361627672604967
                             ,?P)
                 )
    ,?assertEqual(26825750616878363354946415705120124616783473954515284228921149322074823060080
                 ,pow(109145713281741604754885712729628231331329135401249140446457991728192817682717
                             ,28897801297922945330451835637322792013828657623029579235286557361627672604967
                             ,?P)
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% thanks to [https://stackoverflow.com/a/2386387/6782635], Erlang is
%% using truncated division, Python is using floored modulo based on
%% euclidian division. The divrem function from python is defined
%% here: <li>
%% [https://hg.python.org/cpython/file/tip/Objects/longobject.c#l2607]
%% </li>.  Another implementation is required, but it should do the
%% job for now. The Art of programming volume 2 needs to be checked as
%% well.
%%
%% @end
%%--------------------------------------------------------------------
-spec mod(X, Y) -> Return when
      X :: integer(),
      Y :: pos_integer(),
      Return :: pos_integer().

mod(0,_) -> 0;
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 ->
    K = (-X div Y)+1,
    PositiveX = X+K*Y,
    PositiveX rem Y.

% @hidden
-spec mod_test() -> any().
mod_test() ->
    [?assertEqual(0, mod(0,0))
    ,?assertEqual(22452309387972768763530205817659855289843162532160402740598319643309799867816
                 ,mod(109145713281741604754885712729628231331329135401249140446457991728192817682717
                      ,28897801297922945330451835637322792013828657623029579235286557361627672604967
                      )
                 )
    ,?assertEqual(6445491909950176566921629819662936723985495090869176494688237718317872737151
                 ,mod(-109145713281741604754885712729628231331329135401249140446457991728192817682717
                      ,28897801297922945330451835637322792013828657623029579235286557361627672604967
                      )
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal function. `lift_x/1' lifts the point.
%% @end
%%--------------------------------------------------------------------
-spec lift_x(Integer) -> Point when
      Integer :: integer(),
      Point :: point().

lift_x(X) when X >= ?P -> infinity;
lift_x(X) ->
    Y1 = mod((pow(X, 3, ?P) + 7), ?P),
    Y2 = pow(Y1, (?P+1) div 4, ?P),
    case pow(Y2, 2, ?P) =/= Y1 of
        true -> infinity;
        false ->
            case Y2 band 1 =:= 0 of
                true -> #point{x = X, y = Y2};
                false -> #point{x = X, y = ?P-Y2}
            end
    end.

% @hidden
-spec lift_x_test() -> any().
lift_x_test() ->
    [?assertEqual(#point{ x = 1
                        , y = 29896722852569046015560700294576055776214335159245303116488692907525646231534
                        }
                 ,lift_x(1)
                 )
    ,?assertEqual(#point{ x = 123
                        , y = 57446492628264891784098775050622253460841288276710978755323331825353875991360
                        }
                ,lift_x(123)
                )
    ,?assertEqual(infinity
                 ,lift_x(35068317372709509953717756927105732741704429749274497856780391249648561587686)
                 )
    ,?assertEqual(#point{ x = 35068317372709509953717756927105732741707686
                        , y = 88331955267511973018408495437670803136063821269883896203017701759027241157628
                        }
                 ,lift_x(35068317372709509953717756927105732741707686)
                 )
    ].

%%--------------------------------------------------------------------
%% @doc `new_privatekey/0' creates a new secp256k1 private key based
%% on `crypto:generate_key/2' function.
%%
%% @see crypto:generate_key/2
%% @end
%%--------------------------------------------------------------------
-spec new_privatekey() -> PrivateKey when
      PrivateKey :: {ok, private_key()}.

new_privatekey() ->
    {_PublicKey, PrivateKey} = crypto:generate_key(ecdh, secp256k1),
    {ok, PrivateKey}.

%%--------------------------------------------------------------------
%% @doc `new_publickey/1' generates a public key from a private key.
%%
%% @see new_privatekey/0
%% @todo allow integers to be used to generate a public key.
%% @end
%%--------------------------------------------------------------------
-spec new_publickey(PrivateKey) -> PublicKey when
      PrivateKey :: private_key(),
      PublicKey :: {ok, public_key()}
                 | {error, proplists:proplists() | atom()}.

new_publickey(PrivateKey)
  when byte_size(PrivateKey) =/= 32 ->
    {error, [{message, "The private key must be a 32-byte array."}]};
new_publickey(<<0:256>>) ->
  {error, [{message, "The private key must be an integer in the range (1..n-1)."}]};
new_publickey(<<D0:256>>)
  when D0 >= ?N ->
    {error, [{message, "The private key must be an integer in the range (1..n-1)."}]};
new_publickey(PrivateKey)
  when is_bitstring(PrivateKey) ->
    D0 = bitstring_to_integer(PrivateKey),
    new_publickey(D0);
new_publickey(PrivateKey)
  when is_integer(PrivateKey)
       andalso PrivateKey >= 1
       andalso PrivateKey =< (?N-1) ->
    Point = point_mul(?G, PrivateKey),
    case Point of
        infinity -> {error, infinity};
        #point{} -> {ok, point_to_bitstring(Point)}
    end.

% @hidden
-spec new_publickey_test() -> any().
new_publickey_test() ->
    [?assertEqual({error,[{message,"The private key must be a 32-byte array."}]}
                 ,new_publickey(<<>>)
                 )
    ,?assertEqual({error,[{message,"The private key must be an integer in the range (1..n-1)."}]}
                 ,new_publickey(<<0:256>>)
                 )
    ,?assertEqual({error,[{message,"The private key must be an integer in the range (1..n-1)."}]}
                 ,new_publickey(<<?N:256>>)
                 )
    ,?assertEqual({ok, <<121,190,102,126,249,220,187,172,85,160,98,149,206
                   ,135,11,7,2,155,252,219,45,206,40,217,89,242,129,91
                   ,22,248,23,152>>}
                 ,new_publickey(<<1:256>>)
                 )
    ,?assertEqual({ok, <<27,56,144,58,67,247,241,20,237,69,0,180,234,199,8
                   ,63,222,254,206,28,242,156,99,82,141,86,52,70,249
                   ,114,193,128>>}
                 ,new_publickey(<<255:256>>)
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal function. `point_to_bitstring/1' converts a point to
%% its binary form. Only x coordinate is extracted.
%%
%% @end
%% --------------------------------------------------------------------
-spec point_to_bitstring(Point) -> Integer when
      Point :: point(),
      Integer :: pos_integer().

point_to_bitstring(#point{x = X1} = _Point) ->
    integer_to_bitstring(X1).

%%--------------------------------------------------------------------
%% @doc `sign/2' signs a message with a private key.
%%
%% @see sign/3
%% @end
%%--------------------------------------------------------------------
-spec sign(Message, PrivateKey) -> Return when
      Message :: message(),
      PrivateKey :: private_key(),
      Return :: {ok, signature()}
              | {error, proplists:proplists()}.
sign(Message, PrivateKey) ->
    sign(Message, PrivateKey, <<0:256>>).

%%--------------------------------------------------------------------
%% @doc `sign/3' signs a message with a private key and auxiliary
%% values.
%%
%% @end
%%--------------------------------------------------------------------
-spec sign(Message, PrivateKey, AuxRand) -> Return when
      Message :: message(),
      PrivateKey :: private_key(),
      AuxRand :: aux_rand(),
      Return :: {ok, signature()}
              | {error, proplists:proplists()}.

sign(Message, _, _)
  when byte_size(Message) =/= 32 ->
    {error, [{message, "The message must be a 32-byte array."}]};
sign(_, <<0:256>>, _) ->
    {error, [{message, "The private key must be an integer in the range (1..n-1)."}]};
sign(_, <<D0:256>>, _)
  when D0 >= ?N ->
    {error, [{message, "The private key must be an integer in the range (1..n-1)."}]};
sign(_, PrivateKey, _)
  when byte_size(PrivateKey) =/= 32 ->
    {error, [{message, "The private key must be a 32-byte array."}]};
sign(_, _, AuxRand)
  when byte_size(AuxRand) =/= 32 ->
    {error, [{message, "The aux_rand value must be 32 bytes."}]};
sign(<<Message/bitstring>>, <<16#B7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF:256>> = PrivateKey, <<AuxRand/bitstring>>) ->
    ?LOG_WARNING("Private Key \"~p\" should be used for development purpose only.", [PrivateKey]),
    D0 = 16#B7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF,
    Point = point_mul(?G, D0),
    sign1(Message, D0, AuxRand, Point);
sign(<<Message/bitstring>>, <<16#C90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B14E5C9:256>> = PrivateKey, <<AuxRand/bitstring>>) ->
    ?LOG_WARNING("Private Key \"~p\" should be used for development purpose only.", [PrivateKey]),
    D0 = 16#C90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B14E5C9,
    Point = point_mul(?G, D0),
    sign1(Message, D0, AuxRand, Point);
sign(<<Message/bitstring>>, <<16#0B432B2677937381AEF05BB02A66ECD012773062CF3FA2549E44F58ED2401710:256>> = PrivateKey, <<AuxRand/bitstring>>) ->
    ?LOG_WARNING("Private Key \"~p\" should be used for development purpose only.", [PrivateKey]),
    D0 = 16#0B432B2677937381AEF05BB02A66ECD012773062CF3FA2549E44F58ED2401710,
    Point = point_mul(?G, D0),
    sign1(Message, D0, AuxRand, Point);
sign(<<Message/bitstring>>, <<D0:256>> = _PrivateKey, <<AuxRand/bitstring>>) ->
    Point = point_mul(?G, D0),
    sign1(Message, D0, AuxRand, Point).

% @hidden
-spec sign_test() -> any().
sign_test() ->
    [?assertEqual({ok,
                   <<233,7,131,31,128,132,141,16,105,165,55,27,64,36,16
                    ,54,75,223,28,95,131,7,176,8,76,85,241,206,45,202
                    ,130,21,37,246,106,74,133,234,139,113,228,130,167
                    ,79,56,45,44,229,235,238,232,253,178,23,47,71,125
                    ,244,144,13,49,5,54,192>> }
                 ,sign(<<0:256>>, <<3:256>>, <<0:256>>)
                 )

    ,?assertEqual({ok,
                   <<104,150,189,96,238,174,41,109,180,138,34,159,247
                    ,29,254,7,27,222,65,62,109,67,249,23,220,141,207
                    ,140,120,222,51,65,137,6,209,26,201,118,171,204
                    ,178,11,9,18,146,191,244,234,137,126,252,182,57
                    ,234,135,28,250,149,246,222,51,158,75,10>>
                  }
       ,sign(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>
            ,<<16#B7E151628AED2A6ABF7158809CF4F3C762E7160F38B4DA56A784D9045190CFEF:256>>
            ,<<16#0000000000000000000000000000000000000000000000000000000000000001:256>>
            )
       )

    ,?assertEqual({ok,
                   <<88,49,170,238,215,180,75,183,78,94,171,148,186
                    ,157,66,148,196,155,207,42,96,114,141,139,76,32
                    ,15,80,221,49,60,27,171,116,88,121,165,173,149,74
                    ,114,196,90,145,195,165,29,60,122,222,169,141,130
                    ,248,72,30,14,30,3,103,74,111,63,183>>
                  }
       ,sign(<<16#7E2D58D8B3BCDF1ABADEC7829054F90DDA9805AAB56C77333024B9D0A508B75C:256>>
            ,<<16#C90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B14E5C9:256>>
            ,<<16#C87AA53824B4D7AE2EB035A2B5BBBCCC080E76CDC6D1692C4B0B62D798E6D906:256>>
            )
       )
    ,?assertEqual({ok,
                   <<126,176,80,151,87,226,70,241,148,73,136,86,81,97
                    ,28,185,101,236,193,161,135,221,81,182,79,218,30,220
                    ,150,55,213,236,151,88,43,156,177,61,179,147,55,5,179
                    ,43,169,130,175,90,242,95,215,136,129,235,179,39,113
                    ,252,89,34,239,198,110,163>>
                  }
       ,sign(<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256>>
            ,<<16#0B432B2677937381AEF05BB02A66ECD012773062CF3FA2549E44F58ED2401710:256>>
            ,<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256>>
            )
       )
    ,?assertEqual({error, [{message, "The message must be a 32-byte array."}]}
                 ,sign(<<>>, <<>>, <<>>)
                 )
    ,?assertEqual({error, [{message, "The private key must be a 32-byte array."}]}
                 ,sign(<<0:256>>, <<>>, <<>>)
                 )
    ,?assertEqual({error, [{message, "The aux_rand value must be 32 bytes."}]}
                 ,sign(<<0:256>>, <<1:256>>, <<>>)
                 )
    ,?assertEqual({error, [{message, "The private key must be an integer in the range (1..n-1)."}]}
                 ,sign(<<0:256>>, <<0:256>>, <<0:256>>)
                 )
    ,?assertEqual({error, [{message, "The private key must be an integer in the range (1..n-1)."}]}
                 ,sign(<<0:256>>, <<?N:256>>, <<0:256>>)
                 )
    ,?assertEqual({error, [{message, "The private key must be an integer in the range (1..n-1)."}]}
                 ,sign(<<0:256>>, <<(?N+1):256>>, <<0:256>>)
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign1(_Message, _D0, _AuxRand, infinity) ->
    {error, [{message, "p is set to infinity."}]};
sign1(Message, D0, AuxRand, Point) ->
    D = case has_even_y(Point) of
            true -> D0;
            false -> ?N-D0
        end,
    T = sign_tag_aux(D, AuxRand),
    K0 = sign_tag_nonce(T, Point, Message),
    sign2(Message, D0, AuxRand, Point, D, K0).

% @hidden
-spec sign1_test() -> any().
sign1_test() ->
    [?assertEqual({error, [{message, "p is set to infinity."}]}
                ,sign1(0,0,0,infinity))
    ].

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign2(_Message, _D0, _AuxRand, _Point, _D, 0) ->
    {error, [{message, "Failure. This happens only with negligible probability."}]};
sign2(Message, D0, AuxRand, Point, D, K0) ->
    R = point_mul(?G, K0),
    sign3(Message, D0, AuxRand, Point, D, K0, R).

% @hidden
-spec sign2_test() -> any().
sign2_test() ->
    [?assertEqual({error, [{message, "Failure. This happens only with negligible probability."}]}
                 ,sign2(<<>>, <<>>, <<>>, #point{}, <<>>, 0))
    ].

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign3(_Message, _D0, _AuxRand, _Point, _D, _K0, infinity) ->
    {error, [{message, "r is set to infinity"}]};
sign3(Message, _D0, _AuxRand, Point, D, K0, R) ->
    K = case has_even_y(R) of
            false -> ?N - K0;
            true -> K0
        end,
    E = sign_tag_challenge(R, Point, Message),
    Signature = sign_make(R, K, E, D),
    case verify(Message, point_to_bitstring(Point), Signature) of
       true -> {ok, Signature};
       false -> {error, [{message, "The created signature does not pass verification."}]}
    end.

% @hidden
-spec sign3_test() -> any().
sign3_test() ->
    [?assertEqual({error, [{message, "r is set to infinity"}]}
                 ,sign3(<<>>, <<>>, <<>>, #point{}, <<>>, <<>>, infinity))
    ].

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign_make(R, K, E, D) ->
    Mod = mod(K+E*D, ?N),
    Head = point_to_bitstring(R),
    Tail = integer_to_bitstring(Mod),
    <<Head/bitstring, Tail/bitstring>>.

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign_tag_aux(D, AuxRand) ->
    T_Tagged = tagged_hash(<<"BIP0340/aux">>, AuxRand),
    IntegerD = integer_to_bitstring(D),
    crypto:exor(IntegerD, T_Tagged).

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign_tag_nonce(T, Point, Message) ->
    K0_Tag = <<"BIP0340/nonce">>,
    K0_Message = <<T/bitstring
                  ,(point_to_bitstring(Point))/bitstring
                  ,Message/bitstring>>,
    K0_Tagged = tagged_hash(K0_Tag, K0_Message),
    mod(bitstring_to_integer(K0_Tagged), ?N).

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
sign_tag_challenge(R, Point, Message) ->
    E_Tag = <<"BIP0340/challenge">>,
    E_Message = <<(point_to_bitstring(R))/bitstring
                 ,(point_to_bitstring(Point))/bitstring
                 ,Message/bitstring>>,
    E_Tagged = tagged_hash(E_Tag, E_Message),
    mod(bitstring_to_integer(E_Tagged), ?N).

%%--------------------------------------------------------------------
%% @doc `verify/3' checks a message with its signature and a public
%% key.
%%
%% @end
%%--------------------------------------------------------------------
-spec verify(Message, PublicKey, Signature) -> Return when
      Message :: message(),
      PublicKey :: public_key(),
      Signature :: signature(),
      Return :: boolean().

verify(Message, _, _)
  when byte_size(Message) =/= 32 ->
    {error, [{"The message must be a 32-byte array."}]};
verify(_, PublicKey, _)
  when byte_size(PublicKey) =/= 32 ->
    {error, [{"The public key must be a 32-byte array."}]};
verify(_, <<0:256>> = _PublicKey, _) ->
    {error, [{"The public key must be an integer in the range (1..n-1)."}]};
verify(_, _, Signature)
  when byte_size(Signature) =/= 64 ->
    {error, [{"The signature must be a 64-byte array"}]};
verify(Message, PublicKey, <<RSign:256, SSign:256>> = _Signature) ->
    P = lift_x(bitstring_to_integer(PublicKey)),
    Verif1 = (P =:= infinity) orelse (RSign >= P) orelse (SSign >= ?N),
    case Verif1 of
        true -> false;
        false ->
            E = verify_tag(Message, PublicKey, RSign),
            RPoint_A = point_mul(?G, SSign),
            RPoint_B = point_mul(P, ?N-E),
            RPoint = point_add(RPoint_A, RPoint_B),
            verify2(RPoint, RSign)
    end.

% @hidden
-spec verify_test() -> any().
verify_test() ->
    [?assertEqual({error,[{"The message must be a 32-byte array."}]}
                 ,verify(<<>>, <<>>, <<>>)
                 )
    ,?assertEqual({error,[{"The public key must be a 32-byte array."}]}
                 ,verify(<<0:256>>, <<>>, <<>>)
                 )
    ,?assertEqual({error,[{"The signature must be a 64-byte array"}]}
                 ,verify(<<0:256>>, <<1:256>>, <<0>>)
                 )
    ,?assertEqual({error, [{"The public key must be an integer in the range (1..n-1)."}]}
                 ,verify(<<0:256>>, <<0:256>>, <<0:512>>)
                 )
    ,?assertEqual(true
                  ,verify(<<16#0000000000000000000000000000000000000000000000000000000000000000:256>>
                         ,<<16#F9308A019258C31049344F85F89D5229B531C845836F99B08601F113BCE036F9:256>>
                         ,<<16#E907831F80848D1069A5371B402410364BDF1C5F8307B0084C55F1CE2DCA821525F66A4A85EA8B71E482A74F382D2CE5EBEEE8FDB2172F477DF4900D310536C0:512>>
                         )
                  )

    ,?assertEqual(true
                 ,verify(<<16#4DF3C3F68FCC83B27E9D42C90431A72499F17875C81A599B566C9889B9696703:256>>
                        ,<<16#D69C3509BB99E412E68B0FE8544E72837DFA30746D8BE2AA65975F29D22DC7B9:256>>
                        ,<<16#00000000000000000000003B78CE563F89A0ED9414F5AA28AD0D96D6795F9C6376AFB1548AF603B3EB45C9F8207DEE1060CB71C04E80F593060B07D28308D7F4:512>>
                        )
                 )

     ,?assertEqual(true,
                   verify(<<16#4DF3C3F68FCC83B27E9D42C90431A72499F17875C81A599B566C9889B9696703:256>>,
                          <<16#D69C3509BB99E412E68B0FE8544E72837DFA30746D8BE2AA65975F29D22DC7B9:256>>,
                          <<16#00000000000000000000003B78CE563F89A0ED9414F5AA28AD0D96D6795F9C6376AFB1548AF603B3EB45C9F8207DEE1060CB71C04E80F593060B07D28308D7F4:512>>))

    ,?assertEqual(false,
                 verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                        <<16#EEFDEA4CDB677750A420FEE807EACF21EB9898AE79B9768766E4FAA04A2D4A34:256>>,
                        <<16#6CFF5C3BA86C69EA4B7376F31A9BCB4F74C1976089B2D9963DA2E5543E17776969E89B4C5564D00349106B8497785DD7D1D713A8AE82B32FA79D5F7FC407D39B:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#FFF97BD5755EEEA420453A14355235D382F6472F8568A18B2F057A14602975563CC27944640AC607CD107AE10923D9EF7A73C643E166BE5EBEAFA34B1AC553E2:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#1FA62E331EDBC21C394792D2AB1100A7B432B013DF3F6FF4F99FCB33E0E1515F28890B3EDB6E7189B630448B515CE4F8622A954CFE545735AAEA5134FCCDB2BD:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#6CFF5C3BA86C69EA4B7376F31A9BCB4F74C1976089B2D9963DA2E5543E177769961764B3AA9B2FFCB6EF947B6887A226E8D7C93E00C5ED0C1834FF0D0C2E6DA6:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#0000000000000000000000000000000000000000000000000000000000000000123DDA8328AF9C23A94C1FEECFD123BA4FB73476F0D594DCB65C6425BD186051:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#00000000000000000000000000000000000000000000000000000000000000017615FBAF5AE28864013C099742DEADB4DBA87F11AC6754F93780D5A1837CF197:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#4A298DACAE57395A15D0795DDBFD1DCB564DA82B0F269BC70A74F8220429BA1D69E89B4C5564D00349106B8497785DD7D1D713A8AE82B32FA79D5F7FC407D39B:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F69E89B4C5564D00349106B8497785DD7D1D713A8AE82B32FA79D5F7FC407D39B:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#DFF1D77F2A671C5F36183726DB2341BE58FEAE1DA2DECED843240F7B502BA659:256>>,
                         <<16#6CFF5C3BA86C69EA4B7376F31A9BCB4F74C1976089B2D9963DA2E5543E177769FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141:512>>))

    ,?assertEqual(false,
                  verify(<<16#243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89:256>>,
                         <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC30:256>>,
                         <<16#6CFF5C3BA86C69EA4B7376F31A9BCB4F74C1976089B2D9963DA2E5543E17776969E89B4C5564D00349106B8497785DD7D1D713A8AE82B32FA79D5F7FC407D39B:512>>))
    ].

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
verify2(infinity, _) -> false;
verify2(#point{x = X} = _RPoint, RSign) when X =/= RSign -> false;
verify2(RPoint, _RSign) ->
    case not has_even_y(RPoint) of
        true -> false;
        false -> true
    end.

%%--------------------------------------------------------------------
%% @doc internal function.
%%
%% @end
%%--------------------------------------------------------------------
verify_tag(Message, PublicKey, R) ->
    E_Tag = <<"BIP0340/challenge">>,
    E_Message = <<R:256, PublicKey/bitstring, Message/bitstring>>,
    E_Tagged = bitstring_to_integer(tagged_hash(E_Tag, E_Message)),
    mod(E_Tagged, ?N).

%%--------------------------------------------------------------------
%% @doc internal function. `tagged_hash/2' creates a new tagged hash.
%%
%% @end
%%--------------------------------------------------------------------
-spec tagged_hash(Tag, Message) -> Hash when
      Tag :: bitstring(),
      Message :: iodata(),
      Hash :: iodata().

-define(SHA256_TAG_NONCE, <<7,73,119,52,167,155,203,53,91,155,140
                           ,125,3,79,18,28,244,52,215,62,247,45
                           ,218,25,135,0,97,251,82,191,235,47>>).

-define(SHA256_TAG_AUX, <<241,239,78,94,192,99,202,218,109,148,202
                         ,250,157,152,126,160,105,38,88,57,236,193
                         ,31,151,45,119,165,46,216,193,204,144>>).

-define(SHA256_TAG_CHALLENGE, <<123,181,45,122,159,239,88,50,62,177
                               ,191,122,64,125,179,130,210,243,242
                               ,216,27,177,34,79,73,254,81,143,109
                               ,72,211,124>>).

tagged_hash(<<"BIP0340/aux">>, Message) ->
    TagHash = ?SHA256_TAG_AUX,
    crypto:hash(sha256, <<TagHash/bitstring, TagHash/bitstring, Message/bitstring>>);
tagged_hash(<<"BIP0340/challenge">>, Message) ->
    TagHash = ?SHA256_TAG_CHALLENGE,
    crypto:hash(sha256, <<TagHash/bitstring, TagHash/bitstring, Message/bitstring>>);
tagged_hash(<<"BIP0340/nonce">>, Message) ->
    TagHash = ?SHA256_TAG_NONCE,
    crypto:hash(sha256, <<TagHash/bitstring, TagHash/bitstring, Message/bitstring>>);
tagged_hash(Tag, Message) ->
    TagHash = crypto:hash(sha256, Tag),
    crypto:hash(sha256, <<TagHash/bitstring, TagHash/bitstring, Message/bitstring>>).

% @hidden
-spec tagged_hash_test() -> any().
tagged_hash_test() ->
    [?assertEqual(<<45,186,93,188,51,158,115,22,174,162,104,63,175
                   ,131,156,27,123,30,226,49,61,183,146,17,37,136
                   ,17,141,240,102,170,53>>
                 ,tagged_hash(<<>>, <<>>)
                 )
    ,?assertEqual(<<3,4,228,53,11,241,70,225,142,233,227,234,108,197
                   ,232,2,211,243,63,103,51,176,168,109,51,125,59,57
                   ,238,89,167,104>>
                 ,tagged_hash(<<"test">>, <<"test">>)
                 )
    ,?assertEqual(<<84,241,105,207,201,226,229,114,116,128,68,31,144
                   ,186,37,196,136,244,97,199,11,94,165,220,170,247
                   ,175,105,39,10,165,20>>
                 ,tagged_hash(<<"BIP0340/aux">>, <<0:256>>)
                 )
    ].

%%--------------------------------------------------------------------
%% @doc internal function. `has_even_y/1' returns true if y is even.
%%
%% @end
%%--------------------------------------------------------------------
-spec has_even_y(point()) -> boolean.

has_even_y(#point{ y = Y } = _Point) -> Y rem 2 =:= 0;
has_even_y(_) -> false.

% @hidden
-spec has_even_y_test() -> any().
has_even_y_test() ->
    [?assertEqual(false, has_even_y(#point{ y = 1 }))
    ,?assertEqual(false, has_even_y(infinity))
    ,?assertEqual(true, has_even_y(#point{ y = 2 }))
    ].

%%--------------------------------------------------------------------
%% @doc internal function. `integer_to_bitstring/1' converts an
%% integer to a bitstring.
%%
%% @see erlang:integer_to_binary/2
%% @end
%%--------------------------------------------------------------------
-spec integer_to_bitstring(Integer) -> Bitstring when
      Integer :: pos_integer(),
      Bitstring :: iodata().

integer_to_bitstring(Integer) ->
    Size = byte_size(erlang:integer_to_binary(Integer, 2)),
    <<0:(256-Size), Integer:Size/unsigned-integer>>.

% @hidden
-spec integer_to_bitstring_test() -> any().
integer_to_bitstring_test() ->
    [?assertEqual(<<0:256>>, integer_to_bitstring(0))
    ,?assertEqual(<<1:256>>, integer_to_bitstring(1))
    ,?assertEqual(<<65535:256>>, integer_to_bitstring(65535))
    ].

%%--------------------------------------------------------------------
%% @doc internal function. `bitstring_to_integer/1' convers a
%% bitstring to an integer.
%%
%% @see crypto:bytes_to_integer/1
%% @end
%%--------------------------------------------------------------------
-spec bitstring_to_integer(Bitstring) -> Integer when
      Bitstring :: iodata(),
      Integer :: pos_integer().
bitstring_to_integer(Bitstring) ->
    crypto:bytes_to_integer(Bitstring).

% @hidden
-spec bitstring_to_integer_test() -> any().
bitstring_to_integer_test() ->
    [?assertEqual(123, bitstring_to_integer(<<123>>))
    ].
