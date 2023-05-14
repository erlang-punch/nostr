---
date: 2023-08-11
title: Implementing Bech32 and Segwit Address in Pure Erlang
subtitle: From Pair Programming to Solo Programming
author: Mathieu Kerjouan
keywords: erlang,otp,nostr,bech32,segwit,address,bitcoin
license: CC BY-NC-ND
abstract: |

  Bech32 encoding is used by Bitcoin and Nostr to encode binary data like 
  public key or signature. On Bitcoin side, another format is also 
  used, called Segwit using Bech32 to encoding its payload. This 
  article is a brief overview of Bech32 and Segwit implementation 
  in pure Erlang. The goal is to show how to use this new implementation,
  but also to explain how and why it as been implemented. A quick
  overview of other implementation is also available at the end of
  this publication.

toc: true
hyperrefoptions:
- linktoc=all
---

# Implementing Bech32 and Segwit Address in Pure Erlang

This implementation was created to be easily shared with other Erlang
project/application without importing external modules. A simple
copy/paste of `bech32` and `segwit` modules should do the job for the
moment or one can important this application in his own project by
including nostr as dependency. In both case, it should work.

In fact, this application has not been deployed in production
environment, and we can't really trust it for now, but it will be
pretty soon integrated in `nostr` project. Both modules have a decent
coverage (more than 90%) and should work pretty well everywhere.

## Bitcoin, Bech32 and Segwit Addresses

[Bech32](https://en.bitcoin.it/wiki/Bech32)[^bitcoin-bech32] data
format was designed, created and specified in
[BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)[^bitcoin-bip-00173]
by the Bitcoin core team, in particular Pieter Wuille[^github-sipa]
and Greg Maxwell[^github-maxwell]. The goal of this new format was to
fix issues from the old
[base58](https://en.bitcoin.it/wiki/BIP_0032)[^bitcoin-base58] format,
in particular its complexity and its poor performance because of
SHA256 checksum.

Bech32 format is divided in 3 parts:

 - The human readable part or HRP is used to store
 - A static separator represented by the character `1` (one)
 - A data part containing a payload and a checksum.

In other hand, segwit[^wikipedia-segwit] (segregated witness) address
is kind of subset of bech32, fixing limits on the length of the
encoded data. Only segwit address standard is implemented there. 

If you are looking for more information about how bech32 magic is
working, other posts[^medium-bech32-maths] can easily be found using
your favorite search engine.

[^bitcoin-bech32]: [https://en.bitcoin.it/wiki/Bech32](https://en.bitcoin.it/wiki/Bech32)
[^bitcoin-bip-0173]: [https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
[^bitcoin-base58]: [https://en.bitcoin.it/wiki/BIP_0032](https://en.bitcoin.it/wiki/BIP_0032)
[^github-sipa]: [https://github.com/sipa](https://github.com/sipa)
[^github-maxwell]: [https://github.com/gmaxwell](https://github.com/gmaxwell)
[^wikipedia-segwit]: [https://en.wikipedia.org/wiki/SegWit](https://en.wikipedia.org/wiki/SegWit)
[^medium-bech32-maths]: [https://medium.com/@meshcollider/some-of-the-math-behind-bech32-addresses-cf03c7496285](https://medium.com/@meshcollider/some-of-the-math-behind-bech32-addresses-cf03c7496285)

## Teaching and Learning with Pair Programming

This implementation started as an experiment with the following
question in mind: "is it possible to do pair programming during live
session recorded on multi-platform?" The idea was to deploy a small
collaborative text editor like cloud9[^cloud9-ide] in an isolated
place, share a link to this editor to the developers and start working
on the project, without a lot of preparation.

The good part of this experiment was to see the different point of
view during the implementation of the algorithm, based on the
respective experiences of each participants. It was interesting to see
what kind of solution was firstly implemented and how it evolved.

The bad part was quite important though. Because of the lack of
preparation, the code was not correctly implemented. During the
experiment, it was quite hard to think about the different features.

It's always hard to work with someone else, even more when you are
recorded and without any kind of preparation. If you want to record
your pair programming session, try, at least, to prepare it few weeks
before. Try also to do a small implementation by yourself first, just
to find which part will be more complex than another.

[^cloud9-ide]: https://en.wikipedia.org/wiki/Cloud9_IDE

## From Wild Implementation to Erlang/OTP

Creating implementation only from specification is hard, sometimes,
the specifications are not even available, and one will need to read
code example from already existing application. It could be harder at
first to understand the code created by some anonymous developers but
it can also help a lot.

The main reference used to implement it in Erlang was the Python
version but few others like the one in Haskell, C and Ruby helped a
lot as well. One big advantage with script languages like Python or
Ruby is the way you can develop by just crafting something on a
REPL. Recreating the module in this environment by just copying and
pasting it or by importing it works pretty well.

The previous implementation of Schnorr scheme[^schnorr] was also using
this model and it makes a lot of things easier. Firstly, having a code
you can play with will help you to design the interfaces you will
create on your own implementation. Everyone is designing them
differently because of their experiences or their requirement. In the
case of bech32 and segwit, the implementation was created with already
some helpers in mind.

Secondly, having an example in another language will help you to
design the state(s) you will need to deal with. One of the most
complex task is to manage the different transition of the state,
looking on the other implemention will give you an idea of the moving
part of the application.

[^schnorr]: https://github.com/erlang-punch/nostr/tree/main/notes/0004-schnorr-signature-scheme-in-erlang

## Usage and Example

The reference implementation in Python looks a bit weird at first
glance, in particular because if something goes wrong, nothing is
returned. Any developers using this version in production environment
will probably have troubles when understanding a crash because of
that. As sysadmin and Erlang developer, I hate being in a situation
where something is crashing without any explicit reason. This
bech32/segwit implementation is fixing that by explicitely returning
errors and their reason or by raising an error if no matching clause
has been found.

```erlang
1> bech32:encode("123", "test").
{error,[{reason,"Invalid char"},
        {char,"t"},
        {position,0},
        {head,[]},
        {rest,"est"}]}
```

Bech32 encoding works only with indexed values by default. The first
argument is the HRP part as `string()` and can use any kind of
printable characters. The second argument though is the indexed value,
and should use a list of integers, each one greater or equal to 0 and
lesser than 32.

```erlang
2> bech32:encode("123", [ X || X <- lists:seq(0,31) ]).
{ok,"1231qpzry9x8gf2tvdw0s3jn54khce6mua7l4lr8s5"}
```

As you can see, this function used with these arguments is returning a
correct bech32 encoded string without any errors. Using indexed values
is not always what you want and what you need. In fact, I know lot of
developers would probably use also unindexed data by default for some
of their hacks. `bech32:encode/2` is only a wrapper around
`bech32:encode/3` where the last argument contains the extra
options. If someone want to use unindexed value, or switch to another
bech32 format like `bech32m`, one can simply define it in this part of
the function.

```erlang
3> bech32:encode("123", "test", [{format, bech32}, {indexed, false}]).
{ok,"1231w3jhxaq5p4x29"}

4> bech32:encode("123", "test", [{format, bech32m}, {indexed, false}]).
{ok,"1231w3jhxaqpa9208"}
```

This feature can be quite interesting to encode on the fly any kind of
unindexed data and use bech32 as serializer mechanism. If you want to
encode External Term Format (a format used to export terms in Erlang,
also known as BERT) it is possible to do it easily.

```erlang
5> bech32:encode("123", term_to_binary([{ok, value}])
                ,[{format, bech32m}, {indexed, false}]).
{ok,"1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"}
```

The bech32 string produced contain now the value of the serialized
term `[{ok, value}]`.

```erlang
6> bech32:decode("1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp").
{ok,#{checksum => [23,21,13,27,9,1],
      data =>  [16,13,22,0,0,0,0,0,0,5,20,
                0,4,25,0,0,0,9,23,22,22,25,
                0,0,0,21,27,6,2,27,3,21,12,
                21,21,0],
      format => bech32m,hrp => "123",
      origin => "1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"}
}
```

When decoded, the data returned are using base32 and must be converted
to another base if you want to use it correctly. Because developers
are lazy, an option has been created to help them to automatically
convert this data into any kind of base (or format).


```erlang
7> bech32:decode("1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"
                ,[{converter, {base, 8}}]).
{ok,#{checksum => [23,21,13,27,9,1],
      data => [131,108,0,0,0,1,104,2,100,0,
               2,111,107,100,0,5,118,97,108,
               117,101,106,0],
      format => bech32m,hrp => "123",
      origin => "1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"}
}
```

Using `{convert, {base, 8}}`, the function is now returning a list of
integer using bytes. If you are familiar with ETF, you can see the
first integer is `131`, it's the tag used to identify this kind of
data. Does it mean our data was correctly decoded? Let try it.

```erlang
8> Data =
8> {ok, #{ data := Data}} =
8>   bech32:decode("1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"
                  ,[{converter, {base, 8}}]).

9> erlang:binary_to_term(list_to_binary(Data)).
[{ok,value}]
```

The data returned is the serialized one created previously but the
conversion is done outside the converter. To make things easier (and
lazyer), a function can be passed to the converter using `{converter,
fun(D) -> {ok, D} end}`.

```erlang
10> Converter = fun(Data) ->
10>   % convert base5 to base8
10>   {ok, Bytes} = bech32:convertbits(Data, 5, 8),
10>   % convert list to binary
10>   Binary = list_to_binary(Bytes),
10>   % convert binary to term
10>   {ok, binary_to_term(Binary)}
10> end.
#Fun<erl_eval.42.3316493>

11> bech32:decode("1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"
11>              ,[{converter, Converter}]).
{ok,#{checksum => [23,21,13,27,9,1],
      data => [{ok,value}],
      format => bech32m,hrp => "123",
      origin => "1231sdkqqqqqq95qyeqqqfhkkeqqq4mxzmr4v44qh4dmfp"}
}
```

Now, the data returned is the good one, already deserialized and
valid. The function `convertbits/3` has been used, it's the good time
to talk a bit about this one. The bech32 model implementation offered
by Bitcoin project and made in Python gives access to a function
called `convertbits`. It is used to convert from one base to
another. In fact, in Erlang we already have this feature with binary
and bitstring term coupled with pattern matching, but in this
particular scenario, I found this `convertbits` function easier to
use.

```erlang
12> bech32:convertbits("test", 8, 6).
{ok,[29,6,21,51,29,0]}

13> bech32:convertbits("test", 8, 64).
{ok,[8387236823100817408]}

14> bech32:convertbits("test", 8, 128).
{ok,[154717211161333530444085113906767331328]}

15> bech32:convertbits("test", 8, 1)
{ok,[0,1,1,1,0,1,0,0,0,1,1,0,0,1,0,1,0,1,
      1,1,0,0,1,1,0,1,1,1,0,1,0,0]}
```

In the example below, an Erlang string (a list of printable integers)
is converted to different base like base6, base64, base128 and in
bits. `convertbits/3` is probably slower than doing a simple
conversion with binary pattern matching, but it offers more
flexibility. Perhaps small optimization can be done there.

The next part is segwit, this format is a bit more restrictive than
raw bech32 encoding. Firstly, two versions of segwit are available,
version 0 and version 1. Version 0 set a limit to the length of the
data section to 20 elements.

```erlang
15> segwit:encode("test", 0, [ X || X <- lists:seq(1,19) ]).
{error,[{reason,"Invalid data length"}]}

16> segwit:encode("test", 0, [ X || X <- lists:seq(1,21) ]).
{error,[{reason,"Invalid data length"}]}

16> segwit:encode("test", 0, [ X || X <- lists:seq(1,20) ]).
{ok,"test1qqypqxpq9qcrsszg2pvxq6rs0zqg3yyc5uskwrt"}
```

In other hands, this is not really the case with version 1 of the
format, where it can encode a value from 2 to 40 elements.

```erlang
17> segwit:encode("test", 1, [ X || X <- [1,2] ]).
{ok,"test1pqypqe8v4yj"}

18> segwit:encode("test", 1, [ X || X <- lists:seq(1,40) ]).
{ok,"test1pqypqxpq9qcrsszg2pvxq6rs0zqg3yyc5z5tpwxqergd3c8g7"
    "ruszzg3rysjjvfegauug3k"}
```

To decode a segwit address, one can use `segwit:decode/1` and will
have all required information about it retured as map.

```erlang
19> segwit:decode("test1pqypqxpq9qcrsszg2pvxq6rs0zqg3yyc5z5t"
19>               "pwxqergd3c8g7ruszzg3rysjjvfegauug3k").
{ok,#{hrp => "test",
      value => [1,2,3,4,5,6,7,8,9,10
               ,11,12,13,14,15,16,17
               ,18,19,20,21,22,23,24
               ,25,26|...],
      version => 1}
}
```

Let try all these marvelous new functions on real world data. The
first to test is with a nostr address, perhaps the most important
feature for this nostr client/server project. [Edward
Snowden](https://twitter.com/Snowden) nostr address is
`npub1sn0wdenkukak0d9dfczzeacvhkrgz92ak56egt7vdgzn8pv2wfqqhrjdv9`.

```erlang
20> bech32:decode("npub1sn0wdenkukak0d9dfczzeacvhkrgz92ak56egt7vd"
20>               "gzn8pv2wfqqhrjdv9").
{ok,#{checksum => [23,3,18,13,12,5],
      data => [16,19,15,14,13,25,19,22,28,22,29,22,15,13,5,13,9,
               24,2,2,25,29,24,12,23,22,3,8,2,5,10,29,22,20,26,
               25,8,11,30,12,13,8,2,19,7,1,12,10,14,9,0,0],
      format => bech32,
      hrp => "npub",
      origin => 
        "npub1sn0wdenkukak0d9dfczzeacvhkrgz92ak56egt7vdgzn8pv2wfqqhrjdv9"}
}
```

It looks good, the function deserialize the address, print the
checksum, return the data and gives the HRP. What about segwit address
now. To have a raw segwit address, the best way is to go to
[blockchain.com](https://www.blockchain.com/explorer/) and take the
first [transaction
displayed](https://www.blockchain.com/explorer/transactions/btc/9b53984bcde985a9b5bc7c729bc48ded84d5074b51633d6f8668a1ba74b72dba). The
transaction was from `bc1q52265p57jwxph407dqp9r9cucm3qvrqwg9kqpu` to
`bc1q0nwkgcvxhufess9keeszv7vw8xq8zmuy3upa7t`.

```erlang
21> segwit:decode("bc1q52265p57jwxph407dqp9r9cucm3qvrqwg9kqpu")
{ok,#{hrp => "bc",
      value => [162,149,170,6,158,147,140,27,213,254,104,2,81,
                151,28,198,226,6,12,14],
      version => 0}}
      
22> segwit:decode("bc1q0nwkgcvxhufess9keeszv7vw8xq8zmuy3upa7t").
{ok,#{hrp => "bc",
      value => [124,221,100,97,134,191,19,152,64,182,206,96,
                38,121,142,57,128,113,111,132],
      version => 0}}
```

It seems `bech32` and `segwit` modules can parse values from the wild.

## Under the Hood

The whole code is divided in small functions doing one thing, but
trying to do it well.

Charset and indexing is done using `index_to_charset/1` and
`charset_to_index/1` functions. Both are really simple one getting an
integer and converting it to another one (ASCII for example). To be
sure a character is available in the charset, the function
`is_charset/1` has been created, same has been done for the indexed
values with `is_index/1` function.

Strings or indexed strings must be tested, they are by using
respectively `valid_indexed_string/1` and `valid_string/1`. Both of
these functions are really important to ensure the data to be parsed
and serialized or deserialized.

Bech32 format is including lot of interesting function, in particular
a feature to checksum and reconstruct the message in case of
problem. The algorithm used is still a bit obscure at my level of
knowledge but seems to work perfectly. `polymod/1` function generates
the checksum needed to reconstruct and ensure the data sent have been
correctly received. A macro has been used here to simulate a loop as a
pipeline.

```erlang
% ...
-define(POLYMOD_GENERATOR(INDEX, GENERATOR),
    polymod_generator(INDEX, Checksum, Top) ->
        case (Top bsr INDEX) band 1 of
            0 ->
                polymod_generator(INDEX+1, Checksum bxor 0, Top);
            _Value ->
                polymod_generator(INDEX+1, Checksum bxor GENERATOR, Top)
        end
    ).
% ...
?POLYMOD_GENERATOR(0, 16#3B6A_57B2);
?POLYMOD_GENERATOR(1, 16#2650_8E6D);
?POLYMOD_GENERATOR(2, 16#1EA1_19FA);
?POLYMOD_GENERATOR(3, 16#3D42_33DD);
?POLYMOD_GENERATOR(4, 16#2A14_62B3);
polymod_generator(5, Checksum, _Top) -> Checksum.
```

The rest is mainly functions used to convert and validate the value,
this previous one is probably the most completed created in this
project. Decoding and encoding functions are executed as instruction
flow, one after another one until the final result is being returned
by `encode` or `decode` functions.

At this time, the coverage of these two modules, `segwit` and `bech32`
are quite pretty good:

| module  |  coverage  |
|---------|------------|
| [bech32](https://github.com/erlang-punch/nostr/blob/main/src/bech32.erl)  | 99%  |
| [segwit](https://github.com/erlang-punch/nostr/blob/main/src/segwit.erl)  | 97%  |

Here a quick analysis of these modules with
[`scc`](https://github.com/boyter/scc).

| Language |         File | Lines | Blanks | Comments |  Code | Complexity |
| :------- |        ----: | ----: |  ----: |    ----: | ----: |      ----: |
| Erlang   | `bech32.erl` |  1316 |     75 |      413 |   828 |         10 |
| Erlang   | `segwip.erl` |   263 |     18 |      135 |   110 |          2 |

In total, the full bech32/segwit implementation with all the coverage
was made in 1579 line of Erlang code (including comments and blank
lines).

| Language | Files | Lines | Blanks | Comments | Code | Complexity |
| :------- | ----: | ----: | -----: | -------: | ---: | ---------: |
| Erlang   |     2 |  1579 |     93 |      548 |  938 |         12 |
| Total    |     2 |  1579 |     93 |      548 |  938 |         12 |

The final result of estimated cost, schedule effort and people
required:

 - **Estimated Cost to Develop** (organic): $25,258
 - **Estimated Schedule Effort** (organic): 3.398828 months
 - **Estimated People Required** (organic): 0.660230

In fact, this implementation in Erlang takes around 3 days with one
developer dedicated full time but that's an estimation, other analysis
tools could have produced more accurate results.

## Conclusion, Optimizations and Future Modifications

This is a first implementation, made from scratch based on Python
model implementation, and lot of features are missing, even if this
implementation seems flexible, a bit of cleanup will be required.

This code is not optimized, it has been created quickly but it was
correctly tested. Unfortunately, if this code is used in production
environment, a clear performance problem will happen. This is why it
is required to have an idea how to improve this implementation.

Proper (property based testing) and Dialyzer (static-analysis) needs
to be added as well as profiling References.

Anyway, it was fun to create bech32 and segwit format in pure
Erlang. I hope I will find more time to improve it in a near future,
and perhaps use it outside of nostr.

## Alternative Implementations

Erlang is perhaps not the best language to deal with bech32 and
segwit. Firstly because Erlang is not the best language for doing
mathematics, but also because bech32 and segwit have been designed for
more classical language. You can, though, find small implementation,
more tested and optimized in other languages. Here a list of other
bech32 and segwit implementation you can easily find on Github.

  | Project   | Language |               File | Lines | Blanks | Comments |  Code | Complexity |
  | :------   |    :---- |        ----------: |   --: |    --: |     ----: |   --: |        --: |
  | [bech32](https://github.com/sipa/bech32) | C | `segwit_addr.c` | 209 | 11 | 20 | 178 | 90 |
  | [bech32](https://github.com/sipa/bech32) | C++ | `bech32.cpp` | 226 | 27 | 81 | 118 | 43 |
  | [bech32](https://github.com/sipa/bech32) | C++ | `segwit_addr.cpp` | 86 | 10 | 23 | 53 | 27 |
  | [bech32](https://github.com/sipa/bech32) | Go | `bech32.go` | 217 | 12 | 25 | 180 | 78 |
  | [bech32](https://github.com/sipa/bech32) | Haskell | `Bech32.hs` | 159 | 25 | 10 | 124 | 18 |
  | [bech32](https://github.com/sipa/bech32) | JavaScript | `bech32.js` | 131 | 10 | 19 | 102| 28 |
  | [bech32](https://github.com/sipa/bech32) | JavaScript | `segwit_addr.js` | 91 | 5 | 19 | 67 | 26 |
  | [bech32](https://github.com/sipa/bech32) | Python | `segwit_addr.py` | 137 | 13 | 26 | 98 | 11 |
  | [bech32](https://github.com/sipa/bech32) | Ruby | `bech32.rb` | 99 | 14 | 30 | 55 | 12 |
  | [bech32](https://github.com/sipa/bech32) | Ruby | `segwit_addr.rb` | 86 | 11 | 19 | 56 | 36 |
  | [bech32](https://github.com/sipa/bech32) | Rust | `bech32.rs` | 230 | 23 | 60 | 147 | 45 |
  | [bech32](https://github.com/sipa/bech32) | Rust | `wit_prog.rs` | 219 | 11 | 73 | 135 | 36 |
  | [bitcoinj](https://github.com/bitcoinj/bitcoinj) | Java | `Bech32.java` | 188 | 17 | 35 | 136 | 38 |
  | [bitcoinj](https://github.com/bitcoinj/bitcoinj) | Java | `SegwitAddress.java` | 464 | 42 | 196 | 226 | 58 |
  | [bitcoinaddress](https://github.com/fortesp/bitcoinaddress) | Python | `segwit_addr.py` | 123 | 13 | 25 | 85 | 7 |
  | [btclib](https://github.com/btclib-org/btclib) | Python | `bech32.py` | 146 | 28 | 56 | 62 | 28 |
  | [ocaml-bech32](https://github.com/vbmithr/ocaml-bech32) | OCaml | `bech32.ml` | 468 | 43 | 22 | 403 | 54 |
  | [bitcoin-address-validator](https://github.com/kielabokkie/bitcoin-address-validator) | PHP | `Bech32.php` | 342 | 61 | 80 | 201 | 43 |
  | [bech32](https://github.com/akovalenko/bech32) | Lisp | `bech32.lisp` | 248 | 25 | 0 | 223 | 19 |
  | [bech32](https://github.com/input-output-hk/bech32) | Haskell | `Internal.hs` | 919 | 78 | 293 | 548 | 49 |
  | [bech32-elixir](https://github.com/f2pool/bech32-elixir) | Elixir | `bech32.ex` | 356 | 47 | 14 | 299 | 12 |
  
Based on `scc`, the less complex is using Python and the most complex
is the one in C (headers were not included).

## Resources 

 - [Bitcoin Wiki - BIP 0350](https://en.bitcoin.it/wiki/BIP_0350),
   [https://en.bitcoin.it/wiki/BIP_0350](https://en.bitcoin.it/wiki/BIP_0350)
 
 - [Bitcoin Wiki - Bech32](https://en.bitcoin.it/wiki/Bech32),
   [https://en.bitcoin.it/wiki/Bech32](https://en.bitcoin.it/wiki/Bech32)
 
 - [Bitcoin Wiki - Bech32
   Adoption](https://en.bitcoin.it/wiki/Bech32_adoption),
   [https://en.bitcoin.it/wiki/Bech32_adoption](https://en.bitcoin.it/wiki/Bech32_adoption)
 
 - [Nostr Protocol -
   NIP/19](https://github.com/nostr-protocol/nips/blob/master/19.md),
   [https://github.com/nostr-protocol/nips/blob/master/19.md](https://github.com/nostr-protocol/nips/blob/master/19.md)
 
 - [bech32-buffer](https://slowli.github.io/bech32-buffer/),
   [https://slowli.github.io/bech32-buffer/](https://slowli.github.io/bech32-buffer/)
 
 - [Bech32 reference implementation source
   code](https://github.com/sipa/bech32/tree/master/ref),
   [https://github.com/sipa/bech32/tree/master/ref](https://github.com/sipa/bech32/tree/master/ref)
 
 - [Base58 Check
   Encoding](https://en.bitcoin.it/wiki/Base58Check_encoding),
   [https://en.bitcoin.it/wiki/Base58Check_encoding](https://en.bitcoin.it/wiki/Base58Check_encoding)

 - [(Some of) the math behind Bech32
   addresses](https://medium.com/@meshcollider/some-of-the-math-behind-bech32-addresses-cf03c7496285)
   by [Samuel Dobson](https://medium.com/@meshcollider) on medium,
   [https://medium.com/@meshcollider/some-of-the-math-behind-bech32-addresses-cf03c7496285](https://medium.com/@meshcollider/some-of-the-math-behind-bech32-addresses-cf03c7496285)

 - [Understanding Base58 Encoding - It is all about
   integers](https://medium.com/concerning-pharo/understanding-base58-encoding-23e673e37ff6)
   by [Sven VC](https://medium.com/@svenvc),
   [https://medium.com/concerning-pharo/understanding-base58-encoding-23e673e37ff6](https://medium.com/concerning-pharo/understanding-base58-encoding-23e673e37ff6)

 - [Bech32 Racket
   Implementation](https://docs.racket-lang.org/bech32/index.html),
   [https://docs.racket-lang.org/bech32/index.html](https://docs.racket-lang.org/bech32/index.html)
