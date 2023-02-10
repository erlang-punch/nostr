%%%===================================================================
%%% @doc `nostrlib' contains all functions commonly used by relays and
%%% clients. This is a low level interface and should not be directly
%%% used.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib).
-export([encode/1]).
-export([since/0, since/1]).
-export([kind/1, kinds/1]).
-include_lib("eunit/include/eunit.hrl").
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% extra-specification to deal with eunit.
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%% @doc encode/1 is a wrapper around thoas:encode/1 for the moment.
%% see <a href="https://github.com/lpil/thoas">Thoas</a>
%% @end
%%--------------------------------------------------------------------
-spec encode(Message) -> Return when
      Message :: message(),
      Return :: iodata().
encode(Message) ->
    thoas:encode(Message).

%%--------------------------------------------------------------------
%% @doc kind/1 function helps to convert nostr kind from atom to
%%      integer and integer from atom.
%% @end
%%--------------------------------------------------------------------
-spec kind(Kind) -> Return when
      Kind :: kind(),
      Return :: kind().

?KIND(0, metadata);
?KIND(1, short_text_note);
?KIND(2, recommended_relay);
?KIND(3, contacts);
?KIND(4, encrypted_direct_messages);
?KIND(5, event_deletion);
?KIND(7, reaction).

%%--------------------------------------------------------------------
%% @doc kinds/1 function converts a list of atoms into integers.
%% @end
%%--------------------------------------------------------------------
-spec kinds(Kinds) -> Return when
      Kinds :: kinds(),
      Return :: [pos_integer()].

kinds(Kinds) ->
    Converter = fun(Kind) when is_atom(Kind) -> kind(Kind);
                   (Kind) when is_integer(Kind) -> Kind
                end,
    lists:map(Converter, Kinds).

%%--------------------------------------------------------------------
%% @doc wrapper around since/1.
%% @end
%% @TODO create spec
%%--------------------------------------------------------------------
-spec since() -> Return when
      Return :: pos_integer().

since() ->
    since(10).

%%--------------------------------------------------------------------
%% @doc since/1 returns a Posix Timestamp with a shift in second.
%% @end
%% @TODO create spec
%%--------------------------------------------------------------------
-spec since(Shift) -> Return when
      Shift :: pos_integer(),
      Return :: pos_integer().

since(Shift)
  when Shift > 0 ->
    UniversalTime = erlang:universaltime(),
    PosixTime = erlang:universaltime_to_posixtime(UniversalTime),
    PosixTime-Shift.
