%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-type to_be_defined() :: any().
%% A type created to point out a type to define.

-type message() :: list().
%% A nostr message.

-type kind() :: pos_integer() | atom().
%% A nostr kind.

-type kinds() :: [kind(), ...].
%% A list of nostr kind.

-type filters() :: map() | [map(), ...].
%% A nostr filter.

-type subscription_id() :: binary() | bitstring() | iodata().
%% A subscription id.

-type event() :: map().
%% A nostr event.

%%--------------------------------------------------------------------
%% A macro used to translate a kind as integer or atom.
%%--------------------------------------------------------------------
-define(KIND(K_INTEGER, K_ATOM),
  kind(K_INTEGER) -> K_ATOM;
  kind(K_ATOM) -> K_INTEGER
).
