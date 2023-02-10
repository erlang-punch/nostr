%%%===================================================================
%%% @doc
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostrlib_secp256k1).
-export([create_keys/0]).
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_keys() -> Return when
      Return :: {PublicKey, PrivateKey},
      PublicKey :: iodata(),
      PrivateKey :: iodata().
create_keys() ->
    crypto:generate_key(ecdh, secp256k1).
