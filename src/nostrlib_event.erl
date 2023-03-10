%%%===================================================================
%%% @doc
%%%
%%% == Examples ==
%%%
%%% ```
%%% nostrlib_event:create(metadata, #{}
%%% '''
%%%
%%% Here the manual steps to check a message. 
%%%
%%% ```
%%% % 1. read the file (or take it from the wild)
%%% {ok, M} = file:read_file("test/nostrlib_SUITE_data/valid_event_kind1.json").
%%% 
%%% % 2. decode the json
%%% {ok, J} = thoas:decode(M).
%%%
%%% % 3. decode the message and convert it to record
%%% {ok,{_,_,E}} = nostrlib_decode:message(J).
%%%
%%% % 4. serialize the message
%%% X = nostrlib_event:serialize(E).
%%%
%%% % 5. create the hash and convert others values from hex to binary
%%% HashMessage = crypto:hash(sha256, X).
%%% PublicKey = nostrlib:hex_to_binary(E#event.public_key).
%%% Signature = nostrlib:hex_to_binary(E#event.signature).
%%%
%%% % 6. verify the message with the public key and the signature.
%%% true = nostrlib_schnorr:verify(HashMessage, PublicKey, Signature).
%%% '''
%%%
%%% @end
%%%===================================================================
-module(nostrlib_event).



