%%%===================================================================
%%% @doc type and data-structure used in nostrlib_decoder.erl
%%% module. Can be easily imported in other modules as needed.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-type to_be_defined() :: any().
%% A type created to point out a type to define.

%%--------------------------------------------------------------------
%% A macro used to translate a kind as integer or atom.
%%--------------------------------------------------------------------
-define(KIND(K_INTEGER, K_ATOM),
  kind(K_INTEGER) -> {ok, K_ATOM};
  kind(K_ATOM) -> {ok, K_INTEGER}
).

%%%===================================================================
%%% @doc encoded JSON messages
%%% @end
%%%===================================================================
-type encoded_event() :: binary() | bitstring() | iodata().

%%%===================================================================
%%% @doc decoded types and data-structures
%%% @end
%%%===================================================================
-type decoded_event_id() :: undefined | <<_:256>>.
%% event_id() is an event id as defined in NIP/01.

-type decoded_private_key() :: <<_:256>>.
%% A private key as a bitstring.

-type decoded_public_key() :: <<_:256>>.
%% A public key as a bistring.

-type decoded_created_at() :: pos_integer().
%% An Unix timestamp as integer.

-type decoded_kind() :: atom() | 0 | 1 | 2 | 7 | pos_integer().
%% A kind represented as positive integer

-type decoded_kinds() :: [decoded_kind(), ...].
%% A list of kinds

-type decoded_content() :: bitstring() | iodata().
%% The main payload of the message as raw string.

-type decoded_signature() :: <<_:512>>.
%% A signature as an hexadecimal string.

-type decoded_event_ids() :: [decoded_event_id(), ...].
%% A list of event id.

-type decoded_prefix() :: bitstring().
%% A prefix as an hexadecimal string.

-type decoded_author() :: decoded_public_key() | decoded_prefix().
%% An author as defined in NIP/01, can be a public key or a prefix.

-type decoded_authors() :: [decoded_author(), ...].
%% A list of authors.

-type decoded_tag_event_ids() :: decoded_event_ids().
%% A tag event_ids, an alias for event id used in tags.

-type decoded_tag_event_public_keys() :: [decoded_public_key(), ...].
%% A tag containing a list of public keys.

-type decoded_since() :: pos_integer().
%% An Unix timestamp as positive integer.

-type decoded_until() :: pos_integer().
%% An Unix timestamp as positive integer.

-type decoded_limit() :: pos_integer().
%% A limit of event as positive integer

-type decoded_message() :: bitstring().
%% A raw message, used in notice.

-type decoded_subscription_id() :: bitstring().
%% A subscription id as a random string.

%%--------------------------------------------------------------------
%% A tag record.
%%--------------------------------------------------------------------
-record(tag, { name   = undefined :: public_key | event_id
             , value  = undefined :: undefined | bitstring()
             , params = []        :: list()
             }).

-type decoded_tag() :: #tag{ params :: undefined }.
%% A tag as represented in NIP/01. It can describe an event id or a
%% public key.

-type decoded_tags() :: [decoded_tag(), ...].
%% A list of tag, used in events.

%%--------------------------------------------------------------------
%% A full event record.
%%--------------------------------------------------------------------
-record(event, { id         = undefined :: decoded_event_id()
               , public_key = undefined :: decoded_public_key()
               , created_at = undefined :: decoded_created_at()
               , kind       = undefined :: decoded_kind()
               , tags       = []        :: decoded_tags()
               , content    = undefined :: decoded_content()
               , signature  = undefined :: decoded_signature()
               }).
-type decoded_event() :: #event{signature :: decoded_signature()}.

%%--------------------------------------------------------------------
%% A full filter record.
%%--------------------------------------------------------------------
-record(filter, { event_ids       = undefined :: decoded_event_ids()
                , authors         = undefined :: decoded_authors()
                , kinds           = undefined :: decoded_kinds()
                , tag_event_ids   = undefined :: decoded_tag_event_ids()
                , tag_public_keys = undefined :: decoded_tag_event_public_keys()
                , since           = undefined :: decoded_since()
                , until           = undefined :: decoded_until()
                , limit           = undefined :: decoded_limit()
                }).
-type decoded_filter() :: #filter{limit :: decoded_limit()}.

%%--------------------------------------------------------------------
%% A full request record.
%%--------------------------------------------------------------------
-record(request, { subscription_id = undefined :: decoded_subscription_id()
                 , filter = #filter{} :: [decoded_filter(), ...]
                 }).
-type decoded_request() :: #request{filter :: [decoded_filter(), ...]}.

%%--------------------------------------------------------------------
%% A full close record.
%%--------------------------------------------------------------------
-record(close, { subscription_id = undefined :: decoded_subscription_id()
               }).
-type decoded_close() :: #close{subscription_id :: decoded_subscription_id()}.

%%--------------------------------------------------------------------
%% A full notice record.
%%--------------------------------------------------------------------
-record(notice, { message = undefined :: decoded_message() }).
-type decoded_notice() :: #notice{ message :: decoded_message() }.

%%--------------------------------------------------------------------
%% A full eose record.
%%--------------------------------------------------------------------
-record(eose, { id = undefined :: decoded_subscription_id() }).
-type decoded_eose() :: #eose{ id :: decoded_subscription_id() }.

%%--------------------------------------------------------------------
%% A full subscription record.
%%--------------------------------------------------------------------
-type decoded_subscription_content() :: decoded_event()
                                      | decoded_request().
-record(subscription, { id      = undefined :: decoded_subscription_id()
                      , content = undefined :: decoded_subscription_content()
                      }).
-type decoded_subscription() :: #subscription{ content :: decoded_subscription_content() }.

%%--------------------------------------------------------------------
%% A decoded "OK" message from relay to clients.
%%--------------------------------------------------------------------
-type decoded_ok_event_id() :: decoded_event_id().
-type decoded_ok_accepted() :: boolean().
-type decoded_ok_prefix()   :: binary().
-type decoded_ok_message()  :: binary().
-record(ok, { event_id = <<>>  :: decoded_ok_event_id()
            , accepted = false :: decoded_ok_accepted()
            , prefix   = <<>>  :: decoded_ok_prefix()
            , message = <<>>   :: decoded_ok_message()
            }).
-type decoded_ok() :: #ok{}.

%%--------------------------------------------------------------------
%% A type representing all decoded messages available.
%%--------------------------------------------------------------------
-type decoded_messages() :: decoded_event()
                          | decoded_request()
                          | decoded_close()
                          | decoded_notice()
                          | decoded_subscription()
                          | decoded_eose().

%%%===================================================================
%%% @doc encoded types and data-structure.
%%% @end
%%%===================================================================
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
