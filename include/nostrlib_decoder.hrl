%%%===================================================================
%%% @doc type and data-structure used in nostrlib_decoder.erl
%%% module. Can be easily imported in other modules as needed.
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-type event_id() :: undefined | bitstring().
%% event_id() is an event id as defined in NIP/01.

-type public_key() :: bitstring().
%% A public key as an hexdecimal string.

-type created_at() :: pos_integer().
%% An Unix timestamp as integer.

-type kind() :: 0 | 1 | 2 | 7 | pos_integer().
%% A kind represented as positive integer

-type kinds() :: [kind(), ...].
%% A list of kinds

-type tag() :: [bitstring(), ...].
%% A tag as represented in NIP/01. It can describe an event id or a
%% public key.

-type tags() :: [tag(), ...].
%% A list of tag, used in events.

-type content() :: bitstring().
%% The main payload of the message as raw string.

-type signature() :: bitstring().
%% A signature as an hexadecimal string.

-type event_ids() :: [event_id(), ...].
%% A list of event id.

-type prefix() :: bitstring().
%% A prefix as an hexadecimal string.

-type author() :: public_key() | prefix().
%% An author as defined in NIP/01, can be a public key or a prefix.

-type authors() :: [author(), ...].
%% A list of authors.

-type tag_event_ids() :: event_ids().
%% A tag event_ids, an alias for event id used in tags.

-type tag_event_public_keys() :: [public_key(), ...].
%% A tag containing a list of public keys.

-type since() :: pos_integer().
%% An Unix timestamp as positive integer.

-type until() :: pos_integer().
%% An Unix timestamp as positive integer.

-type limit() :: pos_integer().
%% A limit of event as positive integer

-type message() :: bitstring().
%% A raw message, used in notice.

-type subscription_id() :: bitstring().
%% A subscription id as a random string.

%%--------------------------------------------------------------------
%% A full event record.
%%--------------------------------------------------------------------
-record(event, { id :: event_id()
               , public_key :: public_key()
               , created_at :: created_at()
               , kind :: kind()
               , tags :: tags()
               , content :: content()
               , signature :: signature()
               }).
-type event() :: #event{signature :: signature()}.

%%--------------------------------------------------------------------
%% A full filter record.
%%--------------------------------------------------------------------
-record(filter, { event_ids :: event_ids()
                , authors :: authors()
                , kinds :: kinds()
                , tag_event_ids :: tag_event_ids()
                , tag_public_keys :: tag_event_public_keys()
                , since :: since()
                , until :: until()
                , limit :: limit()
                }).
-type filter() :: #filter{limit :: limit()}.

%%--------------------------------------------------------------------
%% A full request record.
%%--------------------------------------------------------------------
-record(request, { subscription_id :: subscription_id()
                 , filter :: filter()
                 }).
-type request() :: #request{filter :: filter()}.

%%--------------------------------------------------------------------
%% A full close record.
%%--------------------------------------------------------------------
-record(close, { subscription_id :: subscription_id() }).
-type close() :: #close{subscription_id :: subscription_id()}.

%%--------------------------------------------------------------------
%% A full notice record.
%%--------------------------------------------------------------------
-record(notice, { message :: message() }).
-type notice() :: #notice{}.

%%--------------------------------------------------------------------
%% A full eose record.
%%--------------------------------------------------------------------
-record(eose, { id :: subscription_id() }).
-type eose() :: #eose{}.

%%--------------------------------------------------------------------
%% A full subscription record.
%%--------------------------------------------------------------------
-type subscription_content() :: event()
                              | request().
-record(subscription, { id :: subscription_id()
                      , content :: subscription_content()
                      }).
-type subscription() :: #subscription{}.

%%--------------------------------------------------------------------
%% A type representing all decoded messages available.
%%--------------------------------------------------------------------
-type decoded_messages() :: event()
                          | request()
                          | close()
                          | notice()
                          | subscription()
                          | eose().
