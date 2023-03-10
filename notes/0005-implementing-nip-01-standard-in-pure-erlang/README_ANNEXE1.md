# ANNEXES2 - Identity Draft

The identity will alter the behavior of the client by configuring
default values. An identity is encrypted and stored in a vault.

 0. An identity MUST BE defined by an ID (randomly chosen or not)
 1. An identity MUST CONTAIN the user private key
 2. An identity CAN BE encrypted
 3. An identity CAN HAVE a creation date
 4. An identity CAN HAVE a recommended server
 5. An identity CAN EMBED metadata
 6. An identity CAN HAVE custom configuration
 7. An identity CAN HAVE custom labels

```erlang
% create a new identity
% nostr_identity:new/0 create a fully random identity with 
% high security level by default
Identity = nostr_identity:new().

% nostr_identity:new/1 creates with a new name with high
% security level by default
Identity = nostr_identity:new(Name).

% nostr_identity:new/2 creates a new custom account, it can be
% used to import identity.
{ok, Identity} = nostr_identity:new(Name, Opts).
{ok, Identity} = nostr_identity:new(<<"my name">>, [
    % password used to encrypt the content of the data-structure
    % and stored in the vault. It is also used to encrypt the
    % private_key by default
    {password, Password},
    
    % private key defined by the user or automatically created
    % when an identity is created
    {private_key, PrivateKey},
    
    % generated using the private key during the creation of
    % the identity
    {public_key, PublicKey},
    
    % random seed created when the identity is created
    {seed, crypto:strong_rand_bytes(64)}
    
    % created when the identity is created
    {created_at, erlang:system_time()},
    
    % recommend_server event sent to the relay
    {recommend_server, <<"wss://myrelay.local">>},
    
    % metadata event sent to the relay
    {metadata, #{ 
            name => <<"my name">>, 
            about => <<"about">>,
            picture => <<"url">>
        }
    },
    
    % a custom configuration to alter the behavior or the
    % connection when identity is used.
    {configuration, #{
            relays => #{
                <<"wss://...">> => <<"wss://...">>,
                <<"wss://...">> => <<"wss://...">>
            },
            subscriptions => #{
                <<"wss://...">> => [],
                <<"wss://...">> => []
            },
            actions => #{}
        }
    },
    
    % labels are used to check if the client or the relay
    % is supporting a list of features.
    {labels, #{
            <<"connection/tor">> => true,
            <<"connection/i2p">> => true,
            <<"connection/proxy">> => true,
            <<"vault">> => true,
            <<"nip/01">> => true,
        }
    }
]).

% list available identity (hashed form)
{ok, List} = nostr_identity:list().

% get an identity
{ok, Identity} = nostr_identity:get(Name).
{ok, Identity} = nostr_identity:get(Name, Opts).
{ok, Result} = nostr_identity:save(Name).
{ok, Identity} = nostr_identity:export(Name).

% create a new connection using one identity
{ok, Connection} = nostr_client:start("wss://relay.local", [{identity, Identity}]).
```
