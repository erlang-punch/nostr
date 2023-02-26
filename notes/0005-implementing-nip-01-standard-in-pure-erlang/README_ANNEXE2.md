# ANNEXE2 - Types, Data-Structures and Definition Summary

Here the common types/values you can find in nostr protocol.

| type name     | type           | value                                                |
|---------------|----------------|------------------------------------------------------|
| `#id`         | `string`       | a 32 bytes (256bits) lowercase hexadecimal string    |
| `#public_key` | `string`       | a 32bytes (256bits) lowercase hexadecimal string     |
| `#prefix`     | `string`       | a 0 to 32bytes (256bits) hexadecimal string          |
| `#kind`       | `integer`      | a positive integer                                   |
| `#signature`  | `string`       | a 64bytes (512bits) lowercase hexadecimal string     |
| `#timestamp`  | `integer`      | a UNIX/POSIX timestamp                               |
| `#tag`        | `[string,...]` | a list of string or integer                          |


| Object   | attribute    | type                | example                               |
|----------|--------------|---------------------|---------------------------------------|
| `Event`  | `id`         | `#id`               | `"f986c724a5085ffe093e8145ef953e..."` |
| `Event`  | `pubkey`     | `#public_key`       | `"79be667ef9dcbbac55a06295ce870b..."` |
| `Event`  | `created_at` | `#timestamp`        | `1678325509`                          |
| `Event`  | `kind`       | `[#kind,...] `      | `0`                                   |
| `Event`  | `tags`       | `[#tag,...]`        | `[["p","79be667ef9dcbbac55a062..."]`  |
| `Event`  | `content`    | `string`            | `"test`                               |
| `Event`  | `sig`        | `#signature`        | `"140d53496710b06dbc8ce239a454bf..."` |
| `Filter` | `ids`        | `[#prefix,...]`     | `["f986c724a5085ffe093e8145"]`        |
| `Filter` | `authors`    | `[#prefix,...]`     | `["79be667ef9dcbbac55a06295ce87..."]` |
| `Filter` | `kinds`      | `[#kind,...]`       | `[0,1,2]`                             |
| `Filter` | `#e`         | `[#id,...]`         | `["f986c724a5085ffe093e8145ef95..."]` |
| `Filter` | `#p`         | `[#public_key,...]` | `["79be667ef9dcbbac55a06295ce87..."]` |
| `Filter` | `since`      | `#timestamp`        | `1678325509`                          |
| `Filter` | `until`      | `#timestamp`        | `1678325509`                          |
| `Filter` | `limit`      | `integer`           | `10`                                  |
| `Close`  | `subscription_id` | `string`       | `"randomstring"`                      |
| `Notice` | `message     | `string`            | `"a valid message"`                   |

| Object  | Attribute    | nostr record | nostr record field    |
|---------|--------------|--------------|-----------------------|
| `Event` | `id`         | `#event{}`   | `E#event.id`          |
| `Event` | `pubkey`     | `#event{}`   | `E#event.public_key`  |
| `Event` | `created_at` | `#event{}`   | `E#event.created_at`  |
| `Event` | `kind`       | `#event{}`   | `E#event.kind`        |

`nostrlib` Erlang library will be divided in many modules, some of
them will be available for common developers, and others will be used
as internal functions to deal with different kind of data or
implementing new features.

## Regular Expression Definition

Note: those regexes have not been tested. They are put it as reminder.

```erlang
{ok, Regex_PrivateKey} = re:compile(<<"^[0-9a-f]{64}$">>, [extended,anchored]).
Regex_PrivateKey = Regex_PublicKey = Regex_EventId.

{ok, Regex_Signature} = re:compile( <<"^[0-9a-f]{128}$">>, [extended,anchored]).

{ok, Regex_Prefix} = re:compile(<<"^[0-9a-f]{48,64}$">>, [extended,anchored]).

{ok, Regex_Content} = re:compile(<<"^$">>, [unicode,anchored,noteol])
```
