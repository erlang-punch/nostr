# nostr

![Erlang Punch Nostr License](https://img.shields.io/github/license/erlang-punch/nostr)
![Erlang Punch Nostr Top Language](https://img.shields.io/github/languages/top/erlang-punch/nostr)
![Erlang Punch Nostr Workflow Status (main branch)](https://img.shields.io/github/actions/workflow/status/erlang-punch/nostr/test.yaml?branch=main)
![Erlang Punch Nostr Last Commit](https://img.shields.io/github/last-commit/erlang-punch/nostr)
![Erlang Punch Nostr Code Size (bytes)](https://img.shields.io/github/languages/code-size/erlang-punch/nostr)
![Erlang Punch Nostr Repository File Count](https://img.shields.io/github/directory-file-count/erlang-punch/nostr)
![Erlang Punch Nostr Repository Size](https://img.shields.io/github/repo-size/erlang-punch/nostr)

An implementation of nostr in pure Erlang using OTP stack with minimal
dependencies. Here the list of features offered by this application:

 - [ ] A library defined in module `nostrlib`

 - [ ] A client defined in module `nostr_client`

 - [ ] A relay defined in module `nostr_relay`

 - [ ] A common interface called `nostr` to play with previously
       defined modules.

## Disclamer

This project is in active development and cannot provide a stable
application. This application should be used only for test at the
moment.

## NIP Support

Here the list of currently supported
[nips](https://github.com/nostr-protocol/nips):

 - [x] [nip/01: Basic protocol flow description](https://github.com/nostr-protocol/nips/blob/master/01.md)
 - [ ] [nip/02: Contact List and Petnames](https://github.com/nostr-protocol/nips/blob/master/02.md)
 - [ ] [nip/03: OpenTimestamps Attestations for Events](https://github.com/nostr-protocol/nips/blob/master/03.md)
 - [ ] [nip/04: Encrypted Direct Message](https://github.com/nostr-protocol/nips/blob/master/04.md)
 - [ ] [nip/05: Mapping Nostr keys to DNS-based internet identifiers](https://github.com/nostr-protocol/nips/blob/master/05.md)
 - [ ] [nip/06: Basic key derivation from mnemonic seed phrase](https://github.com/nostr-protocol/nips/blob/master/06.md)
 - [ ] [nip/07: `window.nostr` capability for web browsers](https://github.com/nostr-protocol/nips/blob/master/07.md)
 - [ ] [nip/08: Handling Mentions](https://github.com/nostr-protocol/nips/blob/master/08.md)
 - [ ] [nip/09: Event Deletion](https://github.com/nostr-protocol/nips/blob/master/09.md)
 - [ ] [nip/10: On "e" and "p" tags in Text Events (kind 1)](https://github.com/nostr-protocol/nips/blob/master/10.md)
 - [ ] [nip/11: Relay Information Document](https://github.com/nostr-protocol/nips/blob/master/11.md)
 - [ ] [nip/12: Generic Tag Queries](https://github.com/nostr-protocol/nips/blob/master/12.md)
 - [ ] [nip/13: Proof of Work](https://github.com/nostr-protocol/nips/blob/master/13.md)
 - [ ] [nip/14: Subject tag in Text events](https://github.com/nostr-protocol/nips/blob/master/14.md)
 - [ ] [nip/15: End of Stored Events Notice](https://github.com/nostr-protocol/nips/blob/master/15.md)
 - [ ] [nip/16: Event Treatment](https://github.com/nostr-protocol/nips/blob/master/16.md)
 - [ ] [nip/18: Reposts](https://github.com/nostr-protocol/nips/blob/master/18.md)
 - [ ] [nip/19: bech32-encoded entities](https://github.com/nostr-protocol/nips/blob/master/19.md)
 - [ ] [nip/20: Command Results](https://github.com/nostr-protocol/nips/blob/master/20.md)
 - [ ] [nip/21: URL scheme](https://github.com/nostr-protocol/nips/blob/master/21.md)
 - [ ] [nip/22: Event `created_at` Limits](https://github.com/nostr-protocol/nips/blob/master/22.md)
 - [ ] [nip/23: Long-form Content](https://github.com/nostr-protocol/nips/blob/master/23.md)
 - [ ] [nip/25: Reactions](https://github.com/nostr-protocol/nips/blob/master/25.md)
 - [ ] [nip/26: Delegated Event Signing](https://github.com/nostr-protocol/nips/blob/master/26.md)
 - [ ] [nip/27: Text Note References](https://github.com/nostr-protocol/nips/blob/master/27.md)
 - [ ] [nip/28: Public Chat](https://github.com/nostr-protocol/nips/blob/master/28.md)
 - [ ] [nip/30: Custom Emoji](https://github.com/nostr-protocol/nips/blob/master/30.md)
 - [ ] [nip/31: Dealing with unknown event kinds](https://github.com/nostr-protocol/nips/blob/master/31.md)
 - [ ] [nip/32: Labeling](https://github.com/nostr-protocol/nips/blob/master/32.md)
 - [ ] [nip/33: Parameterized Replaceable Events](https://github.com/nostr-protocol/nips/blob/master/33.md)
 - [ ] [nip/36: Sensitive Content/Content Warning](https://github.com/nostr-protocol/nips/blob/master/36.md)
 - [ ] [nip/38: User Statuses](https://github.com/nostr-protocol/nips/blob/master/38.md)
 - [ ] [nip/39: External Identities in Profiles](https://github.com/nostr-protocol/nips/blob/master/39.md)
 - [ ] [nip/40: Expiration Timestamp](https://github.com/nostr-protocol/nips/blob/master/40.md)
 - [ ] [nip/42: Authentication of clients to relays](https://github.com/nostr-protocol/nips/blob/master/42.md)
 - [ ] [nip/45: Event Counts](https://github.com/nostr-protocol/nips/blob/master/45.md)
 - [ ] [nip/46: Nostr Connect](https://github.com/nostr-protocol/nips/blob/master/46.md)
 - [ ] [nip/47: Nostr Wallet Connect](https://github.com/nostr-protocol/nips/blob/master/47.md)
 - [ ] [nip/48: Proxy Tags](https://github.com/nostr-protocol/nips/blob/master/48.md)
 - [ ] [nip/50: Search Capability](https://github.com/nostr-protocol/nips/blob/master/50.md)
 - [ ] [nip/51: Lists](https://github.com/nostr-protocol/nips/blob/master/51.md)
 - [ ] [nip/52: Calendar Events](https://github.com/nostr-protocol/nips/blob/master/52.md)
 - [ ] [nip/53: Live Activities](https://github.com/nostr-protocol/nips/blob/master/53.md)
 - [ ] [nip/56: Reporting](https://github.com/nostr-protocol/nips/blob/master/56.md)
 - [ ] [nip/57: Lightning Zaps](https://github.com/nostr-protocol/nips/blob/master/57.md)
 - [ ] [nip/58: Badges](https://github.com/nostr-protocol/nips/blob/master/58.md)
 - [ ] [nip/65: Relay List Metadata](https://github.com/nostr-protocol/nips/blob/master/65.md)
 - [ ] [nip/78: Arbitrary custom app data](https://github.com/nostr-protocol/nips/blob/master/78.md)
 - [ ] [nip/89: Recommended Application Handlers](https://github.com/nostr-protocol/nips/blob/master/89.md)
 - [ ] [nip/94: File Metadata](https://github.com/nostr-protocol/nips/blob/master/94.md)
 - [ ] [nip/98: HTTP Auth](https://github.com/nostr-protocol/nips/blob/master/98.md)

## Other Implementation (required by nostr)

 - [x] [BIP-0340: Schnorr Signatures for secp256k1](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
 - [x] [BIP-0173: Base32 address format for native v0-16 witness outputs](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
 - [x] [BIP-0350: Bech32m format for v1+ witness addresses](https://github.com/bitcoin/bips/blob/master/bip-0350.mediawiki)

## Build

The code can be compiled using
[`rebar3`](https://github.com/erlang/rebar3) Erlang project manager.

    $ rebar3 compile

## Test

The current implementation is tested using
[`eunit`](https://www.erlang.org/doc/apps/eunit/chapter.html) and
[`common_test`](https://www.erlang.org/doc/apps/common_test/basics_chapter.html)
both directly integrated with all default Erlang/OTP
release. [`cover`](https://www.erlang.org/doc/man/cover.html) is
enabled by default to have an idea of the coverage of the test. An
alias called `check` has been created to help using these tools.

    $ rebar3 check

## Documentation

A developer documentation is provided using
[`edoc`](https://www.erlang.org/doc/apps/edoc/chapter.html) and can be
generated on demand (automatically created if using rebar3 `check`
alias).

    $ rebar3 edoc

This project has been created to explain how to create an Erlang/OTP
from scratch using only a release and few dependencies. The design
choices for the infrastructure, data-structures and algorithms can be
found in in [notes](notes) directory. Those notes can be exported in
html, epub, pdf or plaintext using `make`.

    $ make

The documents will be generated in `_build/notes` directory.

## Development

For the developers, this project is using
[`asdf`](https://asdf-vm.com/). The tools used are listed in
`.tool-versions` file. To bootstrap your system, install `asdf` and
just execute this command in the repository:

    $ git clone https://github.com/erlang-punch/nostr
    $ cd nostr
    $ asdf install

## Usage

This project is a work in progress, and no usages are currently
displayed here.

# References and Resources

 - [Erlang Punch Nostr Project](https://github.com/erlang-punch/nostr)
 - [Erlang Punch Nostr Youtube Playlist](https://www.youtube.com/watch?v=4wMedr4k8zM&list=PL_1kmBlWRPQmC4hs5EK35-mxkSTITxpS6)
 - [Erlang Punch Nostr Twitch Channel](https://www.twitch.tv/erlangpunch)
 - [Official Nostr Website](https://nostr.com/)
 - [Main Nostr Open Relay](https://nostr.ch/)
 - [Official Nostr Statistics](https://nostr.watch/)
 - [Official Nostr Github Repository](https://github.com/nostr-protocol/)
 - [Official Nostr Github Repository (nips)](https://github.com/nostr-protocol/nips/)
