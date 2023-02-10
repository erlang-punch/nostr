This documentation is currently a draft and will be updated later when
the whole pipeline will be correctly used.

# Create Github Actions Workflow

This documentation was created using QubeOS as main OS. The Github
Actions runner was executed in a dedicated virtual machine connected
to Github with Tor.

## Test Pipeline Design and Requirement

The goal is to test nostr on latest stable Erlang version. At this
time, the application should be able to be executed on Erlang R25 and
Erlang R23. Here the rules:

 - `eunit` MUST be executed with `rebar3`
 - `common_test` MUST be executed with `rebar3`
 - `dialyzer` MAY be executed with `rebar3`
 - profilers MAY be executed with `rebar3`

## Github Actions Repository Bootstrapping 

## Github Actions and QubeOS Disposable VM

QubeOS can be used to create short-living virtual machines called
Disposable VM. These VMs were designed to be executed one-time and all
the content...

## Github Actions and asdf

TODO

## Github Actions and Containers (docker)

Unfortunately, containers are the fatest way to have something working
in our modern world. Lot of modifications have been needed to maintain
other version...

