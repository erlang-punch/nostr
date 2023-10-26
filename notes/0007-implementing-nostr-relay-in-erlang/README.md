---
date: 2023-03-10
title: Implementing a Basic Nostr Relay in Erlang
subtitle: |
  A Naive and Primitive Implementation of a Nostr Relay with Erlang/OTP
author: Mathieu Kerjouan
keywords: erlang,otp,nostr,relay
license: CC BY-NC-ND
abstract: |
  A working client has been previously implemented in Erlang, only
  supporting NIP/01. A primitive relay is required to test it
  correctly.

toc: true
hyperrefoptions:
  - linktoc=all
---

# Implementing a Basic Nostr relay in Erlang

Supporting a client and a server in the same project can help a
lot. Usually, when a project is created, only a client or a server is
created, when tests are needed, developers need to mock all behaviors
by simulating a connection and so on. Those tests are still relevant,
and can help to describe how the software is reacting and what kind of
data both actors should return. Using a full implementation of a
protocol can be used in integration testing by tested the all elements
of the chain.

## A Quick Introduction to Cowboy

Cowboy is probably the most used web server on Erlang and Elixir, in
particular if you are using Phoenix framework, you will use it by
default. This web server is quite powerful and supports
HTTP/1.0[^rfc-http-1.0], HTTP/1.1[^rfc-http-1.1],
websocket[^rfc-websocket] and HTTP/2[^rfc-http-2]. QUIC[^rfc-quic] and
HTTP/3[^rfc-http-3] protocols implementation were announced in Cowboy
2.8[^cowboy-28-announce] but not implemented yet.

 - classical tcp connection with gen_tcp
 - cowboy and ranch (dependency)
 - routes and dispatch
 - listening
 - handling request
 - security with ssl/tls

## Nostr Relay Implementation

### Listening

### Routing

### Handling Requests

### Indexing

### Queuing

### Testing

 - Connect to the server with a client
 - Data Exchange
 - Test integration in common_test

## Erlang Nostr Features

Erlang/OTP offers a great environment to create complex features
without a lot of effort, and Cowboy is doing the same.

 - Message validation
 - Multi Relay support
 - Memory Store
 - Disk Store
 - Mnesia Store

# Conclusion

 - Test only for the moment

# References

 - https://ninenines.eu/docs/
 - https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy.start_clear/
 - https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_router/
 - https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_websocket/
 - https://ninenines.eu/docs/en/cowboy/2.9/manual/cowboy_handler/

---

[^rfc-http-1.0]: https://datatracker.ietf.org/doc/html/rfc1945
[^rfc-http-1.1]: https://datatracker.ietf.org/doc/html/rfc9112
[^rfc-http-2]: https://datatracker.ietf.org/doc/html/rfc9113
[^rfc-http-3]: https://datatracker.ietf.org/doc/html/rfc9114
[^rfc-quic]: https://datatracker.ietf.org/doc/html/rfc9000
[^rfc-websocket]: https://datatracker.ietf.org/doc/html/rfc6455
[^cowboy-28-announce]: https://ninenines.eu/articles/cowboy-2.8.0/
