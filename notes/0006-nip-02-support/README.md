---
date: 2023-03-10
title: NIP/02 Support
subtitle: |
  Add Contact List and Petnames support in nostr
author: Mathieu Kerjouan
keywords: erlang,otp,nostr,nip/02,contact,petnames
license: CC BY-NC-ND
abstract: |
  
toc: true
hyperrefoptions:
  - linktoc=all
---

---

This article has been redacted inMarch 2023. It describes the
methodologies applied and describes the methods used to implement the
NIP/02 from nostr protocol in Erlang with a minimal amount of
dependencies. The following code has been tested using [Erlang/OTP
R25](https://www.erlang.org/news/157) running on
[OPENBSD-72-CURRENT](openbsd.org/) and [Parrot
Linux](https://parrotsec.org/) (a Debian like distribution).

# NIP/02 Support: Contact List and Petname

The identity is the most important piece of the whole structure. An
user using one identity can be on many different relay. A contact list
is dedicated to one user, containing the list of interesting authors
based on their public key. The current implementation will create the
functions to deal with the event format and the client part.


```erlang


```

# Conclusion

