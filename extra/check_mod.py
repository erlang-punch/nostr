#!/usr/bin/env python3
# check_mod.py
"""
This script is used to check custom modulo operator created
in Erlang for the nostr project.
"""

import sys
import string
import secrets

limit = 256
if len(sys.argv) > 1:
    limit = int(sys.argv[1])

generator = secrets.SystemRandom()
start = -(2**256)
end = 2**256
for i in range(limit):
    a = generator.randrange(start, end)
    m = generator.randrange(0,end)
    r = a % m
    l = ",".join([str(i),str(a),str(m),str(r)])
    print(l)
