BusyTone TODO
=============

## On

1. Create a sample test: agent logs in, go available, receive a call, hangups after some time.

## Agents

2. On agent's death stop all agent incoming calls as well

## Tests

Done
====

1. Close ws connection on agent terminate
3. Add a function to fetch agent's channels (by number)
1. Start agents from gen_server, not supervisor (we don't care about them to die)
4. Properly handle invalid login/password (agent should die loudly)
