BusyTone TODO
=============

## Agents

1. Start agents from gen_server, not supervisor (we don't care about them to die)
2. On agent's death stop all agent incoming calls as well
3. Add a function to fetch agent's channels (by number)
4. Properly handle invalid login/password (agent should die loudly)

## Tests

1. Create a sample test: agent logs in, go available, receive a call, hangups after some time.
