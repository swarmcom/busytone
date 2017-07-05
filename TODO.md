BusyTone TODO
=============

## On

1. Better bind for calls to processes, either inbound or outbound, it should be transparent to users
2. Speedup event waiting by matching on-event
3. Optimize history lookup
4. Add wait for UUID function (for originated calls).

## Agents

2. On agent's death stop all agent incoming calls as well

## Tests

## Other

2. Use sys.config param to place calls to Reach by numbers only
3. Add call stubs for agent? e.g. agent:hangup/1, agent:answer/1?

Done
====

1. Create a sample test: agent logs in, go available, receive a call, hangups after some time.
1. Keep ws messages and call events history, allow to wait for specific event/message
1. Close ws connection on agent terminate
3. Add a function to fetch agent's channels (by number)
1. Start agents from gen_server, not supervisor (we don't care about them to die)
4. Properly handle invalid login/password (agent should die loudly)
