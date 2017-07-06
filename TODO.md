BusyTone TODO
=============

## On

1. Notification system: notify agent on call, notify call on agent death, etc. grpoc?
2. Allow to execute tests in a sequence, but each in it's own process (not to spawn)
3. Ensure talking by issuing/detecting DTMF

## Other

2. Use sys.config param to place calls to Reach by numbers only
3. Add call stubs for agent? e.g. agent:hangup/1, agent:answer/1?

Done
====

2. On agent's death stop all agent incoming calls as well
2. Speedup event waiting by matching on-event
3. Optimize history lookup
4. Add wait for UUID function (for originated calls).
1. Better bind for calls to processes, either inbound or outbound, it should be transparent to users
1. Create a sample test: agent logs in, go available, receive a call, hangups after some time.
1. Keep ws messages and call events history, allow to wait for specific event/message
1. Close ws connection on agent terminate
3. Add a function to fetch agent's channels (by number)
1. Start agents from gen_server, not supervisor (we don't care about them to die)
4. Properly handle invalid login/password (agent should die loudly)
