BusyTone TODO
=============

## On

1. Automate channel counter on agent answer?

## Other

Done
====

1. Add recipe tests
2. Add and distinguish Queues
2. Use sys.config param to place calls to Reach by numbers only
1. fs_sync should be able to compile newly added modules
1. In order to conduct tests there should be a way to alter (add/remove) agent/queues/skills properties dynamically
1. Create a transfer to agent test
3. Provide a library for test functions
1. Allow to execute tests in a sequence, but each in it's own process (not to spawn)
3. Ensure talking by issuing/detecting DTMF
1. Notification system: notify agent on call, notify call on agent death, etc. grpoc?
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
