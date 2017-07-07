BusyTone
========

Reach functional tests and framework. This is work in progress, see [TODO](TODO.md).

Notes
=====

Relies heavily on [mod_erlang_event](https://freeswitch.org/confluence/display/FREESWITCH/mod_erlang_event),
also requires FreeSWITCH [drone](https://github.com/swarmcom/docker/tree/master/agents) to operate.

`call_sup:originate/1` and `call_sup:originate/2` anchor calls back to BusyTone to manage, the same way
as incoming calls gets in, therefore no additional tricks required to get channel events/updates.

`call:match_for/2` matches online calls for [Channel Variable](https://freeswitch.org/confluence/display/FREESWITCH/Channel+Variables) set.
`call:variables/1` collects [variables](https://freeswitch.org/confluence/display/FREESWITCH/Variables).

You can alter test suite verbosiness with `test_sup:debug()`, `test_sup:info()` and `test_sup:notice()` commands.

Test cases
==========

Test cases should be put to [src/tests](src/tests) folder, and named like `t_${Test Name}.erl` (suite runner will match module names with this pattern).
If test case file is altered it gets dynamically compiled and reloaded.

Example session
===============

```erlang
(busytone@172.17.0.1)149> test_sup:notice().
ok
(busytone@172.17.0.1)150> test_sup:run().
2017-07-07 11:04:10.747 <0.2466.0> notice test_sup.63 test:t_seq_login result:ok
2017-07-07 11:04:10.956 <0.2466.0> notice test_sup.63 test:t_simple_inqueue result:ok
2017-07-07 11:04:12.988 <0.2466.0> notice test_sup.63 test:t_seq_pickup result:ok
2017-07-07 11:04:13.916 <0.2466.0> notice test_sup.63 test:t_simple result:ok
ok
(busytone@172.17.0.1)151> test_sup:run(t_seq_login).
2017-07-07 11:05:09.099 <0.2466.0> notice test_sup.63 test:t_seq_login result:ok
ok
```
