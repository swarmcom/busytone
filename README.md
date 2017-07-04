BusyTone
========

Reach functional tests and framework. This is work in progress, see [TODO](TODO.md).

Notes
=====

Relies heavily on [mod_erlang_event](https://freeswitch.org/confluence/display/FREESWITCH/mod_erlang_event),
also requires FreeSWITCH [drone](https://github.com/swarmcom/docker/tree/master/agents) to operate.

`call_manager:originate/1` and `call_manager:originate/2` anchor calls back to BusyTone to manage, the same way
as incoming calls gets in, therefore no additional tricks required to get channel events/updates.

`call:match_for/2` matches online calls for [Channel Variable](https://freeswitch.org/confluence/display/FREESWITCH/Channel+Variables) set.
`call:variables/1` collects [variables](https://freeswitch.org/confluence/display/FREESWITCH/Variables).

Examples
========

```erlang
(busytone@172.17.0.1)163> {ok, UUID} = call_manager:originate("sofia/gateway/reach/10000").
{ok,"f569442b-f991-4e69-a3f6-4e2a167f701e"}

(busytone@172.17.0.1)164> call:alive(UUID).
ok

(busytone@172.17.0.1)165> call:match_for("Call-Direction", "outbound").
["f569442b-f991-4e69-a3f6-4e2a167f701e"]

(busytone@172.17.0.1)166> call:online().
["f569442b-f991-4e69-a3f6-4e2a167f701e"]

(busytone@172.17.0.1)167> call:vars(UUID).
... a lot of ...

(busytone@172.17.0.1)167> call:variables(UUID).
... a lot of ...

(busytone@172.17.0.1)167> call:answer(UUID).
ok

(busytone@172.17.0.1)167> call:hangup(UUID).
ok
```
