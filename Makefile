devel:
	rebar3 release

release:
	rebar3 as prod release

clean:
	rebar3 clean

clean-all:
	rebar3 clean -a
	rm -rf ~/.cache/rebar3
	rm -f rebar.lock
	rm -rf _build/
	rebar3 update
	rebar3 local upgrade

dialyzer: 
	rebar3 dialyzer

xref: 
	rebar3 xref

test:
	rebar3 eunit

console: devel
	_build/default/rel/busytone/bin/busytone console
