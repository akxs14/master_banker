PROJECT = bidder

.PHONY: release clean-release

release: 
	rebar compile
	relx -o rel

chmod:
	chmod a+x rel/master_banker/bin/master_banker

clean-release: 
	rm -rf rel/master_banker

start:
	sh rel/master_banker/bin/master_banker

full:
	rebar compile
	relx -o rel
	chmod a+x rel/master_banker/bin/master_banker
	sh rel/master_banker/bin/master_banker

include erlang.mk
