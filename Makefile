PROJECT = bidder

.PHONY: release clean-release

release: 
	relx -o rel/master_banker
	chmod a+x rel/master_banker/master_banker/bin/master_banker

clean-release: 
	rm -rf rel/master_banker

start:
	sh rel/master_banker/master_banker/bin/master_banker

include erlang.mk
