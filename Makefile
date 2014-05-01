build:
	rebar compile

dev:
	erl -pa ebin -pa deps/epgsql/ebin

clean:
	rebar clean

deps:
	rebar get-deps

release:
	rebar compile
	cd rel
	rebar generate

launch:
	sh rel/master_banker/bin/master_banker console