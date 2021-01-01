all:
	make server
	make client
server:
	rebar3 compile
client:
	erlc -o _build/default/lib/prattle/ebin apps/client/src/prattle_store.erl apps/client/src/prattle_client.erl 
pack:
	tar -czf prattle.tar.gz _build/default/lib/prattle/ebin
shell:
	erl -pa _build/default/lib/prattle/ebin
