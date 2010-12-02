all:
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean

distclean:
	./rebar delete-deps
