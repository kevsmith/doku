all: escript

escript: compile
	./rebar escriptize

compile: deps/markdown deps/getopts
	./rebar compile

deps/markdown:
	./rebar get-deps

deps/getopts:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar del-deps