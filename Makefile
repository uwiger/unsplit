.PHONY: all compile clean deps eunit test docs doc

all: compile

compile: deps
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps
	./rebar update-deps

eunit:
	./rebar eunit

test:
	./rebar ct

docs: doc
doc:
	./rebar doc
#	./mk_readme.escript doc/README.md README.md

