.PHONY: all compile clean eunit test eqc doc

DIRS=src 
EQC=${HOME}/lib/eqc-1.0.1

BRANCH=`git branch | awk '/\*/ {print $2}'`

all: compile

compile:
	./rebar compile


clean:
	./rebar clean

eunit:
	./rebar eunit

test/run_eqc_test.beam: test/run_eqc_test.erl
	erlc -W -o test test/run_eqc_test.erl

test: script test/run_eqc_test.beam
	./run_eqc.escript -m run_eqc_test -n 1000 -rpt error -pa test

doc:
	./rebar doc
	./mk_readme.escript doc/README.md README.md

script: compile
	escript ebin/run_eqc.beam generate run_eqc.escript ${EQC}
	chmod u+x run_eqc.escript