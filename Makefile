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

ct_run:
	ct_run -noshell -pa "$(addprefix $(CURDIR), /ebin)" "$(addprefix $(CURDIR), /.)"  -include "$(addprefix $(CURDIR), /include)"  -name test@127.0.0.1 -logdir "$(addprefix $(CURDIR), /logs)" -env TEST_DIR "$(addprefix $(CURDIR), /test)" -ct_config test/test.config -dir test  2>&1 | tee -a $(addprefix $(CURDIR), /logs/raw.log)

test: ct_run
# ./rebar ct

docs: doc
doc:
	./rebar doc
#	./mk_readme.escript doc/README.md README.md

