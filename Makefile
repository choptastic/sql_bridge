.PHONY: test

all: compile

compile: rebar3
	$(REBAR) compile

run: rebar3
	$(REBAR) shell --config sample.config

test: rebar3 setup-all-tests
	$(REBAR) eunit

publish: rebar3
	$(REBAR) hex publish

dialyzer: rebar3
	$(REBAR) dialyzer

setup-all-tests:
	@(echo "This is going to ask for the sudo password because it will need to preload")
	@(echo "some configurations for MySQL and postgreSQL")
	@(cd test; sudo ./setup_dbs.sh)
	


# Check if rebar3.mk exists, and if not, download it
ifeq ("$(wildcard rebar3.mk)","")
$(shell curl -O https://raw.githubusercontent.com/choptastic/rebar3.mk/master/rebar3.mk)
endif

# rebar3.mk adds a new rebar3 rule to your Makefile
# (see https://github.com/choptastic/rebar3.mk) for full info
include rebar3.mk
