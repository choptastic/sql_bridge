.PHONY: test

all: compile

compile: rebar3
	$(REBAR) compile

run: rebar3
	$(REBAR) shell --config sample.config

test: rebar3
	$(REBAR) eunit

publish: rebar3
	$(REBAR) hex publish

dialyzer: rebar3
	$(REBAR) dialyzer

# Check if rebar3.mk exists, and if not, download it
ifeq ("$(wildcard rebar3.mk)","")
$(shell curl -O https://raw.githubusercontent.com/choptastic/rebar3.mk/master/rebar3.mk)
endif

# rebar3.mk adds a new rebar3 rule to your Makefile
# (see https://github.com/choptastic/rebar3.mk) for full info
include rebar3.mk
