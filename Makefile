.PHONY: test

all: compile

compile: rebar3
	$(REBAR) compile

run: rebar3
	$(REBAR) shell --config sample.config

test: rebar3 setup-all-tests
	$(REBAR) eunit

dev: _checkouts _checkouts/epgsql _checkouts/mysql

_checkouts:
	mkdir -p _checkouts

_checkouts/epgsql:
	git clone https://github.com/epgsql/epgsql _checkouts/epgsql

_checkouts/mysql:
	git clone https://github.com/mysql-otp/mysql-otp _checkouts/mysql

publish: rebar3
	$(REBAR) hex publish

dialyzer: rebar3
	$(REBAR) dialyzer

setup-all-tests:
	@(echo "Configurationing databases for MySQL and postgreSQL tests")
	@(./test/db_config.sh)


# Check if rebar3.mk exists, and if not, download it
ifeq ("$(wildcard rebar3.mk)","")
$(shell curl -O https://raw.githubusercontent.com/choptastic/rebar3.mk/master/rebar3.mk)
endif

# rebar3.mk adds a new rebar3 rule to your Makefile
# (see https://github.com/choptastic/rebar3.mk) for full info
include rebar3.mk
