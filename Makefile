all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

run:
	erl -pa ebin/ -pa deps/*/ebin/ -eval "application:start(crypto)" -eval "application:start(emysql)"
