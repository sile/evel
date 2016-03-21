.PHONY: test

all: compile xref test dialyze edoc

compile:
	@./rebar3 compile

xref:
	@./rebar3 xref

clean:
	@./rebar3 clean

test:
	@./rebar3 eunit
	@./rebar3 ct
	@./rebar3 cover

edoc:
	@./rebar3 as edown edoc

start: compile
	@./rebar3 shell

dialyze: compile
	@./rebar3 dialyzer
