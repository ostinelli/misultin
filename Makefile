EBIN_DIR := ebin
SRC_DIR := src
EXAMPLES_DIR := examples
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)

compile:
	@./rebar compile
clean:
	@./rebar clean
	@find . -name "erl_crash\.dump" | xargs rm -f

eunit:
	@rm -rf .eunit
	@./rebar eunit

test: eunit

dialyzer: compile
	@./rebar dialyze

example: compile
	$(ERLC) $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl