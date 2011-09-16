EBIN_DIR := ebin
SRC_DIR := src
EXAMPLES_DIR := examples
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)

all: compile

compile:
	@rebar compile
	
debug: 
	@rebar debug_info=true compile

clean:
	@rebar clean
	@find . -name "erl_crash\.dump" | xargs rm -f

dialyzer: check-plt
	@rebar dialyze

check-plt: compile
	@rebar check-plt

build-plt: compile
	@rebar build-plt

example: compile
	$(ERLC) $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl

tests: compile
	@mkdir -p $(PWD)/test/results
	@ct_run -suite $(PWD)/misultin_SUITE -pa $(PWD)/ebin -logdir $(PWD)/test/results
