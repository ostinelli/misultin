EBIN_DIR := ebin
SRC_DIR := src
EXAMPLES_DIR := examples
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)

all: compile

clean:
	@rebar clean

compile:
	@rebar compile

debug: 
	@rebar debug_info=true compile

dialyzer: check-plt
	@rebar dialyze

check-plt: compile
	@rebar check-plt

build-plt: compile
	@rebar build-plt

example: compile
	$(ERLC) $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl
