EXAMPLES_DIR:=examples
REBAR_CONFIG:=$(PWD)/rebar.config

all: compile

compile:
	@rebar compile

clean:
	@rebar clean
	@find $(PWD)/. -name "erl_crash\.dump" | xargs rm -f

tests: compile
	@rebar ct

debug: clean
	@echo {erl_opts, [{d, log_debug}]}. > $(REBAR_CONFIG)
	@rebar debug_info=true compile
	@rm $(REBAR_CONFIG)