REBAR_CONFIG:=$(PWD)/rebar.config
INCLUDE_DIR:=include
SRC_DIR:=src

all: compile

compile: clean
	@rebar compile

clean:
	@rebar clean
	@find $(PWD)/. -name "erl_crash\.dump" | xargs rm -f

tests: compile
	@rebar ct

debug: clean
	@if test -f $(REBAR_CONFIG); then mv $(REBAR_CONFIG) $(REBAR_CONFIG).bak; fi;
	@echo {erl_opts, [{d, log_debug}]}. > $(REBAR_CONFIG)
	@rebar debug_info=true compile
	@rm $(REBAR_CONFIG)
	@if test -f $(REBAR_CONFIG).bak; then mv $(REBAR_CONFIG).bak $(REBAR_CONFIG); fi;

dialyze:
	@dialyzer -n -I $(INCLUDE_DIR) --src $(SRC_DIR)/*.erl
