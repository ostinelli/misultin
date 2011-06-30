EBIN_DIR := ebin
SRC_DIR := src
EXAMPLES_DIR := examples
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR)

all:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@cp $(SRC_DIR)/misultin.app.src $(EBIN_DIR)/misultin.app
	
clean:
	@rm -rf $(EBIN_DIR)/*
	@rm -f erl_crash.dump
	
debug:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@cp $(SRC_DIR)/misultin.app.src $(EBIN_DIR)/misultin.app
	
dialyzer:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) +debug_info $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@cp $(SRC_DIR)/misultin.app.src $(EBIN_DIR)/misultin.app

example:
	@mkdir -p $(EBIN_DIR)
	$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@cp $(SRC_DIR)/misultin.app.src $(EBIN_DIR)/misultin.app
	$(ERLC) $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl

tests:
	@mkdir -p $(PWD)/test/results
	@ct_run -suite $(PWD)/misultin_SUITE -pa $(PWD)/ebin -logdir $(PWD)/test/results
