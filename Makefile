LAZBUILD := $(shell which lazbuild)
LAZBUILD_OPTS := -B --bm=Release
PACKAGE := packages/Lazarus/fp_tera_tpl.lpk
EXAMPLES := variables filters full
TESTS:= testteratpl_cli

MAKEFLAGS += --no-print-directory

default: all

all: test examples

test:
	@echo "==== Building tests ===="
	@$(LAZBUILD) $(PACKAGE)
	@$(LAZBUILD) $(LAZBUILD_OPTS) ./tests/$(TESTS).lpi
	@echo
	@echo "==== Testing ===="
	@./bin/$(TESTS)

examples:
	@for example in $(EXAMPLES); do \
	  $(MAKE) $$example; \
	done

variables:
	@echo "==== Building $(@) ====";
	$(LAZBUILD) $(LAZBUILD_OPTS) examples/$(@)/$(@).lpi
	@echo "==== Output: $(@) ===="
	@./bin/$(@)

filters:
	@echo "==== Building $(@) ====";
	$(LAZBUILD) $(LAZBUILD_OPTS) examples/$(@)/$(@).lpi
	@echo "==== Output: $(@) ===="
	@./bin/$(@)

full:
	@echo "==== Building $(@) ====";
	$(LAZBUILD) $(LAZBUILD_OPTS) examples/$(@)/$(@).lpi
	@echo "==== Output: $(@) ===="
	@./bin/$(@)

.PHONY: all test examples variables filters full