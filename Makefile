
LIBDIR := lib/github.com/pzel/mltorrent
MLCOMP ?= polymlb
MLB_PATH := -mlb-path-var "SMLPKG $(shell pwd)/lib"

ifeq ($(MLCOMP), polymlb)
MLCOMP_FLAGS=-ann "ignoreFiles call-main.sml"
endif

.PHONY: all
all:	 test

.PHONY: clean
clean:
	-@rm -f bin/*

.PHONY: test
test: bin/test
	./$<

bin/test: $(shell find $(LIBDIR))
	@$(MLCOMP) $(MLCOMP_FLAGS) $(MLB_PATH) -output $@ $(LIBDIR)/test/test.mlb

