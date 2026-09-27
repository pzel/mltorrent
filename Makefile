
LIBDIR := lib/github.com/pzel/mltorrent
MLCOMP ?= polymlb
SMLPKG_PATH := -mlb-path-var "SMLPKG $(shell pwd)/lib"

ifeq ($(MLCOMP), polymlb)
MLCOMP_FLAGS=-deps-first -ann "ignoreFiles call-main.sml" -verbose 4
POLY_PATH := -mlb-path-var "POLY \$$(SML_LIB)/basis/"
else
MLCOMP_FLAGS=-default-ann 'allowRecordPunExps true'
POLY_PATH := -mlb-path-var "POLY \$$(SMLPKG)/github.com/pzel/polyml-fill/"
endif

.PHONY: all
all:	 test

.PHONY: clean
clean:
	-@rm -f bin/*

.PHONY: test
test: bin/test
	./$<

COMP := $(MLCOMP) $(MLCOMP_FLAGS) $(SMLPKG_PATH) $(POLY_PATH) -output

bin/test: $(shell find $(LIBDIR))
	$(COMP) $@ $(LIBDIR)/test/test.mlb

bin/mltorrent: $(shell find $(LIBDIR))
	$(COMP) $@ $(LIBDIR)/mltorrent.mlb

