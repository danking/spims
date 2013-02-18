SPIMS_RACKET_BIN  ?= /proj/racket/bin
SPIMS_RACKET      ?= $(SPIMS_RACKET_BIN)/racket
SPIMS_RACO        ?= $(SPIMS_RACKET_BIN)/raco
SPIMS_ENTRY_POINT ?= main.rkt
SPIMS_EXECUTABLE  ?= spims
SPIMS_BUILD       ?= $(SPIMS_RACO) exe
SPIMS_COMPILE     ?= $(SPIMS_RACO) make


all:
	@$(SPIMS_BUILD) -o $(SPIMS_EXECUTABLE) $(SPIMS_ENTRY_POINT)

byte-compile:
	@$(SPIMS_COMPILE) $(SPIMS_ENTRY_POINT)

test:
	@cd tests && $(SPIMS_RACKET) run-all-tests.rkt; cd ..

package:
	$(eval $@_TMP := $(shell mktemp -d))
	$(eval $@_ZIP_DIR := $($@_TMP)/spims)
	$(eval $@_CWD := $(shell pwd))
	@mkdir $($@_ZIP_DIR)
	@cp Makefile $($@_ZIP_DIR)
	@cp README $($@_ZIP_DIR)
	@cp ./*.rkt $($@_ZIP_DIR)
	@cd $($@_TMP) && tar czf $($@_CWD)/spims.tar.gz spims && cd $($@_CWD)
	@rm -rf $($@_TMP)
