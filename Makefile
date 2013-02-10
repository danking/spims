ifndef SPIMS_RACKET_BIN
	SPIMS_RACKET_BIN=/proj/racket/bin
endif

ifndef SPIMS_RACKET
	SPIMS_RACKET=${SPIMS_RACKET_BIN}/racket
endif

ifndef SPIMS_RACO
	SPIMS_RACO=${SPIMS_RACKET_BIN}/raco
endif

ifndef SPIMS_ENTRY_POINT
	SPIMS_ENTRY_POINT=main.rkt
endif

ifndef SPIMS_EXECUTABLE
	SPIMS_EXECUTABLE=spims
endif

ifndef SPIMS_BUILD
	SPIMS_BUILD=${SPIMS_RACO} exe
endif

ifndef SPIMS_COMPILE
	SPIMS_COMPILE=${SPIMS_RACO} make
endif


all:
	@${SPIMS_BUILD} -o ${SPIMS_EXECUTABLE} ${SPIMS_ENTRY_POINT}

byte-compile:
	@${SPIMS_COMPILE} ${SPIMS_ENTRY_POINT}

test:
	@${SPIMS_RACKET} tests/run-all-tests.rkt
