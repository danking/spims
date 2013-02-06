ifndef SPIMS_RACO
	SPIMS_RACO=/proj/racket/bin/raco
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
