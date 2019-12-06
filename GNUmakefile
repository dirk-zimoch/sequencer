include /ioc/tools/driver.makefile

MODULE = seq

BUILDCLASSES += Linux
EXCLUDE_VERSIONS = 3.13 3.14.8

USE_EXACT_MINOR_VERSION=yes

SNL=snl/R$(EPICSVERSION)

MAKE_FIRST = snl
.PHONY: snl
snl:
	make -f Makefile -C src common lemon snc clean EPICS_BASE=$(EPICS)/base-$(EPICSVERSION) INSTALL_LOCATION=$(PWD)/$(SNL) \
	    SRCS= \
            PATH="$(PATH):$(PWD)/re2c/$(EPICS_HOST_ARCH)" \
            CROSS_COMPILER_TARGET_ARCHS="$(filter-out ${EPICS_HOST_ARCH} $(addprefix %,${EXCLUDE_ARCHS}) $(addsuffix %,${EXCLUDE_ARCHS}),$(if ${ARCH_FILTER},$(filter ${ARCH_FILTER},${CROSS_COMPILER_TARGET_ARCHS}),${CROSS_COMPILER_TARGET_ARCHS}))"

USR_INCLUDES += -I ../${SNL}/include

HEADERS += ${SNL}/include/seq_mask.h
HEADERS += ${SNL}/include/seq_prim_types.h
HEADERS += ${SNL}/include/seq_release.h

HEADERS += src/pv/pv.h
HEADERS += src/pv/pvAlarm.h
HEADERS += src/pv/pvType.h

SOURCES += src/pv/pv.c

HEADERS += src/seq/seqCom.h
HEADERS += src/seq/seqStats.h
HEADERS += src/seq/seq_snc.h
HEADERS += src/seq/seq_main.c

SOURCES += src/seq/seq_main.c
SOURCES += src/seq/seq_chan.c
SOURCES += src/seq/seq_task.c
SOURCES += src/seq/seq_ca.c
SOURCES += src/seq/seq_if.c
SOURCES += src/seq/seq_ef.c
SOURCES += src/seq/seq_mac.c
SOURCES += src/seq/seq_prog.c
SOURCES += src/seq/seq_qry.c
SOURCES += src/seq/seq_cmd.c
SOURCES += src/seq/seq_queue.c

ifneq ($(filter %x86 %x86_64 win%,${T_A}),)
BINS = $(SNL)/bin/$(T_A)/snc${EXE}
endif

ifdef BASE_3_15
clean:
else
clean::
endif
	rm -rf snl `find src -name "O.*"`
