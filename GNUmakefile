include /ioc/tools/driver.makefile

# requires re2c to be installed

MODULE = seq
USE_EXACT_MINOR_VERSION=yes

BUILDCLASSES += vxWorks Linux WIN32
EXCLUDE_VERSIONS = 3.13 3.14.8

# Build snc for host architectures
# This also creates the version header file seq_release.h.
SNC_ARCHS = SL% RHEL% win%
MAKE_FIRST = snl
.PHONY: snl
snl:
	PATH=$(PATH):$(PWD)/re2c/$(EPICS_HOST_ARCH) make -f Makefile -C src DIRS="common lemon snc" \
        SRCS= EPICS=$(EPICS) EPICSVERSION=$(EPICSVERSION) CHECK_RELEASE=NO \
        INSTALL_LOCATION=$(PWD)/snl/R$(EPICSVERSION) \
        CROSS_COMPILER_TARGET_ARCHS="$(filter-out $(EPICS_HOST_ARCH),$(filter $(SNC_ARCHS), $(CROSS_COMPILER_TARGET_ARCHS)))"

USR_INCLUDES += -I ../snl/R$(EPICSVERSION)/include

HEADERS += src/common/O.Common/seq_release.h
HEADERS += src/common/seq_prim_types.h
HEADERS += src/common/seq_mask.h
HEADERS += src/pv/pv.h
HEADERS += src/pv/pvAlarm.h
HEADERS += src/pv/pvType.h

SOURCES += src/pv/pv.c

HEADERS += src/seq/seq_snc.h
HEADERS += src/seq/seqCom.h
HEADERS += src/seq/seq_prim_types.h
HEADERS += src/seq/seq_mask.h
HEADERS += src/seq/pvAlarm.h

SOURCES += src/seq/seq_main.c
SOURCES += src/seq/seq_task.c
SOURCES += src/seq/seq_ca.c
SOURCES += src/seq/seq_if.c
SOURCES += src/seq/seq_mac.c
SOURCES += src/seq/seq_prog.c
SOURCES += src/seq/seq_qry.c
SOURCES += src/seq/seq_cmd.c
SOURCES += src/seq/seq_queue.c

ifneq ($(filter $(SNC_ARCHS),${T_A}),)
BINS = snl/R$(EPICSVERSION)/bin/$(T_A)/snc${EXE}
endif

ifdef BASE_3_15
clean:
else
clean::
endif
	rm -rf snl `find src -name "O.*"`
