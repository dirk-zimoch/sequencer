include /ioc/tools/driver.makefile

MODULE = seq

BUILDCLASSES += Linux
EXCLUDE_VERSIONS = 3.13 3.14.8

BUILD=build/R$(EPICSVERSION)

MAKE_FIRST = snl-lib
snl-lib:
	make -f Makefile src EPICS_BASE=$(EPICS_BASE) INSTALL_LOCATION=$(PWD)/$(BUILD)


HEADERS += $(BUILD)/include/seq_snc.h
HEADERS += $(BUILD)/include/seqCom.h
HEADERS += $(BUILD)/include/seq_prim_types.h
HEADERS += $(BUILD)/include/seq_mask.h
HEADERS += $(BUILD)/include/pvAlarm.h
HEADERS += $(BUILD)/include/seq_release.h

LIBOBJS_$(T_A) += $(BUILD)/lib/$(T_A)/libseq.a
LIBOBJS_$(T_A) += $(BUILD)/lib/$(T_A)/libpv.a 

BINS_SL6 = $(BUILD)/bin/$(T_A)/snc

clean::
	rm -rf build `find src -name "O.*"`
