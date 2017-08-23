include /ioc/tools/driver.makefile

MODULE = seq

BUILDCLASSES += Linux
EXCLUDE_VERSIONS = 3.13 3.14.8

SNL=snl/R$(EPICSVERSION)

MAKE_FIRST = snl
.PHONY: snl
snl:
	make -f Makefile src EPICS_BASE=$(EPICS)/base-$(EPICSVERSION) INSTALL_LOCATION=$(PWD)/$(SNL) \
            PATH=$(PATH):$(PWD)/re2c/$(EPICS_HOST_ARCH) \
            CROSS_COMPILER_TARGET_ARCHS="$(filter-out ${EPICS_HOST_ARCH} $(addprefix %,${EXCLUDE_ARCHS}) $(addsuffix %,${EXCLUDE_ARCHS}),$(if ${ARCH_FILTER},$(filter ${ARCH_FILTER},${CROSS_COMPILER_TARGET_ARCHS}),${CROSS_COMPILER_TARGET_ARCHS}))"

HEADERS += $(SNL)/include/seq_snc.h
HEADERS += $(SNL)/include/seqCom.h
HEADERS += $(SNL)/include/seq_prim_types.h
HEADERS += $(SNL)/include/seq_mask.h
HEADERS += $(SNL)/include/pvAlarm.h
HEADERS += $(SNL)/include/seq_release.h

LIBOBJS_$(T_A) += $(SNL)/lib/$(T_A)/libseq.a
LIBOBJS_$(T_A) += $(SNL)/lib/$(T_A)/libpv.a 

BINS_SL6 = $(SNL)/bin/$(T_A)/snc

ifdef BASE_3_15
clean:
else
clean::
endif
	rm -rf snl `find src -name "O.*"`
