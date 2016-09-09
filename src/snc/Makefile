TOP = ../..
include $(TOP)/configure/CONFIG
#----------------------------------------
#  ADD MACRO DEFINITIONS AFTER THIS LINE

# default main (+m option)
INC += seqMain.c

#  snc product
PROD_HOST += snc

#CFLAGS += -Wconversion -Wextra

snc_SRCS += main.c          # main program
snc_SRCS += lexer.c         # generated from lexer.re
snc_SRCS += snl.c           # parser (generated from snl.lem)
snc_SRCS += node.c          # syntax node operations
snc_SRCS += var_types.c     # declarations
snc_SRCS += analysis.c      # analysis routines
snc_SRCS += gen_code.c      # code generation
snc_SRCS += gen_ss_code.c   # code generation (state sets)
snc_SRCS += gen_tables.c    # code generation (tables)
snc_SRCS += sym_table.c     # symbol table
snc_SRCS += builtin.c       # builtin constants and functions
snc_SRCS += type_check.c    # rudimentary type checker

snc_LIBS += Com

HTMLS += Syntax.txt

# avoid deprecation warnings with base 3.15 and later
ifeq ($(BASE_3_14),YES)
LEMON = $(call PATH_FILTER,$(INSTALL_HOST_BIN)/lemon$(HOSTEXE))
else
LEMON = $(INSTALL_HOST_BIN)/lemon$(HOSTEXE)
endif

include $(TOP)/configure/RULES
#----------------------------------------
#  ADD RULES AFTER THIS LINE

vpath %.lem ..
vpath %.lt ..
vpath %.txt $(COMMON_DIR)

%.c %.h: %.lem %.lt $(LEMON)
	$(LEMON) o=. $<

vpath %.re ..

%.c: %.re
	$(RE2C) -s -b -o $@ $<

$(COMMON_DIR)/snl.bnf: snl.lem
	$(LEMON) o=. -g $< > $@.tmp
	$(MV) $@.tmp $@

$(COMMON_DIR)/Syntax.txt: ../bnf2txt.pl lexer.re $(COMMON_DIR)/snl.bnf
	$(PERL) $^ > $@.tmp
	$(MV) $@.tmp $@

ifeq ($(BASE_3_14),YES)
%$(DEP): %.c
	@-$(MKMF) $(HDEPENDS_FLAGS) -m $*$(DEP) $(HDEPENDS_INCLUDES) $*$(OBJ) $<
endif

$(HDEPENDS_FILES): | snl.h

CLEANS += lexer.c snl.c snl.h

# there is no rule like this in base 3.14
ifeq ($(BASE_3_14),YES)
clean::
	@$(RM) $(CLEANS)
endif
