/*************************************************************************\
Copyright (c) 1990      The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2012 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/*************************************************************************\
                Code generation
\*************************************************************************/
#ifndef INCLgencodeh
#define INCLgencodeh

#include "types.h"

void generate_code(Program *p);
void gen_defn_c_code(Expr *scope, int level);
void gen_var_decl(Var *vp);
void indent(int level);

#define NM_VARS		"_seq_var"
#define NM_CHANS	"_seq_chans"
#define NM_STATES	"_seq_states"
#define NM_STATESETS	"_seq_statesets"

#define NM_ENTRY	"_seq_entry"
#define NM_EXIT		"_seq_exit"
#define NM_INIT		"_seq_init"
#define NM_ACTION	"_seq_action"
#define NM_EVENT	"_seq_event"
#define NM_MASK		"_seq_mask"

#define NM_SS		"_seq_ss"
#define NM_TRN		"_seq_trn"
#define NM_PTRN		"_seq_ptrn"
#define NM_PNST		"_seq_pnst"

#endif	/*INCLgencodeh*/
