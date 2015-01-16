/*************************************************************************\
Copyright (c) 1990      The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/*************************************************************************\
                State set code generation
\*************************************************************************/
#ifndef INCLgensscodeh
#define INCLgensscodeh

#include "types.h"

uint default_context(Options *options);
void gen_ss_code(uint context, Node *prog, ChanList *channels, EvFlagList *event_flags);
void gen_funcdef(uint context, Node *fp);

#endif	/*INCLgensscodeh*/
