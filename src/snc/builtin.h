/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#ifndef INCLbuiltinh
#define INCLbuiltinh

#include "sym_table.h"

/* Create Var structs for builtin functions and constants and
   insert them symbol table under the given scope */
void register_builtins(SymTable sym_table, struct syntax_node *scope);

/* Look up a builtin constant from the symbol table;
   the result is a valid E_CONST node or NULL */
struct syntax_node *lookup_builtin_const(SymTable sym_table, const char *name);

#endif /*INCLbuiltinh */
