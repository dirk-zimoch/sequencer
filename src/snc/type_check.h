/*************************************************************************\
Copyright (c) 2013-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#ifndef INCLtype_checkh
#define INCLtype_checkh

#include "types.h"
#include "var_types.h"

/* return an approximation of the type of an expression */
Type *type_of(Node *e);

/* check if the value types of two pv types are equal */
int pv_type_check(Type *expected, Type *inferred);

#endif /*INCLtype_checkh */
