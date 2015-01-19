/*************************************************************************\
Copyright (c) 1989-1993 The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/*************************************************************************\
			Creating expressions
\*************************************************************************/
#ifndef INCLparseh
#define INCLparseh

#include "types.h"

/* defined in node.c */
Node *node(
	enum node_tag	tag,
	Token		tok,
	...
);

Node *opt_defn(
	Token	name,
	Token	value
);

Node *link_node(
	Node	*ep1,
	Node	*ep2
);

uint strtoui(
	char *str,		/* string representing a number */
	uint limit,		/* result should be < limit */
	uint *pnumber		/* location for result if successful */
);

Node *defn_list_from_scope(Node *scope);
VarList **pvar_list_from_scope(Node *scope);

#define var_list_from_scope(scope) (*pvar_list_from_scope(scope))

/* Iteratee ("what gets iterated") for traverse_syntax_tree */
typedef int node_iter(Node *ep, Node *scope, void *parg);

/* Depth-first traversal of the syntax tree. Call the supplied iteratee whenever
 * call_mask has the (ep->type)'th bit set. The function is called with the current
 * ep, the current scope, and an additional user defined argument (argp). Afterwards,
 * if the iteratee returned a non-zero value, recurse into all child nodes except
 * those whose type'th bit is set in stop_mask. The traversal starts at the first
 * argument. The 4th argument is the current scope; 0 may be supplied for it, in
 * which case it will be set to a valid scope as soon as the traversal encounters
 * one.
 *
 * NOTE: The next pointer of the start node is ignored, this functions does NOT
 * descend into sibling list elements. It does, however, do this for the child nodes
 * it encounters.
 */
void traverse_syntax_tree(
	Node		*ep,		/* start node */
	NodeMask	call_mask,	/* when to call iteratee */
	NodeMask	stop_mask,	/* when to stop descending */
	Node		*scope,		/* current scope, 0 at top-level */
	node_iter	*iteratee,	/* function to call */
	void		*parg		/* argument to pass to function */
);

Node *mk_subscr_node(Node *operand, uint n);
Node *mk_select_node(Node *operand, char *name);
Node *mk_var_node(Var *vp);
Node *mk_string_node(Node *other, char *s);

void dump_node(Node *e, int level);

#endif	/*INCLparseh*/
