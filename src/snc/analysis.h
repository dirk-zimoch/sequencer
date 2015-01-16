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
                Analysis of parse tree
\*************************************************************************/
#ifndef INCLanalysish
#define INCLanalysish

#include "types.h"

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

/*
 * Type of channel iteratees.
 */
typedef uint channel_action(Chan *node, void *env);

/*
 * Type of evflag iteratees.
 */
typedef uint evflag_action(EvFlag *ef, void *env);

/*
 * Traverse a channel tree, calling either chan_iter or evflag_iter depending
 * on the node type. The iteratees should return a boolean failure or success
 * status. A failure (non-zero) means that sibling nodes are ignored and the
 * status is propagated upstream and finally returned.
 */
uint traverse_channel_tree(
	ChanNode	*node,		/* start node */
	channel_action	*chan_iter,	/* function to call for each channel */
	evflag_action	*evflag_iter,	/* function to call for each event flag */
	void		*env		/* environment, passed to iteratees */
);

void add_var_to_scope(Var *vp, Node *scope);

Program *analyse_program(Node *ep, Options *options);

#endif	/*INCLanalysish*/
