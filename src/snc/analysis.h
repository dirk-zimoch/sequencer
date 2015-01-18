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
