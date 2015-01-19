/*************************************************************************\
Copyright (c) 1990      The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <errno.h>
#include <limits.h>

#define node_info_GLOBAL
#include "types.h"
#undef node_info_GLOBAL
#include "node.h"
#include "main.h"
#include "var_types.h"
#include "snl.h"

static const int impossible = 0;

static const StateOptions default_state_options = DEFAULT_STATE_OPTIONS;

Node *node(
	enum node_tag	tag,
	Token		tok,
	...			/* variable number of child arguments */
)
{
	va_list	argp;
	uint	i, num_children;
	Node	*ep;

	num_children = node_info[tag].num_children;

	ep = new(Node);
	ep->next = 0;
	ep->last = ep;
	ep->tag = tag;
        ep->token = tok;
	ep->children = newArray(Node *, num_children);
	/* allocate extra data */
	switch (tag)
	{
	case D_SS:
		ep->extra.e_ss = new(StateSet);
		break;
	case D_STATE:
		ep->extra.e_state = new(State);
		ep->extra.e_state->options = default_state_options;
		break;
	case D_WHEN:
		ep->extra.e_when = new(When);
		break;
	default:
		break;
	}
	if (is_scope(ep))
	{
		var_list_from_scope(ep) = new(VarList);
	}

#ifdef	DEBUG
	report_at_node(ep, "node: ep=%p, tag=%s, value=\"%s\", file=%s, line=%d",
		ep, node_name(ep), tok.str, tok.file, tok.line);
#endif	/*DEBUG*/
	va_start(argp, tok);
	for (i = 0; i < node_info[ep->tag].num_children; i++)
	{
		ep->children[i] = va_arg(argp, Node*);
#ifdef	DEBUG
		report(", child[%d]=%p", i, ep->children[i]);
#endif	/*DEBUG*/
	}
	va_end(argp);
#ifdef	DEBUG
	report(")\n");
#endif	/*DEBUG*/

	return ep;
}

Node *opt_defn(Token name, Token value)
{
	Node *opt = node(D_OPTION, name);
	opt->extra.e_option = (value.str[0] == '+');
	return opt;
}

/* Link two expression structures and/or lists.  Returns ptr to combined list.
   Note: last ptrs are correct only for 1-st element of the resulting list */
Node *link_node(
	Node	*ep1,	/* 1-st structure or list */
	Node	*ep2	/* 2-nd (append it to 1-st) */
)
{
	if (ep1 == 0)
		return ep2;
	if (ep2 == 0)
		return ep1;
	ep1->last->next = ep2;
	ep1->last = ep2->last;
	ep2->last = 0;
	return ep1;
}

uint strtoui(
	char *str,		/* string representing a number */
	uint limit,		/* result should be < limit */
	uint *pnumber		/* location for result if successful */
)
{
	unsigned long result;

	errno = 0;
	result = strtoul(str, 0, 0);
	if (errno || result >= limit)
		return FALSE;
	*pnumber = result;
	return TRUE;
}

VarList **pvar_list_from_scope(Node *scope)
{
	assert(scope);				/* precondition */
	assert(is_scope(scope));		/* precondition */

	switch(scope->tag)
	{
	case D_PROG:
		return &scope->extra.e_prog;
	case D_SS:
		assert(scope->extra.e_ss);	/* invariant */
		return &scope->extra.e_ss->var_list;
	case D_STATE:
		assert(scope->extra.e_state);	/* invariant */
		return &scope->extra.e_state->var_list;
	case S_CMPND:
		return &scope->extra.e_cmpnd;
	case D_FUNCDEF:
		return &scope->extra.e_funcdef;
	default:
		assert(impossible); return NULL;
	}
}

Node *defn_list_from_scope(Node *scope)
{
	assert(scope);				/* precondition */
	assert(is_scope(scope));		/* precondition */

	switch(scope->tag)
	{
	case D_PROG:
		return scope->prog_defns;
	case D_SS:
		return scope->ss_defns;
	case D_STATE:
		return scope->state_defns;
	case S_CMPND:
		return scope->cmpnd_defns;
	case D_FUNCDEF:
		assert(scope->funcdef_decl);			/* invariant */
		assert(scope->funcdef_decl->extra.e_decl);	/* invariant */
		return scope->funcdef_decl->extra.e_decl->type->val.function.param_decls;
	default:
		assert(impossible); return NULL;
	}
}

void traverse_syntax_tree(
	Node		*ep,		/* start node */
	NodeMask	call_mask,	/* when to call iteratee */
	NodeMask	stop_mask,	/* when to stop descending */
	Node		*scope,		/* current scope, 0 at top-level */
	node_iter	*iteratee,	/* function to call */
	void		*parg		/* argument to pass to function */
)
{
	Node	*cep;
	uint	i;
	int	descend = TRUE;

	if (!ep)
		return;

#ifdef DEBUG
	report("traverse_syntax_tree(tag=%s,token.str=%s)\n",
		node_name(ep), ep->token.str);
#endif

	/* Call the function? */
	if (call_mask & bit(ep->tag))
	{
		descend = iteratee(ep, scope, parg);
	}

	if (!descend)
		return;

	/* Are we just entering a new scope? */
	if (is_scope(ep))
	{
#ifdef DEBUG
	report("traverse_syntax_tree: new scope=(%s,%s)\n",
		node_name(ep), ep->token.str);
#endif
		scope = ep;
	}

	/* Descend into children */
	for (i = 0; i < node_info[ep->tag].num_children; i++)
	{
		foreach (cep, ep->children[i])
		{
			if (!(bit(cep->tag) & stop_mask))
			{
				traverse_syntax_tree(cep, call_mask, stop_mask,
					scope, iteratee, parg);
			}
		}
	}
}

static Node *mk_const_node(Node *other, uint n)
{
	static char buf[21];	/* long enough for a 64 bit unsigned in decimal */
	Token k = other->token;

	k.symbol = TOK_INTCON;
	sprintf(buf, "%u", n);
	k.str = strdup(buf);
	return node(E_CONST, k);
}

Node *mk_subscr_node(Node *operand, uint n)
{
	Token k = operand->token;

	k.symbol = TOK_LBRACKET;
	k.str = "[";
	return node(E_SUBSCR, k, operand, mk_const_node(operand, n));
}

static Node *mk_member_node(Node *other, char *name)
{
	Token k = other->token;

	k.symbol = TOK_NAME;
	k.str = name;
	return node(E_MEMBER, k);
}

Node *mk_select_node(Node *operand, char *name)
{
	Token k = operand->token;

	k.symbol = TOK_PERIOD;
	k.str = ".";
	return node(E_SELECT, k, operand, mk_member_node(operand, name));
}

Node *mk_var_node(Var *vp)
{
	Token k;
	Node *r;

	assert(vp->decl);
	k = vp->decl->token;
	k.symbol = TOK_NAME;
	k.str = vp->name;
	r = node(E_VAR, k);
	r->extra.e_var = vp;
	return r;
}

Node *mk_string_node(Node *other, char *s)
{
	Token k = other->token;
	k.symbol = TOK_STRCON;
	k.str = s;
	return node(E_STRING, k);
}

void dump_node(Node *e, int level)
{
    int i, l;
    for (l = 0; l < level; l++) report("  ");
    if (e) {
        Node *ce;
        report("%s '%s'\n", node_name(e), e->token.str);
        for (i = 0; i < node_info[e->tag].num_children; i++) {
            foreach (ce, e->children[i]) {
                dump_node(ce, level+1);
            }
        }
    } else {
        report("***NULL***\n");
    }
}
