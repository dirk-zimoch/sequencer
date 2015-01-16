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
                Generate tables for runtime sequencer
\*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#ifdef _WIN32
#  include <malloc.h>
#elif (__STDC_VERSION__ < 199901L) && !defined(__GNUC__)
#  include <alloca.h>
#endif

#include "seq_snc.h"
#include "analysis.h"
#include "main.h"
#include "sym_table.h"
#include "gen_code.h"
#include "node.h"
#include "var_types.h"
#include "gen_tables.h"

typedef struct event_mask_args {
	seqMask	*event_words;
	uint	num_event_flags;
} event_mask_args;

static void gen_state_table(Node *ss_list, uint num_event_flags, uint num_channels);
static void fill_state_struct(Node *sp, char *ss_name, uint ss_num);
static void gen_prog_table(Program *p);
static void encode_options(Options options);
static void encode_state_options(StateOptions options);
static void gen_ss_table(Node *ss_list);
static void gen_state_event_mask(Node *sp, uint num_event_flags,
	seqMask *event_words, uint num_event_words);
static int iter_event_mask_scalar(Node *ep, Node *scope, void *parg);
static int iter_event_mask_array(Node *ep, Node *scope, void *parg);

/* Generate all kinds of tables for a SNL program. */
void gen_tables(Program *p)
{
	gen_code("\n/************************ Tables ************************/\n");
	gen_state_table(p->prog->prog_statesets, p->num_event_flags, p->chan_list->num_elems);
	gen_ss_table(p->prog->prog_statesets);
	gen_prog_table(p);
}

/* Generate state event mask and table */
static void gen_state_table(Node *ss_list, uint num_event_flags, uint num_channels)
{
	Node	*ssp;
	Node	*sp;
	uint	n;
	uint	num_event_words = NWORDS(num_event_flags + num_channels);
	uint	ss_num = 0;

#if (__STDC_VERSION__ >= 199901L) || defined(__GNUC__)
	seqMask	event_mask[num_event_words];
#else
	seqMask	*event_mask = (seqMask *)alloca(num_event_words*sizeof(seqMask));
#endif

	/* NOTE: Bit zero of event mask is not used. Bit 1 to num_event_flags
	   are used for event flags, then come channels. */

	/* For each state set... */
	foreach (ssp, ss_list)
	{
		/* Generate event mask array */
		gen_code("\n/* Event masks for state set \"%s\" */\n", ssp->token.str);
		foreach (sp, ssp->ss_states)
		{
			gen_state_event_mask(sp, num_event_flags, event_mask, num_event_words);
			gen_code("static const seqMask " NM_MASK "_%s_%d_%s[] = {\n",
				ssp->token.str, ss_num, sp->token.str);
			for (n = 0; n < num_event_words; n++)
				gen_code("\t0x%08x,\n", event_mask[n]);
			gen_code("};\n");
		}

		/* Generate table of state structures */
		gen_code("\n/* State table for state set \"%s\" */\n", ssp->token.str);
		gen_code("static seqState " NM_STATES "_%s[] = {\n", ssp->token.str);
		foreach (sp, ssp->ss_states)
		{
			fill_state_struct(sp, ssp->token.str, ss_num);
		}
		gen_code("};\n");
		ss_num++;
	}
}

/* Generate a state struct */
static void fill_state_struct(Node *sp, char *ss_name, uint ss_num)
{
	gen_code("\t{\n");
	gen_code("\t/* state name */        \"%s\",\n", sp->token.str);
	gen_code("\t/* action function */   " NM_ACTION "_%s_%d_%s,\n", ss_name, ss_num, sp->token.str);
	gen_code("\t/* event function */    " NM_EVENT "_%s_%d_%s,\n", ss_name, ss_num, sp->token.str);
	gen_code("\t/* entry function */    ");
	if (sp->state_entry)
		gen_code(NM_ENTRY "_%s_%d_%s,\n", ss_name, ss_num, sp->token.str);
	else
		gen_code("0,\n");
	gen_code("\t/* exit function */     ");
	if (sp->state_exit)
		gen_code(NM_EXIT "_%s_%d_%s,\n", ss_name, ss_num, sp->token.str);
	else
		gen_code("0,\n");
	gen_code("\t/* event mask array */  " NM_MASK "_%s_%d_%s,\n", ss_name, ss_num, sp->token.str);
	gen_code("\t/* state options */     ");
	encode_state_options(sp->extra.e_state->options);
	gen_code("\n\t},\n");
}

/* Generate the state option bitmask */
static void encode_state_options(StateOptions options)
{
	gen_code("(0");
	if (!options.do_reset_timers)
		gen_code(" | OPT_NORESETTIMERS");
	if (!options.no_entry_from_self)
		gen_code(" | OPT_DOENTRYFROMSELF");
	if (!options.no_exit_to_self)
		gen_code(" | OPT_DOEXITTOSELF");
	gen_code(")");
} 

/* Generate state set table, one entry for each state set */
static void gen_ss_table(Node *ss_list)
{
	Node	*ssp;
	int	num_ss;

	gen_code("\n/* State set table */\n");
	gen_code("static seqSS " NM_STATESETS "[] = {\n");
	num_ss = 0;
	foreach (ssp, ss_list)
	{
		if (num_ss > 0)
			gen_code("\n");
		num_ss++;
		gen_code("\t{\n");
		gen_code("\t/* state set name */    \"%s\",\n", ssp->token.str);
		gen_code("\t/* states */            " NM_STATES "_%s,\n", ssp->token.str);
		gen_code("\t/* number of states */  %d\n", ssp->extra.e_ss->num_states);
		gen_code("\t},\n");
	}
	gen_code("};\n");
}

/* Generate a single program structure ("seqProgram") */
static void gen_prog_table(Program *p)
{
	gen_code("\n/* Program table (global) */\n");
	gen_code("seqProgram %s = {\n", p->name);
	gen_code("\t/* magic number */      %d,\n", MAGIC);
	gen_code("\t/* program name */      \"%s\",\n", p->name);
	gen_code("\t/* num. channels */     %d,\n", p->chan_list->num_elems);
	gen_code("\t/* state sets */        " NM_STATESETS ",\n");
	gen_code("\t/* num. state sets */   %d,\n", p->num_ss);
	if (p->options.reent)
		gen_code("\t/* user var size */     sizeof(struct %s),\n", NM_VARS);
	else
		gen_code("\t/* user var size */     0,\n");
	gen_code("\t/* param */             \"%s\",\n", p->param);
	gen_code("\t/* num. event flags */  %d,\n", p->num_event_flags);
	gen_code("\t/* encoded options */   "); encode_options(p->options);
	gen_code("\t/* init func */         " NM_INIT ",\n");
	gen_code("\t/* entry func */        %s,\n", p->prog->prog_entry ? NM_ENTRY : "0");
	gen_code("\t/* exit func */         %s,\n", p->prog->prog_exit ? NM_EXIT : "0");
	gen_code("\t/* num. queues */       %d\n", p->syncq_list->num_elems);
	gen_code("};\n");
}

static void encode_options(Options options)
{
	gen_code("(0");
	if (options.async)
		gen_code(" | OPT_ASYNC");
	if (options.conn)
		gen_code(" | OPT_CONN");
	if (options.debug)
		gen_code(" | OPT_DEBUG");
	if (options.newef)
		gen_code(" | OPT_NEWEF");
	if (options.reent)
		gen_code(" | OPT_REENT");
	if (options.safe)
		gen_code(" | OPT_SAFE");
	gen_code("),\n");
}

/* Generate event mask for a single state. The event mask has a bit set for each
   event flag and for each process variable (assigned var) used in one of the
   state's when() conditions. The bits from 1 to num_event_flags are for the
   event flags. The bits from num_event_flags+1 to num_event_flags+num_channels
   are for process variables. Bit zero is not used for whatever mysterious reason
   I cannot tell. */
static void gen_state_event_mask(Node *sp, uint num_event_flags,
	seqMask *event_words, uint num_event_words)
{
	uint	n;
	Node	*tp;

	for (n = 0; n < num_event_words; n++)
		event_words[n] = 0;

	/* Look at the when() conditions for references to event flags
	 * and assigned variables.  Database variables might have a subscript,
	 * which could be a constant (set a single event bit) or an expression
	 * (set a group of bits for the possible range of the evaluated expression)
	 */
	foreach (tp, sp->state_whens)
	{
		event_mask_args em_args = { event_words, num_event_flags };

		/* look for scalar variables and event flags */
		traverse_syntax_tree(tp->when_cond, bit(E_VAR), 0, 0,
			iter_event_mask_scalar, &em_args);

		/* look for arrays and subscripted array elements */
		traverse_syntax_tree(tp->when_cond, bit(E_VAR)|bit(E_SUBSCR), 0, 0,
			iter_event_mask_array, &em_args);
	}
#ifdef DEBUG
	report("event mask for state %s is", sp->token.str);
	for (n = 0; n < num_event_words; n++)
		report(" 0x%lx", (unsigned long)event_words[n]);
	report("\n");
#endif
}

#define bitnum(var_ix, ch_ix, num_efs) ((var_ix)+(ch_ix)+(num_efs)+1)

/* Iteratee for scalar variables (including event flags). */
static int iter_event_mask_scalar(Node *ep, Node *scope, void *parg)
{
	event_mask_args	*em_args = (event_mask_args *)parg;
	Chan		*cp;
	Var		*vp;
	uint		num_event_flags = em_args->num_event_flags;
	seqMask		*event_words = em_args->event_words;
	Type		*t;

#ifdef DEBUG
	report("  iter_event_mask_scalar: enter\n");
#endif
	assert(ep->tag == E_VAR);
	vp = ep->extra.e_var;
	assert(vp != 0);

	if (vp->type->tag == T_EVFLAG)
	{
#ifdef DEBUG
		report("  iter_event_mask_scalar: evflag: %s, ef_num=%d\n",
			vp->name, vp->chan.evflag->index);
#endif
		bitSet(event_words, vp->chan.evflag->index);
		return FALSE;		/* no children anyway */
	}
#ifdef DEBUG
	report("  iter_event_mask_scalar: name=%s, type=\n", vp->name);
	dump_type(vp->type, 2);
	report("    assign=%d\n", vp->assign);
#endif
	t = type_contains_pv(vp->type);
#ifdef DEBUG
	report("    type_under_pv=\n");
	if (t)
		dump_type(t, 2);
#endif
	if (!t || strip_pv_type(t)->tag != T_PRIM)
		return FALSE;		/* no children anyway */

	/* if not associated with channel, return */
	if (vp->assign == M_NONE)
		return FALSE;
	if (vp->assign != M_SINGLE)	/* what about by L3? */
		return FALSE;
	cp = vp->chan.single;

	bitSet(event_words, bitnum(vp->index,cp->index,num_event_flags));
#ifdef DEBUG
	report("  iter_event_mask_scalar: var: %s, event bit=%d+%d+%d+1=%d\n",
		vp->name, vp->index, cp->index, num_event_flags,
		bitnum(vp->index,cp->index,num_event_flags));
#endif
	return FALSE;		/* no children anyway */
}

/* Iteratee for array variables. */
static int iter_event_mask_array(Node *ep, Node *scope, void *parg)
{
	event_mask_args	*em_args = (event_mask_args *)parg;
	uint		num_event_flags = em_args->num_event_flags;
	seqMask		*event_words = em_args->event_words;

	Var		*vp=0;
	Node		*e_var=0, *e_ix=0;

#ifdef DEBUG
	report("  iter_event_mask_array: enter\n");
#endif
	assert(ep->tag == E_SUBSCR || ep->tag == E_VAR);

	if (ep->tag == E_SUBSCR)
	{
		e_var = ep->subscr_operand;
		e_ix = ep->subscr_index;
		assert(e_var != 0);
		assert(e_ix != 0);
		if (e_var->tag != E_VAR)
			return TRUE;
	}
	if (ep->tag == E_VAR)
	{
		e_var = ep;
		e_ix = 0;
	}

	vp = e_var->extra.e_var;
	assert(vp != 0);

	/* this subroutine handles only the array variables */
	if (vp->type->tag != T_ARRAY)
		return TRUE;

	assert(vp->type->tag == T_ARRAY);

	if (vp->assign == M_NONE)
	{
		return FALSE;
	}
	else if (vp->assign == M_SINGLE)
	{
		uint ix = vp->chan.single->index;
#ifdef DEBUG
		report("  iter_event_mask_array: %s, event bit=%d+%d+%d+1=%d\n",
			vp->name, vp->index, ix, num_event_flags,
			bitnum(vp->index,ix,num_event_flags));
#endif
		bitSet(event_words, bitnum(vp->index,ix,num_event_flags));
		return TRUE;
	}
	else
	{
		uint length1 = type_array_length1(vp->type);

		assert(vp->assign == M_MULTI);
		/* an array variable subscripted with a constant */
		if (e_ix && e_ix->tag == E_CONST)
		{
			uint ix;

			if (!strtoui(e_ix->token.str, length1, &ix))
			{
				error_at_node(e_ix,
					"subscript in '%s[%s]' out of range\n",
					vp->name, e_ix->token.str);
				return FALSE;
			}
#ifdef DEBUG
			report("  iter_event_mask_array: %s, event bit=%d+%d+%d+1=%d\n",
				vp->name, vp->index, ix, num_event_flags,
                                bitnum(vp->index,ix,num_event_flags));
#endif
			bitSet(event_words, bitnum(vp->index,ix,num_event_flags));
			return FALSE;	/* important: do NOT descend further
				   	   otherwise will find the array var and
				   	   set all the bits (see below) */
		}
		else if (e_ix)	/* subscript is an expression */
		{
			/* must descend for the array variable (see below) and
			   possible array vars inside subscript expression */
			return TRUE;
		}
		else /* no subscript */
		{
			/* set all event bits for this variable */
			uint ix;
#ifdef DEBUG
			report("  iter_event_mask_array: %s, event bits=%d..(%d)\n",
				vp->name, bitnum(vp->index,0,num_event_flags),
				bitnum(vp->index,length1,num_event_flags));
#endif
			for (ix = 0; ix < length1; ix++)
			{
				bitSet(event_words, bitnum(vp->index,ix,num_event_flags));
			}
			return FALSE;	/* no children anyway */
		}
	}
}
