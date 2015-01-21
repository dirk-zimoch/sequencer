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
} EventMaskArgs;

static void gen_state_table(Node *ss_list, uint num_event_flags, uint num_channels);
static void fill_state_struct(Node *sp, char *ss_name, uint ss_num);
static void gen_prog_table(Node *prog);
static void encode_options(Options *options);
static void encode_state_options(StateOptions options);
static void gen_ss_table(Node *ss_list);
static void gen_state_event_mask(Node *sp, uint num_event_flags,
	seqMask *event_words, uint num_event_words);

/* Generate all kinds of tables for a SNL program. */
void gen_tables(Node *prog)
{
	Program *p;

	assert(prog->tag == D_PROG);
	p = prog->extra.e_prog;

	gen_code("\n/************************ Tables ************************/\n");
	gen_state_table(prog->prog_statesets, p->evflag_list->num_elems, p->chan_list->num_elems);
	gen_ss_table(prog->prog_statesets);
	gen_prog_table(prog);
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
static void gen_prog_table(Node *prog)
{
	Program *p = prog->extra.e_prog;

	gen_code("\n/* Program table (global) */\n");
	gen_code("seqProgram %s = {\n", prog->token.str);
	gen_code("\t/* magic number */      %d,\n", MAGIC);
	gen_code("\t/* program name */      \"%s\",\n", prog->token.str);
	gen_code("\t/* num. channels */     %d,\n", p->chan_list->num_elems);
	gen_code("\t/* state sets */        " NM_STATESETS ",\n");
	gen_code("\t/* num. state sets */   %d,\n", p->num_ss);
	if (p->options->reent)
		gen_code("\t/* user var size */     sizeof(struct %s),\n", NM_VARS);
	else
		gen_code("\t/* user var size */     0,\n");
	gen_code("\t/* param */             \"%s\",\n",
		prog->prog_param ? prog->prog_param->token.str : "");
	gen_code("\t/* num. event flags */  %d,\n", p->evflag_list->num_elems);
	gen_code("\t/* encoded options */   "); encode_options(p->options);
	gen_code("\t/* init func */         " NM_INIT ",\n");
	gen_code("\t/* entry func */        %s,\n", prog->prog_entry ? NM_ENTRY : "0");
	gen_code("\t/* exit func */         %s,\n", prog->prog_exit ? NM_EXIT : "0");
	gen_code("\t/* num. queues */       %d\n", p->syncq_list->num_elems);
	gen_code("};\n");
}

static void encode_options(Options *options)
{
	gen_code("(0");
	if (options->async)
		gen_code(" | OPT_ASYNC");
	if (options->conn)
		gen_code(" | OPT_CONN");
	if (options->debug)
		gen_code(" | OPT_DEBUG");
	if (options->newef)
		gen_code(" | OPT_NEWEF");
	if (options->reent)
		gen_code(" | OPT_REENT");
	if (options->safe)
		gen_code(" | OPT_SAFE");
	gen_code("),\n");
}

/*
 * A simpler version of the same named function in analysis.c.
 * It works on arbitrary expressions and returns NULL if the
 * expression is not a var_expr.
 */
static ChanNode *traverse_var_expr(Node *vxp)
{
	Type *t;
	ChanNode *chan_node;
	uint index;
	Node *member;

	switch (vxp->tag)
	{
	case E_VAR:
		return vxp->extra.e_var->chan;
	case E_SUBSCR:
		chan_node = traverse_var_expr(vxp->subscr_operand);
		if (!chan_node)
			return 0;
		t = chan_node->type;
		if (t->tag != T_ARRAY)
			return 0;
		if (vxp->subscr_index->tag != E_CONST)
			return chan_node;
		if (!strtoui(vxp->subscr_index->token.str, t->val.array.num_elems, &index))
		{
			error_at_node(vxp->subscr_index, "subscript '[%s]' out of range\n",
				vxp->subscr_index->token.str);
			return 0;
		}
		return chan_node->val.nodes[index];
	case E_SELECT:
		chan_node = traverse_var_expr(vxp->select_left);
		if (!chan_node)
			return 0;
		t = chan_node->type;
		if (t->tag != T_STRUCT)
			return 0;
		if (vxp->select_right->tag != E_MEMBER)
			return chan_node;
		index = 0;
		foreach (member, t->val.structure.member_decls)
		{
			if (strcmp(member->token.str, vxp->select_right->token.str) == 0)
				break;
			index++;
		}
		return chan_node->val.nodes[index];
	default:
		return 0;
	}
}

static uint set_channel_bit(Chan *chan, void *env)
{
	EventMaskArgs *args = (EventMaskArgs *)env;
	bitSet(args->event_words, 1 + args->num_event_flags + chan->index);
	return FALSE;
}

static uint set_evflag_bit(EvFlag *ef, void *env)
{
	EventMaskArgs *args = (EventMaskArgs *)env;
	bitSet(args->event_words, 1 + ef->index);
	return FALSE;
}

static int iter_event_mask(Node *expr, Node *scope, void *parg)
{
	ChanNode *chan_node;

	chan_node = traverse_var_expr(expr);
	traverse_channel_tree(chan_node, set_channel_bit, set_evflag_bit, parg);
	return FALSE;
}

/* Generate event mask for a single state. The event mask has a bit set for each
   event flag and for each process variable (assigned var) used in one of the
   state's when() conditions. The bits from 1 to num_event_flags are for the
   event flags. The bits from num_event_flags+1 to num_event_flags+num_channels
   are for process variables. Bit zero is used in the runtime system to denote
   all possible events. */
static void gen_state_event_mask(Node *sp, uint num_event_flags,
	seqMask *event_words, uint num_event_words)
{
	uint		n;
	Node		*tp;
	EventMaskArgs	em_args = { event_words, num_event_flags };

	for (n = 0; n < num_event_words; n++)
		event_words[n] = 0;

	foreach (tp, sp->state_whens)
	{
		traverse_syntax_tree(tp->when_cond,
			bit(E_VAR)|bit(E_SUBSCR)|bit(E_SELECT), 0, 0,
			iter_event_mask, &em_args);
	}
#ifdef DEBUG
	report("event mask for state %s is ", sp->token.str);
#if 0
	for (n = 0; n < num_event_words; n++)
		report("%8lx", (unsigned long)event_words[n]);
#endif
	dump_mask(event_words, num_event_words);
	report("\n");
#endif
}

#define WORD_BIN_FMT "%u%u%u%u%u%u%u%u'%u%u%u%u%u%u%u%u'%u%u%u%u%u%u%u%u'%u%u%u%u%u%u%u%u"
#define WORD_BIN(word) \
  ((word) & (1u<<31) ? 1u : 0), \
  ((word) & (1u<<30) ? 1u : 0), \
  ((word) & (1u<<29) ? 1u : 0), \
  ((word) & (1u<<28) ? 1u : 0), \
  ((word) & (1u<<27) ? 1u : 0), \
  ((word) & (1u<<26) ? 1u : 0), \
  ((word) & (1u<<25) ? 1u : 0), \
  ((word) & (1u<<24) ? 1u : 0), \
  ((word) & (1u<<23) ? 1u : 0), \
  ((word) & (1u<<22) ? 1u : 0), \
  ((word) & (1u<<21) ? 1u : 0), \
  ((word) & (1u<<20) ? 1u : 0), \
  ((word) & (1u<<19) ? 1u : 0), \
  ((word) & (1u<<18) ? 1u : 0), \
  ((word) & (1u<<17) ? 1u : 0), \
  ((word) & (1u<<16) ? 1u : 0), \
  ((word) & (1u<<15) ? 1u : 0), \
  ((word) & (1u<<14) ? 1u : 0), \
  ((word) & (1u<<13) ? 1u : 0), \
  ((word) & (1u<<12) ? 1u : 0), \
  ((word) & (1u<<11) ? 1u : 0), \
  ((word) & (1u<<10) ? 1u : 0), \
  ((word) & (1u<< 9) ? 1u : 0), \
  ((word) & (1u<< 8) ? 1u : 0), \
  ((word) & (1u<< 7) ? 1u : 0), \
  ((word) & (1u<< 6) ? 1u : 0), \
  ((word) & (1u<< 5) ? 1u : 0), \
  ((word) & (1u<< 4) ? 1u : 0), \
  ((word) & (1u<< 3) ? 1u : 0), \
  ((word) & (1u<< 2) ? 1u : 0), \
  ((word) & (1u<< 1) ? 1u : 0), \
  ((word) & (1u<< 0) ? 1u : 0)

void dump_mask(seqMask *mask, uint num_words)
{
	int n;
	for (n = num_words-1; n >= 0; n--)
	{
		report("%s"WORD_BIN_FMT, n?"'":"", WORD_BIN(mask[n]));
	}
}
