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
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#include "node.h"
#include "analysis.h"
#include "gen_code.h"
#include "main.h"
#include "builtin.h"
#include "gen_ss_code.h"
#include "type_check.h"
#include "snl.h"

static const int impossible = 0;

static void gen_state_func(
	const char *ss_name,
	uint ss_num,
	const char *state_name,
	Node *xp,
	void (*gen_body)(uint context, Node *xp),
	uint context,
	const char *title,
	const char *prefix,
	const char *rettype,
	const char *extra_args
);
static void gen_entex_body(uint context, Node *xp);
static void gen_event_body(uint context, Node *xp);
static void gen_action_body(uint context, Node *xp);
static void gen_statement(
	uint context,
	Type *return_type,	/* type expected for a return expression */
	Node *ep,		/* node to generate code for */
	int level		/* indentation level */
);
static void gen_expr(
	uint context,
	Type *expected,		/* expected type of expression */
	Node *ep,		/* expression to generate code for */
	int level		/* indentation level */
);
static void gen_prog_func(
	uint context,
	Node *prog,
	ChanList *channels,
	EvFlagList *event_flags,
	const char *doc,
	const char *name,
	void (*gen_body)(uint context, Node *prog, ChanList *channels, EvFlagList *event_flags)
);
static void gen_prog_entex_func(
	uint context,
	Node *prog,
	const char *doc,
	const char *name,
	void (*gen_body)(uint context, Node *)
);
static void gen_prog_init_body(uint context, Node *prog, ChanList *channels, EvFlagList *event_flags);
static void gen_prog_entry_body(uint context, Node *prog);
static void gen_prog_exit_body(uint context, Node *prog);

/*
 * Expression context. Certain nodes of the syntax tree are
 * interpreted differently depending on the context in which
 * they appear. For instance, the state change command is only
 * allowed in transition action context (C_TRANS flag set).
 */
enum context_flag
{
	C_COND,		/* when() condition */
	C_TRANS,	/* state transition actions */
	C_FUNC,		/* function definition */
	C_STATIC,	/* static initialiser */
	C_CHID,		/* expected channel id */
	C_PLAIN,	/* generate variable(s) as in SNL */
	C_REENT,	/* reentrant option is active */
	C_LAX,		/* lax type checking a la C */
};

#define ctxClear(ctx,flag)	( (ctx) & (~(1u<<(flag+16))) )
#define ctxSet(ctx,flag)	( (ctx) | ((1u<<(flag+16))) )
#define ctxEmpty		( 0u )
#define ctxTest(ctx,flag)	( ((ctx) & (1u<<(flag+16))) )

void dump_ctx(uint context)
{
	report("%s%s%s%s%s%s%s%s",
		ctxTest(context,C_COND)?"O":"",
		ctxTest(context,C_TRANS)?"T":"",
		ctxTest(context,C_FUNC)?"F":"",
		ctxTest(context,C_STATIC)?"S":"",
		ctxTest(context,C_CHID)?"C":"",
		ctxTest(context,C_PLAIN)?"P":"",
		ctxTest(context,C_REENT)?"R":"",
		ctxTest(context,C_LAX)?"X":"");
}

unsigned default_context(Options *options)
{
	uint	context = ctxEmpty;

	if (options->reent)
		context = ctxSet(context,C_REENT);
	return context;
}

/* Generate state set C code from analysed syntax tree */
void gen_ss_code(uint context, Node *prog, ChanList *channels, EvFlagList *event_flags)
{
	Node	*sp, *ssp;
	uint	ss_num = 0;

	/* Generate program init function */
	gen_code("#define " NM_VAR " (*(struct " NM_VARS " *const *)" NM_ENV ")\n");
	gen_prog_func(context, prog, channels, event_flags, "init", NM_INIT, gen_prog_init_body);

	/* Generate program entry function */
	if (prog->prog_entry)
		gen_prog_entex_func(context, prog, "entry", NM_ENTRY, gen_prog_entry_body);

	/* For each state set ... */
	foreach (ssp, prog->prog_statesets)
	{
		/* For each state ... */
		foreach (sp, ssp->ss_states)
		{
			gen_code("\n/****** Code for state \"%s\" in state set \"%s\" ******/\n",
				sp->token.str, ssp->token.str);

			/* Generate entry and exit functions */
			if (sp->state_entry)
				gen_state_func(ssp->token.str, ss_num, sp->token.str, 
					sp->state_entry, gen_entex_body,
					context, "Entry", NM_ENTRY, "void", "");
			if (sp->state_exit)
				gen_state_func(ssp->token.str, ss_num, sp->token.str,
					sp->state_exit, gen_entex_body,
					context, "Exit", NM_EXIT, "void", "");
			/* Generate event processing function */
			gen_state_func(ssp->token.str, ss_num, sp->token.str,
				sp->state_whens, gen_event_body,
				context, "Event", NM_EVENT, "seqBool",
				", int *"NM_PTRN", int *"NM_PNST);
			/* Generate action processing function */
			gen_state_func(ssp->token.str, ss_num, sp->token.str,
				sp->state_whens, gen_action_body,
				context, "Action", NM_ACTION, "void",
				", int "NM_TRN", int *"NM_PNST);
		}
		ss_num++;
	}

	/* Generate program exit function */
	if (prog->prog_exit)
		gen_prog_entex_func(context, prog, "exit", NM_EXIT, gen_prog_exit_body);

	gen_code("#undef " NM_VAR "\n");
}

static void gen_state_func(
	const char *ss_name,
	uint ss_num,
	const char *state_name,
	Node *xp,
	void (*gen_body)(uint context, Node *xp),
	uint context,
	const char *title,
	const char *prefix,
	const char *rettype,
	const char *extra_args
)
{
	gen_code("\n/* %s function for state \"%s\" in state set \"%s\" */\n",
		title, state_name, ss_name);
	gen_code("static %s %s_%s_%d_%s(SS_ID " NM_ENV "%s)\n",
		rettype, prefix, ss_name, ss_num, state_name, extra_args);
	gen_body(context, xp);
}

static void gen_block(uint context, Type *return_type, Node *xp, int level)
{
	Node	*cxp;

	assert(xp->tag == S_CMPND);			/* precondition */
	indent(level); gen_code("{\n");

	foreach (cxp, xp->cmpnd_defns)
	{
		if (cxp->tag == D_DECL)
		{
			Var *vp = cxp->extra.e_decl;
			assert(vp->type->tag != T_NONE);	/* syntax */
			assert(vp->type->tag != T_EVFLAG);	/* syntax */
			assert(vp->decl);			/* invariant */

			gen_line_marker(vp->decl);
			indent(level+1);
			gen_var_decl(vp);

			/* optional initialisation */
			if (cxp->decl_init)
			{
				gen_code(" = ");
				gen_expr(context, vp->type, vp->decl->decl_init, level+1);
			}
			gen_code(";\n");
		}
		else if (cxp->tag == T_TEXT)
		{
			gen_line_marker(cxp);
			indent(level+1);
			gen_code("%s\n", cxp->token.str);
		}
	}

	foreach (cxp, xp->cmpnd_stmts)
	{
		gen_statement(context, return_type, cxp, level+1);
	}

	indent(level); gen_code("}\n");
}

static void gen_entex_body(uint context, Node *xp)
{
	assert(xp->tag == D_ENTEX);			/* precondition */
	assert(xp->entex_block->tag == S_CMPND);	/* invariant */
	gen_block(context, mk_no_type(), xp->entex_block, 0);
}

static void gen_action_body(uint context, Node *xp)
{
	Node		*tp;
	int		trans_num;
	const int	level = 1;

	gen_code("{\n");
	/* "switch" statment based on the transition number */
	indent(level); gen_code("switch(" NM_TRN ")\n");
	indent(level); gen_code("{\n");
	trans_num = 0;

	/* For each transition ("when" statement) ... */
	foreach (tp, xp)
	{
		assert(tp->tag == D_WHEN);		/* precondition */
		/* one case for each transition */
		indent(level); gen_code("case %d:\n", trans_num);
		gen_block(ctxSet(context,C_TRANS), mk_no_type(), tp->when_block, level+1);
		/* end of case */
		indent(level+1); gen_code("return;\n");
		trans_num++;
	}
	/* end of switch stmt */
	indent(level); gen_code("}\n");
	/* end of function */
	gen_code("}\n");
}

static Type *mk_bool_type(void)
{
	return mk_prim_type(P_INT);
}

static Type *mk_index_type(void)
{
	return mk_prim_type(P_LONG);
}

/* Generate a C function that checks events for a particular state */
static void gen_event_body(uint context, Node *xp)
{
	Node		*tp;
	int		trans_num;
	const int	level = 1;

	gen_code("{\n");
	trans_num = 0;
	/* For each transition generate an "if" statement ... */
	foreach (tp, xp)
	{
		Node *next_sp;

		assert(tp->tag == D_WHEN);		/* precondition */
		if (tp->when_cond)
			gen_line_marker(tp->when_cond);
		indent(level); gen_code("if (");
		if (tp->when_cond == 0)
			gen_code("TRUE");
		else
			gen_expr(context, mk_bool_type(), tp->when_cond, level);
		gen_code(")\n");
		indent(level); gen_code("{\n");

		next_sp = tp->extra.e_when->next_state;
		if (!next_sp)
		{
			/* "when(...) {...} exit" -> exit from program */
			indent(level+1);
			gen_code("seq_exit(" NM_ENV ");\n");
		}
		else
		{
			indent(level+1);
			gen_code("*" NM_PNST " = %d;\n", next_sp->extra.e_state->index);
		}
		indent(level+1);gen_code("*" NM_PTRN " = %d;\n", trans_num);
		indent(level+1); gen_code("return TRUE;\n");
		indent(level); gen_code("}\n");
		trans_num++;
	}
	indent(level); gen_code("return FALSE;\n");
	/* end of function */
	gen_code("}\n");
}

static void gen_statement(
	uint context,
	Type *return_type,
	Node *ep,		/* node to generate code for */
	int level		/* indentation level */
)
{
	Node	*cep;		/* child node */

	if (ep == 0)
		return;

#ifdef	DEBUG
	report("gen_expr(%s,%s)\n", node_name(ep), ep->token.str);
#endif
	switch(ep->tag)
	{
	/* Statements */
	case S_CMPND:
		gen_block(context, return_type, ep, level);
		break;
	case S_STMT:
		gen_line_marker(ep);
		indent(level);
		gen_expr(context, mk_void_type(), ep->stmt_expr, level);
		gen_code(";\n");
		break;
	case S_IF:
		gen_line_marker(ep);
		indent(level);
		gen_code("if (");
		gen_expr(context, mk_bool_type(), ep->if_cond, level);
		gen_code(")\n");
		cep = ep->if_then;
		gen_statement(context, return_type, cep,
			cep->tag == S_CMPND ? level : level+1);
		if (ep->if_else)
		{
			indent(level);
			gen_code("else\n");
			cep = ep->if_else;
			gen_statement(context, return_type, cep,
				cep->tag == S_CMPND ? level : level+1);
		}
		break;
	case S_WHILE:
		gen_line_marker(ep);
		indent(level);
		gen_code("while (");
		gen_expr(context, mk_bool_type(), ep->while_cond, level);
		gen_code(")\n");
		cep = ep->while_stmt;
		gen_statement(context, return_type, cep,
			cep->tag == S_CMPND ? level : level+1);
		break;
	case S_FOR:
		gen_line_marker(ep);
		indent(level);
		gen_code("for (");
		gen_expr(context, mk_void_type(), ep->for_init, level);
		gen_code("; ");
		gen_expr(context, mk_bool_type(), ep->for_cond, level);
		gen_code("; ");
		gen_expr(context, mk_void_type(), ep->for_iter, level);
		gen_code(")\n");
		cep = ep->for_stmt;
		gen_statement(context, return_type, cep,
			cep->tag == S_CMPND ? level : level+1);
		break;
	case S_JUMP:
		indent(level);
		gen_code("%s;\n", ep->token.str);
		break;
	case S_CHANGE:
		if (!ctxTest(context,C_TRANS))
		{
			error_at_node(ep, "state change statement not allowed here\n");
			break;
		}
		indent(level);
		gen_code("{*" NM_PNST " = %d; return;}\n",
			ep->extra.e_change->extra.e_state->index);
		break;
	case S_RETURN:
		if (!ctxTest(context,C_FUNC))
		{
			error_at_node(ep, "return statement not allowed here\n");
			break;
		}
		indent(level);
		gen_code("return ");
		gen_expr(context, return_type, ep->return_expr, level);
		gen_code(";\n");
		break;
	case T_TEXT:
		indent(level);
		gen_code("%s\n", ep->token.str);
		break;
	default:
		error_at_node(ep, "unhandled syntax node (%s:%s)\n",
			node_name(ep), ep->token.str);
		assert(impossible);
	}
}

static void gen_var_access(uint context, Var *vp)
{
	const char *ch_pre = type_contains_pv(vp->type) && ctxTest(context,C_CHID) ? NM_CHID : "";
	const char *pre = ctxTest(context,C_REENT) ? NM_VAR "->" : "";

	assert(vp);				/* precondition */
	assert(vp->scope);			/* invariant */

#ifdef DEBUG
	report("gen_var_access: %s, context=",vp->name);
	dump_ctx(context);
	report(", scope=(%s,%s)\n",
		node_name(vp->scope), vp->scope->token.str);
#endif

	assert(ctxTest(context,C_CHID) || !type_contains_pv(vp->type)
		|| vp->scope->tag == D_PROG
		|| vp->scope->tag == D_SS
		|| vp->scope->tag == D_STATE);

	assert(is_scope(vp->scope));		/* invariant */

	if (vp->type->tag == T_NONE		/* foreign entity */
		|| vp->type->tag == T_FUNCTION	/* SNL function */
		|| ctxTest(context,C_PLAIN))	/* explicitly requested */
	{
		gen_code("%s", vp->name);
	}
	else if (vp->scope->tag == D_PROG)
	{
		gen_code("%s%s%s", pre, ch_pre, vp->name);
	}
	else if (vp->scope->tag == D_SS)
	{
		gen_code("%s%s_%s.%s%s", pre, NM_VARS, vp->scope->token.str, ch_pre, vp->name);
	}
	else if (vp->scope->tag == D_STATE)
	{
		gen_code("%s%s_%s.%s_%s.%s%s", pre, NM_VARS,
			vp->scope->extra.e_state->var_list->parent_scope->token.str,
			NM_VARS, vp->scope->token.str, ch_pre, vp->name);
	}
	/* function parameter or compound stmt => direct variable access */
	else
	{
		gen_code("%s%s", ch_pre, vp->name);
	}
}

static void gen_func_args(uint context, Type *ft, Node *ep)
{
	Node *ap;

	assert(ep->tag == E_FUNC);		/* precondition */
	if (!ft)
	{
		/* foreign function: just generate argument expressions
		   and let the C compiler figure out what to make of it */
		foreach (ap, ep->func_args)
		{
			gen_expr(context, mk_no_type(), ap, 0);
			if (ap->next)
				gen_code(", ");
		}
	}
	else
	{
		Node *pd;

#ifdef DEBUG
		report("gen_func_args() function:\n");
		dump_node(ep, 1);
#endif

		assert(ft->tag == T_FUNCTION);	/* invariant */

		/* add arguments for implicit parameters */
		gen_code(NM_ENV);

		ap = ep->func_args;
		foreach (pd, ft->val.function.param_decls->next)
		{
			Type *pt = pd->extra.e_decl->type;

#ifdef DEBUG
			report("gen_func_args() parameter type:\n");
			dump_type(pt, 1);
#endif
			gen_code(", ");
#ifdef DEBUG
			report("gen_func_args() argument expr:\n");
#endif
			if (ap)
			{
#ifdef DEBUG
				dump_node(ap, 1);
#endif
				gen_expr(context, pt, ap, 0);
				ap = ap->next;
			}
			else if (pd->decl_init)
			{
				/* we have a default for the parameter */
#ifdef DEBUG
				dump_node(pd->decl_init, 1);
#endif
				gen_expr(ctxSet(context,C_STATIC), pt, pd->decl_init, 0);
			}
			else
			{
				error_at_node(ep, "not enough arguments\n");
				return;
			}
		}
		if (ap)
		{
			warning_at_node(ep, "discarding excess arguments\n");
		}
	}
}

static void gen_expr(
	uint context,
	Type *expected,
	Node *ep,		/* expression to generate code for */
	int level		/* indentation level */
)
{
	Node	*cep;		/* child expression */
	Type	*inferred;	/* inferred type of expression */

	if (ep == 0)
		return;

#ifdef	DEBUG
	report("gen_expr()\n");
	dump_node(ep,1);
#endif

	inferred = type_of(ep);
	if (expected->tag != T_PV && inferred->tag == T_PV
		&& !ctxTest(context,C_PLAIN)
		&& expected->tag != T_VOID)
	{
		/* We expect a non-pv expression, but inferred a pv type, so
		the expression should decay to a value type. Which means, in
		general, that we have to wrap the expression with a call to
		the seq_pvValue macro. See below for cases in which we can
		avoid this wrapping and access the object directly.

		There are a two exceptional cases we have to exclude:

		If C_PLAIN is in the context, we are interested in the
		expression exactly as in the SNL source (this typically
		means we generate a string literal).

		If we expect T_VOID, this is either because we are
		generating a statement (in which case the conversion to a
		value type is unnecessary), or because we have a special
		reason why we want to access the value directly (see e.g.
		gen_channel_init). */

#ifdef DEBUG
		report("maybe insert seq_pvValue\n");
#endif
		/* Optimisation: access C variable directly.
		This also makes the generated code more readable
		and allows certain initialisers to be static.

		TODO: Generalize this to all kinds of var expressions.
		*/
		if (ep->tag == E_VAR && (
			ep->extra.e_var->scope->tag == D_PROG ||
			ep->extra.e_var->scope->tag == D_SS
			)
		)
		{
			gen_var_access(context, ep->extra.e_var);
		}
		else
		{
			/* convert the pv expression to a C value */
			gen_code("seq_pvValue(");
			/* note this will *not* fail even though type inference
			   is inexact, because the inexactness is limited to
			   *value* expressions, and does not apply to
			   expressions that have pv type */
			gen_type(mk_pointer_type(inferred), "", "");
			gen_code(", ");
			gen_expr(ctxSet(context,C_LAX), mk_pv_type(expected), ep, level);
			gen_code(")");
		}
		return;
	}
	else if (expected->tag == T_PV)
	{
		/* For pv types, we must check for an exact match (modulo void). */
#ifdef DEBUG
		report_at_node(ep, "checking pv type, expected:\n");
		dump_type (expected, 1);
		report_at_node(ep, "got:\n");
		dump_type (inferred, 1);
#endif
		if (!ctxTest(context, C_LAX) && !pv_type_check(expected, inferred))
		{
			error_at_node(ep, "type mismatch, expected:\n");
			dump_type (expected, 1);
			report_at_node(ep, "got:\n");
			dump_type (inferred, 1);
			return;
		}
	}

	switch(ep->tag)
	{

	/* Expressions */
	case E_VAR:
		assert(ep->extra.e_var);
		if (type_contains_pv(expected))
			gen_var_access(ctxSet(context,C_CHID), ep->extra.e_var);
		else
			gen_var_access(context, ep->extra.e_var);
		break;
	case E_SUBSCR:
		gen_expr(context, mk_pointer_type(expected), ep->subscr_operand, level);
		gen_code("[");
		/* note: passing as array index discards pv from type */
		gen_expr(context, mk_index_type(), ep->subscr_index, level);
		gen_code("]");
		break;
	case E_CONST:
		gen_code("%s", ep->token.str);
		break;
	case E_STRING:
		gen_code("\"%s\"", ep->token.str);
		break;
	case E_FUNC:
		if (ctxTest(context,C_STATIC))
		{
			error_at_node(ep, "function call not allowed here\n");
		}
		else
		{
			Type *func_type = type_of(ep->func_expr);
			gen_expr(context, func_type, ep->func_expr, level);
			gen_code("(");
			gen_func_args(context, type_is_function(func_type), ep);
			gen_code(")");
		}
		break;
	case E_INIT:
		gen_code("{");
		foreach (cep, ep->init_elems)
		{
			gen_expr(context, expected, cep, level);
			if (cep->next)
				gen_code(", ");
		}
		gen_code("}");
		break;
	case E_TERNOP:
		gen_expr(context, mk_bool_type(), ep->ternop_cond, level);
		gen_code(" ? ");
		gen_expr(context, strip_pv_type(expected), ep->ternop_then, level);
		gen_code(" : ");
		gen_expr(context, strip_pv_type(expected), ep->ternop_else, level);
		break;
	case E_ASSOP:
#ifdef DEBUG
		report("gen_expr(E_ASSOP)\n");
		dump_node(ep,1);
#endif
		if (ctxTest(context,C_STATIC))
			error_at_node(ep, "assignment operator not allowed here\n");
		expected = strip_pv_type(type_of(ep->assop_left));
		gen_expr(context, expected, ep->assop_left, level);
		gen_code(" %s ", ep->token.str);
		gen_expr(context, expected, ep->assop_right, level);
		break;
	case E_BINOP:
		gen_expr(context, strip_pv_type(expected), ep->binop_left, level);
		gen_code(" %s ", ep->token.str);
		gen_expr(context, strip_pv_type(expected), ep->binop_right, level);
		break;
	case E_SELECT:
		if (ep->token.symbol == TOK_POINTER)
			gen_expr(context,
				mk_pointer_type(mk_structure_type("<anonymous>",
					new_decl(ep->select_right->token, expected))),
				ep->select_left, level);
		else
			gen_expr(context,
				mk_structure_type("<anonymous>",
					new_decl(ep->select_right->token, expected)),
				ep->select_left, level);
		gen_code("%s", ep->token.str);
		assert(ep->select_right->tag == E_MEMBER);	/* syntax */
		gen_code("%s%s", type_contains_pv(expected) ? NM_CHID : "", ep->select_right->token.str);
		break;
	case E_MEMBER:
		assert(impossible);
		break;
	case E_PAREN:
		gen_code("(");
		gen_expr(context, expected, ep->paren_expr, level);
		gen_code(")");
		break;
	case E_CAST:
		if (type_contains_pv(ep->cast_type->extra.e_decl->type))
		{
			error_at_node(ep, "cast to pv type not allowed\n");
		}
		else
		{
			gen_code("(");
			gen_var_decl(ep->cast_type->extra.e_decl);
			gen_code(")");
		}
		/* TODO: is it correct to expect the cast type or should this be no_type? */
		gen_expr(context, ep->cast_type->extra.e_decl->type, ep->cast_operand, level);
		break;
	case E_PRE:
		if (ep->token.symbol == TOK_PV)
		{
			dump_node(ep, 0);
			assert(impossible);
			break;
		}
		gen_code("%s", ep->token.str);
		switch (ep->token.symbol)
		{
		case TOK_INCR:
		case TOK_DECR:
			if (ctxTest(context,C_STATIC))
			{
				error_at_node(ep, "this operator is not allowed here\n");
				break;
			}
		case TOK_ADD:
		case TOK_SUB:
		case TOK_NOT:
		case TOK_TILDE:
		case TOK_SIZEOF:
			gen_expr(context, expected, ep->pre_operand, level);
			break;
		case TOK_ASTERISK:
			gen_expr(context, mk_pointer_type(expected), ep->pre_operand, level);
			break;
		case TOK_AMPERSAND:
			if (type_is_pointer(expected))
				expected = type_is_pointer(expected);
			if (ctxTest(context, C_REENT))
				gen_expr(context, expected, ep->pre_operand, level);
			else
				gen_expr(ctxClear(context, C_STATIC), expected, ep->pre_operand, level);
			break;
		default:
			dump_node(ep, 0);
			assert(impossible);
		}
		break;
	case E_POST:
		if (ctxTest(context,C_STATIC))
		{
			error_at_node(ep, "this operator is not allowed here\n");
			break;
		}
		gen_expr(context, expected, ep->post_operand, level);
		gen_code("%s", ep->token.str);
		break;
	case E_SIZEOF:
		assert(ep->sizeof_decl);
		assert(ep->sizeof_decl->extra.e_decl);
		assert(ep->sizeof_decl->extra.e_decl->type);
		gen_code("sizeof(");
		gen_var_decl(ep->sizeof_decl->extra.e_decl);
		gen_code(")");
		break;
	default:
		error_at_node(ep, "unhandled expression (%s:%s)\n",
			node_name(ep), ep->token.str);
		assert(impossible);
	}
}

static void gen_ef_init(uint context, EvFlag *ef)
{
	gen_line_marker(ef->expr);
	indent(1);
	gen_expr(context, mk_ef_type(), ef->expr, 0);
	gen_code(" = seq_efCreate("NM_ENV", %d, ", 1 + ef->index);
	if (ef->init)
		gen_expr(context, mk_bool_type(), ef->init, 0);
	else
		gen_code("FALSE");
	gen_code(");\n");
}

static void gen_channel_init(uint context, Chan *cp)
{
	Type *basetype, *value_type;
	enum prim_type_tag prim_tag;
	Monitor *mp;

	assert(cp->type->tag == T_PV);

	value_type = cp->type->val.pv.value_type;
	basetype = base_type(value_type);
	assert(basetype->tag == T_PRIM);
	prim_tag = basetype->val.prim;

	if (prim_tag == P_LONG || prim_tag == P_ULONG)
	{
		gen_code(
"#if LONG_MAX > 0x7fffffffL\n"
		);
		gen_line_marker(cp->expr);
		gen_code(
"#error expression '"
		);
		gen_expr(ctxSet(context,C_PLAIN), mk_no_type(), cp->expr, 0);
		gen_code("'"
" cannot be assigned to a PV (on the chosen target system)\\\n"
" because Channel Access does not support integral types longer than 4 bytes.\\\n"
" You can use '%s' instead, or the fixed size type '%s'.\n"
"#endif\n",
			prim_tag == P_LONG ? "int" : "unsigned int",
			prim_tag == P_LONG ? "int32_t" : "uint32_t"
		);
	}

#ifdef DEBUG
	report("gen_channel_init: cp->expr=\n");
	dump_node(cp->expr,1);
	report("gen_channel_init: type_of(cp->expr)=\n");
	dump_type(type_of(cp->expr), 1);
#endif

	/* left hand side (the channel object) */
	indent(1);
	gen_expr(context, mk_pv_type(mk_void_type()), cp->expr, 0);

	/* right hand side starts with the call */
	gen_code(" = seq_pvCreate("NM_ENV", ");

	/* index of channel; assigned channel name */
	if (!cp->name)
		gen_code("%d, 0, ", cp->index);
	else
		gen_code("%d, \"%s\", ", cp->index, cp->name);

	/* offset to value */
	if (ctxTest(context, C_REENT))
		gen_code("offsetof(struct %s, ", NM_VARS);
	else
		gen_code("(size_t)&");
	/* note: must remove C_REENT from the context here */
	gen_expr(ctxClear(context, C_REENT), mk_void_type(), cp->expr, 0);
	if (ctxTest(context, C_REENT))
		gen_code(")");

	/* channel expression as a string literal for use by the
	   runtime library (e.g. for error messages) */
	gen_code(", \"");
	gen_expr(ctxSet(context,C_PLAIN), mk_no_type(), cp->expr, 0);

	/* base value type */
	gen_code("\", %s, ", prim_type_tag_name[prim_tag]);

	/* element count for arrays */
	if (value_type->tag == T_ARRAY)
		gen_code("%d, ", value_type->val.array.num_elems);
	else
		gen_code("1, ");

	/* event flag expression if synced, else NOEVFLAG */
	if (cp->sync)
		gen_expr(context, mk_ef_type(), cp->sync->expr, 0);
	else
		gen_code("NOEVFLAG");

	/* syncq size (0=not queued) and index */
	if (!cp->syncq)
		gen_code(", 0, 0");
	else if (!cp->syncq->size)
		gen_code(", DEFAULT_QUEUE_SIZE, %d", cp->syncq->index);
	else
		gen_code(", %d, %d", cp->syncq->size, cp->syncq->index);
	gen_code(");\n");

	foreach (mp, cp->monitor)
	{
		indent(1);
		gen_code("seq_pvAddMonitor("NM_ENV", ");
		gen_expr(context, mk_pv_type(mk_void_type()), cp->expr, 0);
		gen_code(", %d);\n", (mp->scope->tag == D_PROG) ? UINT_MAX : mp->scope->extra.e_ss->index);
	}
}

static void gen_init(uint context, Type *expected, Node *tgt, Node *ixp, int level)
{
	uint i;
	Node *m, *cixp;

	if (ixp->tag == E_PRE && ixp->token.symbol == TOK_PV)
		return;
	switch (expected->tag)
	{
	case T_STRUCT:
		if (ixp->tag != E_INIT)
		{
			error_at_node(ixp, "expected aggregate initialiser\n");
			return;
		}
		cixp = ixp->init_elems;
		foreach(m, expected->val.structure.member_decls)
		{
			if (!cixp)
			{
				extra_warning_at_node(ixp, "missing elements in aggregate initialiser\n");
				return;
			}
			if (m->tag == D_DECL)
			{
				gen_init(context, m->extra.e_decl->type, mk_select_node(tgt, m->extra.e_decl->name), cixp, level);
				cixp = cixp ->next;
			}
			else
			{
				extra_warning_at_node(ixp, "not initialising foreign struct members\n");
			}
		}
		if (cixp)
		{
			warning_at_node(cixp, "discarding excess elements in aggregate initialiser");
		}
		break;
	case T_ARRAY:
		if (ixp->tag != E_INIT)
		{
			error_at_node(ixp, "expected aggregate initialiser\n");
			return;
		}
		cixp = ixp->init_elems;
		for(i=0; i<expected->val.array.num_elems; i++)
		{
			if (!cixp)
			{
				extra_warning_at_node(ixp, "missing elements in aggregate initialiser\n");
				return;
			}
			gen_init(context, expected->val.array.elem_type, mk_subscr_node(tgt, i), cixp, level);
			cixp = cixp ->next;
		}
		if (cixp)
		{
			warning_at_node(cixp, "discarding excess elements in aggregate initialiser");
		}
		break;
	default:
		gen_line_marker(ixp);
		indent(level); gen_code("{\n");
		indent(level);
		if (ctxTest(context,C_REENT))
			/* TODO check this, it doesn't feel right */
			gen_code("static ");
		gen_type(expected, NM_INITVAR, "");
		gen_code(" = ");
		gen_expr(context, strip_pv_type(expected), ixp, level);
		gen_code(";\n");
		indent(level); gen_code("memcpy(&");
		gen_expr(context, strip_pv_type(expected), tgt, level);
		gen_code(", &" NM_INITVAR ", sizeof(" NM_INITVAR "));\n");
		indent(level); gen_code("}\n");
	}
}

static void gen_var_init(Var *vp, uint context, int level)
{
	assert(vp);
	if (vp->type->tag != T_EVFLAG && vp->decl && vp->decl->decl_init)
	{
		assert(vp->type->tag != T_NONE);	/* syntax */
		gen_init(ctxSet(context,C_STATIC), vp->type, mk_var_node(vp), vp->decl->decl_init, level);
	}
}

/* Generate initialisers for variables of global lifetime */
static void gen_user_var_init(uint context, Node *prog, int level)
{
	Var	*vp;
	Node	*ssp;

	assert(prog->tag == D_PROG);
	/* global variables */
	foreach(vp, var_list_from_scope(prog)->first)
	{
		gen_var_init(vp, context, level);
	}
	/* state and state set variables */
	foreach (ssp, prog->prog_statesets)
	{
		Node *sp;

		assert(ssp->tag == D_SS);
		/* state set variables */
		foreach(vp, ssp->extra.e_ss->var_list->first)
		{
			gen_var_init(vp, context, level);
		}
		foreach (sp, ssp->ss_states)
		{
			assert(sp->tag == D_STATE);
			/* state variables */
			foreach (vp, sp->extra.e_state->var_list->first)
			{
				gen_var_init(vp, context, level);
			}
		}
	}
}

static void gen_prog_func(
	uint context,
	Node *prog,
	ChanList *channels,
	EvFlagList *event_flags,
	const char *doc,
	const char *name,
	void (*gen_body)(uint context, Node *prog, ChanList *channels, EvFlagList *event_flags)
)
{
	assert(prog->tag == D_PROG);
	gen_code("\n/* Program %s function */\n", doc);
	gen_code("static void %s(PROG_ID " NM_ENV ")\n{\n",
		name);
	gen_body(context, prog, channels, event_flags);
	gen_code("}\n");
}

static void gen_prog_entex_func(
	uint context,
	Node *prog,
	const char *doc,
	const char *name,
	void (*gen_body)(uint context, Node *)
)
{
	assert(prog->tag == D_PROG);
	gen_code("\n/* Program %s function */\n", doc);
	gen_code("static void %s(SS_ID " NM_ENV ")\n",
		name);
	gen_body(context, prog);
}

static void gen_prog_init_body(uint context, Node *prog, ChanList *channels, EvFlagList *event_flags)
{
	Chan	*cp;
	EvFlag	*ef;
	int	level = 1;

	assert(prog->tag == D_PROG);
	indent(level); gen_code("/* Create event flags */\n");
	foreach(ef, event_flags->first)
		gen_ef_init(context, ef);
	indent(level); gen_code("/* Create channels */\n");
	foreach (cp, channels->first)
		gen_channel_init(context, cp);
	indent(level); gen_code("/* Initialize variables */\n");
	gen_user_var_init(context, prog, level);
}

static void gen_prog_entry_body(uint context, Node *prog)
{
	assert(prog->tag == D_PROG);
	gen_entex_body(context, prog->prog_entry);
}

static void gen_prog_exit_body(uint context, Node *prog)
{
	assert(prog->tag == D_PROG);
	gen_entex_body(context, prog->prog_exit);
}

void gen_funcdef(uint context, Node *fp)
{
	if (fp->tag == D_FUNCDEF)
	{
		Var *vp = fp->funcdef_decl->extra.e_decl;

		gen_code("\n");
		gen_code("#define " NM_VAR " (*(struct " NM_VARS " *const *)" NM_ENV ")\n");
		gen_line_marker(vp->decl);
		gen_code("static ");
		gen_var_decl(vp);
		gen_code("\n");
		gen_block(ctxSet(context,C_FUNC),
			vp->type->val.function.return_type,
			fp->funcdef_block, 0);
		gen_code("#undef " NM_VAR "\n");
	}
}
