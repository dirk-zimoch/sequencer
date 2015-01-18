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
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <assert.h>

#include "types.h"
#include "sym_table.h"
#include "main.h"
#include "node.h"
#include "builtin.h"
#include "gen_code.h"
#include "var_types.h"
#include "snl.h"
#include "analysis.h"

static const int impossible = 0;

/* unique dummy pointer to distinguish struct
   names from other stuff in the symbol table */
void *structdefs = &structdefs;

static void analyse_definitions(Program *p);
static void analyse_option(Options *options, Node *defn);
static void analyse_state_option(StateOptions *options, Node *defn);
static void analyse_declaration(SymTable st, ChanList *chan_list, EvFlagList *evflag_list, Node *scope, Node *defn);
static void analyse_assign(SymTable st, ChanList *chan_list, Node *scope, Node *defn);
static void analyse_monitor(SymTable st, Node *scope, Node *defn);
static void analyse_sync(SymTable st, Node *scope, Node *defn);
static void analyse_syncq(SymTable st, SyncQList *syncq_list, Node *scope, Node *defn);
static Chan *new_channel(ChanList *chan_list, Type *type, Node *expr);
static EvFlag *new_event_flag(EvFlagList *evflag_list, Node *expr);
static SyncQ *new_sync_queue(SyncQList *syncq_list, uint size);
static int connect_variable(Node *ep, Node *scope, void *parg);
static void connect_variables(SymTable st, Node *scope);
static void connect_state_change_stmts(SymTable st, Node *scope);
static uint connect_states(SymTable st, Node *ss_list);
static void check_states_reachable_from_first(Node *ss);
static Var *find_var(SymTable st, char *name, Node *scope);

Program *analyse_program(Node *prog, Options *options)
{
	Program *p = new(Program);
	Node *ss;

	assert(prog);	/* precondition */
#ifdef DEBUG
	report("-------------------- Analysis --------------------\n");
#endif

	p->options = options;
	p->prog = prog;

	p->name			= prog->token.str;
	if (prog->prog_param)
		p->param	= prog->prog_param->token.str;
	else
		p->param	= "";

#ifdef DEBUG
	report("\n**************program=%s**************\n", p->name);
#endif

	p->sym_table = sym_table_create();

	p->chan_list = new(ChanList);
	p->evflag_list = new(EvFlagList);
	p->syncq_list = new(SyncQList);

#ifdef DEBUG
	report("created symbol table, channel list, and syncq list\n");
#endif

	register_builtins(p->sym_table, p->prog);

#ifdef DEBUG
	report("registered builtin symbols\n");
#endif

	analyse_definitions(p);
	p->num_ss = connect_states(p->sym_table, prog);
	connect_variables(p->sym_table, prog);
	connect_state_change_stmts(p->sym_table, prog);
	foreach (ss, prog->prog_statesets)
		check_states_reachable_from_first(ss);
	return p;
}

static void fixup_struct_refs(SymTable st, Type *t);

static void analyse_structdef(SymTable st, Node *defn)
{
	Node *d;
	assert(defn->tag == D_STRUCTDEF);
	if (!sym_table_insert(st, defn->token.str, structdefs, 
		mk_structure_type(defn->token.str, defn->structdef_members)))
	{
		warning_at_node(defn, "ignoring duplicate struct declaration\n");
	}
	foreach (d, defn->structdef_members)
	{
		if (d->tag == D_DECL)
		{
			fixup_struct_refs(st, d->extra.e_decl->type);
		}
	}
}

static void analyse_funcdef(Node *defn)
{
	Node *decl;
	Var *var;
	Node *p;

	assert(defn->tag == D_FUNCDEF);
	decl = defn->funcdef_decl;
	assert(decl->tag == D_DECL);
	var = decl->extra.e_decl;
#if 0
	decl->decl_init = defn->funcdef_block;
#endif
	if (var->type->tag != T_FUNCTION)
	{
		error_at_node(decl, "not a function type\n");
		return;
	}

	foreach (p, var->type->val.function.param_decls)
	{
		/* check parameter has a name */
		if (!p->extra.e_decl->name)
		{
			error_at_node(p, "function parameter must have a name\n");
		}
	}
}

static void analyse_defns(Node *defn_list, Node *scope, Program *p)
{
	Node *defn;

	assert(is_scope(scope));				/* precondition */
	foreach (defn, defn_list)
	{
		switch (defn->tag)
		{
		case D_OPTION:
			if (scope->tag == D_PROG)
			{
				analyse_option(p->options, defn);
			}
			else if (scope->tag == D_STATE)
			{
				analyse_state_option(&scope->extra.e_state->options, defn);
			}
			break;
		case D_DECL:
			analyse_declaration(p->sym_table, p->chan_list, p->evflag_list, scope, defn);
			break;
		case D_FUNCDEF:
			analyse_funcdef(defn);
			analyse_declaration(p->sym_table, p->chan_list, p->evflag_list, scope, defn->funcdef_decl);
			break;
		case D_ENUMDEF:
			/* TODO: analyse enum definition */
			break;
		case D_STRUCTDEF:
			analyse_structdef(p->sym_table, defn);
			break;
		case D_ASSIGN:
			analyse_assign(p->sym_table, p->chan_list, scope, defn);
			break;
		case D_MONITOR:
			analyse_monitor(p->sym_table, scope, defn);
			break;
		case D_SYNC:
			analyse_sync(p->sym_table, scope, defn);
			break;
		case D_SYNCQ:
			analyse_syncq(p->sym_table, p->syncq_list, scope, defn);
			break;
		case T_TEXT:
			break;
		default:
			assert(impossible);
		}
	}
}

static int analyse_scope(Node *scope, Node *parent_scope, void *parg)
{
	Program	*p = (Program *)parg;
	Node *defn_list;
	VarList *var_list;

	assert(scope);	/* precondition */

#ifdef DEBUG
	report("analyse_defn: scope=(%s:%s)\n",
		node_name(scope), scope->token.str);
#endif

	assert(is_scope(scope));				/* precondition */
	assert(!parent_scope || is_scope(parent_scope));	/* precondition */

	defn_list = defn_list_from_scope(scope);
	var_list = var_list_from_scope(scope);

	assert(var_list);					/* invariant, see node() */
	assert(!var_list->parent_scope);			/* invariant, see node() */

	var_list->parent_scope = parent_scope;

	analyse_defns(defn_list, scope, p);
	if (scope->tag == D_PROG)
	{
		analyse_defns(scope->prog_xdefns, scope, p);
	}

	return TRUE; /* always descend into children */
}

static void analyse_definitions(Program *p)
{
#ifdef DEBUG
	report("**begin** analyse definitions\n");
#endif

	traverse_syntax_tree(p->prog, scope_mask, ~has_sub_scope_mask, 0, analyse_scope, p);

#ifdef DEBUG
	report("**end** analyse definitions\n");
#endif
}

/* Options at the top-level. Note: latest given value for option wins. */
static void analyse_option(Options *options, Node *defn)
{
	char	*optname;
	uint	optval;

	assert(options);		/* precondition */
	assert(defn);			/* precondition */
	assert(defn->tag == D_OPTION);	/* precondition */

	optname = defn->token.str;
	optval = defn->extra.e_option;

	for (; *optname; optname++)
	{
		switch(*optname)
		{
		case 'a': options->async = optval; break;
		case 'c': options->conn = optval; break;
		case 'd': options->debug = optval; break;
		case 'e': options->newef = optval; break;
		case 'l': options->line = optval; break;
		case 'm': options->main = optval; break;
		case 'r': options->reent = optval; break;
		case 's': options->safe = optval; break;
		case 'w': options->warn = optval; break;
		case 'W': options->xwarn = optval; break;
		default: report_at_node(defn,
		  "warning: unknown option '%s'\n", optname);
		}
	}
	if (options->safe) {
		options->reent = TRUE;
	}
}

/* Options in state declarations. Note: latest given value for option wins. */
static void analyse_state_option(StateOptions *options, Node *defn)
{
	char	*optname;
	uint	optval;

	assert(options);		/* precondition */
	assert(defn);			/* precondition */
	assert(defn->tag == D_OPTION);	/* precondition */

	optname = defn->token.str;
	optval = defn->extra.e_option;

	for (; *optname; optname++)
	{
		switch(*optname)
		{
		case 't': options->do_reset_timers = optval; break;
		case 'e': options->no_entry_from_self = optval; break;
		case 'x': options->no_exit_to_self = optval; break;
		default: report_at_node(defn,
		  "warning: unknown state option '%s'\n", optname);
		}
	}
}

/* Traverse a type and replace foreign struct types with SNL struct
   type if one exists with the same name */
static void fixup_struct_refs(SymTable st, Type *t)
{
	Node *d;
	Type *r;

#ifdef DEBUG
	report("fixup_struct_refs():\n");
	dump_type(t, 1);
#endif
	switch (t->tag)
	{
	case T_NONE:
	case T_EVFLAG:
	case T_VOID:
	case T_PRIM:
		break;
	case T_FOREIGN:
		switch (t->val.foreign.tag)
		{
		case F_ENUM:
			break;
		case F_STRUCT:
			r = sym_table_lookup(st, t->val.foreign.name, structdefs);
			if (r)
			{
				assert(r->tag == T_STRUCT);
				*t = *r;
			}
			break;
		case F_UNION:
		case F_TYPENAME:
			break;
		}
		break;
	case T_POINTER:
		fixup_struct_refs(st, t->val.pointer.value_type);
		break;
	case T_ARRAY:
		fixup_struct_refs(st, t->val.array.elem_type);
		break;
	case T_FUNCTION:
		fixup_struct_refs(st, t->val.function.return_type);
		foreach (d, t->val.function.param_decls)
		{
			if (d->tag == D_DECL)
				fixup_struct_refs(st, d->extra.e_decl->type);
		}
		break;
#if 0
	case T_CONST:
		fixup_struct_refs(st, t->val.constant.value_type);
		break;
#endif
	case T_STRUCT:
		if (!t->val.structure.mark)
		{
			t->val.structure.mark = TRUE;
			foreach (d, t->val.structure.member_decls)
			{
				if (d->tag == D_DECL)
				{
#ifdef DEBUG
					report("struct member %s.%s:\n", t->val.structure.name, d->extra.e_decl->name);
#endif
					fixup_struct_refs(st, d->extra.e_decl->type);
				}
			}
			t->val.structure.mark = FALSE;
		}
		break;
	case T_PV:
		fixup_struct_refs(st, t->val.pv.value_type);
		break;
	}
}

void dump_channel_node(ChanNode *node, int level)
{
	uint n, num_nodes;

	for (n = 0; n < level; n++) report("  ");
	if (!node)
	{
		report("NOTHING\n");
		return;
	}
	switch (node->type->tag)
	{
	case T_PV:
		assert(node->val.chan);
		report("CHANNEL '%s'\n", node->val.chan->name);
		return;
	case T_EVFLAG:
		assert(node->val.evflag);
		report("EVFLAG\n");
		return;
	case T_ARRAY:
		report("ARRAY\n");
		num_nodes = node->type->val.array.num_elems;
		break;
	case T_STRUCT:
		report("STRUCT\n");
		num_nodes = node->type->val.structure.num_members;
		break;
	default:
		assert(impossible);
	}
	assert(node->val.nodes);
	for (n = 0; n < num_nodes; n++)
	{
		dump_channel_node(node->val.nodes[n], level+1);
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

static Node *mk_string_node(Node *other, char *s)
{
	Token k = other->token;
	k.symbol = TOK_STRCON;
	k.str = s;
	return node(E_STRING, k);
}

static Node *mk_subscr_node(Node *operand, uint n)
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

static Node *mk_select_node(Node *operand, char *name)
{
	Token k = operand->token;

	k.symbol = TOK_PERIOD;
	k.str = ".";
	return node(E_SELECT, k, operand, mk_member_node(operand, name));
}

static Node *mk_var_node(Var *vp)
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

/* Pretty printer for the kind of simple expressions that appear in assign
   (or monitor, etc) clauses and pv initialisers. For error messages. */
void report_expr(Node *xp)
{
	switch(xp->tag)
	{
	case E_VAR:
	case E_MEMBER:
	case E_CONST:
		report("%s", xp->token.str);
		return;
	case E_SUBSCR:
		report_expr(xp->subscr_operand);
		report("["); report_expr(xp->subscr_index); report("]");
		return;
	case E_SELECT:
		report_expr(xp->select_left);
		report("%s", xp->token.str);
		report_expr(xp->select_right);
		return;
	case E_STRING:
		if (xp->token.str)
			report("\"%s\"", xp->token.str);
		return;
	case E_INIT:
		report("{");
		foreach (xp, xp->init_elems)
		{
			report_expr(xp);
			if (xp->next) report(",");
		}
		report("}");
		return;
	default:
		report("internal compiler error (xp->tag==%s)\n", node_info[xp->tag].name);
		assert(impossible);
	}
}

/* Build the channel tree for a part of a variable. The part is denoted by
   an expression 'vxp' that consists only of variables, subscripts (with
   constant index), and member selection. */
ChanNode *build_channel_tree(
	ChanList *chan_list,		/* program wide channel list */
	EvFlagList *evflag_list,	/* program wide event flag list */
	ChanNode *cn,			/* an already existing channel node */
	Type *type,			/* type of this part of the variable */
	Node *vxp,			/* which part of the variable */
	Node *ixp			/* pv init expression */
)
{
	Node *member;
	Node *sub_ixp = 0;
	uint n;
	char *pv_name;

	assert(vxp);

#ifdef DEBUG
	report("build_channel_tree:\n  type=\n");
	dump_type(type, 2);
	report("  vxp=\n");
	dump_expr(vxp, 2);
	report("  ixp=\n");
	dump_expr(ixp, 2);
#endif

	switch (type->tag)
	{
	case T_PV:
		if (ixp)
		{
			if (ixp->tag == E_STRING)
				pv_name = ixp->token.str;
			else
			{
				error_at_node(ixp, "unexpected '%s' in pv init expression '",
					ixp->token.str);
				report_expr(ixp);
				report("', expecting string (pv name)\n");
				pv_name = 0;
			}
		}
		else
			pv_name = 0;
		if (cn && cn->val.chan)
		{
			if (cn->val.chan->name && pv_name)
			{
				warning_at_node(vxp, "channel name changed from '%s' to '%s'\n",
					cn->val.chan->name, pv_name);
			}
		}
		if (!cn)
		{
			cn = new(ChanNode);
			cn->type = type;
		}
		assert(cn->type == type);
		if (!cn->val.chan)
			cn->val.chan = new_channel(chan_list, type, vxp);
		if (pv_name)
			cn->val.chan->name = pv_name;
		return cn;
	case T_EVFLAG:
		assert(!ixp);
		if (!cn)
		{
			cn = new(ChanNode);
			cn->type = type;
		}
		assert(cn->type == type);
		if (!cn->val.chan)
			cn->val.evflag = new_event_flag(evflag_list, vxp);
		return cn;
	case T_ARRAY:
		if (ixp)
		{
			if (ixp->tag == E_INIT)
			{
				sub_ixp = ixp->init_elems;
			}
			else
			{
				error_at_node(ixp, "expecting aggregate initialiser for array\n");
				ixp = 0;
			}
		}
		assert(!ixp || ixp->tag == E_INIT);
		for (n = 0; n < type->val.array.num_elems; n++)
		{
			Node *sup_vxp = mk_subscr_node(vxp, n);
			ChanNode *sub_cn = build_channel_tree(chan_list, evflag_list,
				cn ? cn->val.nodes[n] : 0, type->val.array.elem_type, sup_vxp, sub_ixp);
			if (sub_cn)
			{
				if (!cn)
				{
					cn = new(ChanNode);
					cn->type = type;
					cn->val.nodes = newArray(ChanNode*, type->val.array.num_elems);
				}
				cn->val.nodes[n] = sub_cn;
			}
			if (sub_ixp)
				sub_ixp = sub_ixp->next;
			else if (ixp)
				extra_warning_at_node(ixp, "defaulting missing initialisers to empty\n");
		}
		if (sub_ixp)
			warning_at_node(sub_ixp, "discarding excess PV names in aggregate pv initialiser");
		return cn;
	case T_STRUCT:
		if (ixp)
		{
			if (ixp->tag == E_INIT)
				sub_ixp = ixp->init_elems;
			else
			{
				error_at_node(ixp, "expecting aggregate initialiser for struct\n");
				ixp = 0;
			}
		}
		assert(!ixp || ixp->tag == E_INIT);
		n = 0;
		foreach (member, type->val.structure.member_decls)
		{
			if (member->tag == D_DECL)
			{
				Node *sup_vxp = mk_select_node(vxp, member->extra.e_decl->name);
				ChanNode *sub_cn = build_channel_tree(chan_list, evflag_list,
					cn ? cn->val.nodes[n] : 0, member->extra.e_decl->type, sup_vxp, sub_ixp);
				if (sub_cn)
				{
					if (!cn)
					{
						cn = new(ChanNode);
						cn->type = type;
						cn->val.nodes = newArray(ChanNode*, type->val.structure.num_members);
					}
					cn->val.nodes[n] = sub_cn;
				}
				if (sub_ixp)
					sub_ixp = sub_ixp->next;
				else if (ixp)
				{
					extra_warning_at_node(ixp,
						"defaulting missing pv struct initialisers "
						"to 'not assigned'\n");
					ixp = 0;
				}
				n++;
			}
			/* note: we completely skip over non-DECL members */
		}
		if (sub_ixp)
			warning_at_node(sub_ixp, "discarding excess pv initialisers in aggregate\n");
		return cn;
	default:
		if (!ixp)
			return 0;
		warning_at_node(ixp, "ignoring pv initialiser at non-pv type\n");
		return 0;
	}
}

static void analyse_declaration(
	SymTable st,
	ChanList *chan_list,
	EvFlagList *evflag_list,
	Node *scope,
	Node *defn)
{
	Var *vp;
	VarList *var_list;
	static uint seen_foreign = FALSE;
	ChanNode *chan = 0;
	Node *pv_init = 0;

	assert(scope);			/* precondition */
	assert(defn);			/* precondition */
	assert(defn->tag == D_DECL);	/* precondition */

	vp = defn->extra.e_decl;

	assert(vp);			/* invariant */
#ifdef DEBUG
	report("declaration: %s\n", vp->name);
#endif

	/* enforce some restrictions */

	if (vp->type->tag == T_NONE && !seen_foreign)
	{
		warning_at_node(defn,
			"foreign declarations are deprecated\n");
		seen_foreign = TRUE;
	}
	if (vp->type->tag == T_NONE && scope->tag != D_PROG)
		error_at_node(defn,
			"foreign objects can only be declared at the top-level\n");

	if (vp->type->tag == T_FUNCTION && scope->tag != D_PROG)
		error_at_node(defn,
			"functions can only be declared at the top-level\n");

	if (vp->type->tag == T_PV && vp->type->val.pv.value_type->tag == T_VOID)
	{
		if (scope->tag != D_FUNCDEF)
			error_at_node(defn,
				"void pv can only be declared as function parameter\n");
	}

#ifdef DEBUG
	report("name=%s, before fixup:\n", vp->name);
	dump_type(vp->type, 0);
#endif
	fixup_struct_refs(st, vp->type);
#ifdef DEBUG
	report("name=%s, after fixup:\n", vp->name);
	dump_type(vp->type, 0);
#endif

	var_list = var_list_from_scope(scope);

	assert(vp->name);

	if (!sym_table_insert(st, vp->name, var_list, vp))
	{
		Var *vp2 = (Var *)sym_table_lookup(st, vp->name, var_list);

		if (vp2->decl)
			error_at_node(defn,
			 "'%s' already declared at %s:%d\n",
			 vp->name, vp2->decl->token.file, vp2->decl->token.line);
		else
			error_at_node(defn,
			 "'%s' already (implicitly) declared\n", vp->name);
	}
	else
	{
		add_var_to_scope(vp, scope);
	}

	if (defn->decl_init && defn->decl_init->tag == E_PRE && defn->decl_init->token.symbol == TOK_PV)
	{
		if (scope->tag == D_PROG || scope->tag == D_SS || scope->tag == D_STATE)
			pv_init = defn->decl_init->pre_operand;
		else
			error_at_node(defn->decl_init, "pv initialiser not allowed here\n");
		defn->decl_init = 0;
	}

	assert(!vp->chan);
	if (scope->tag != D_FUNCDEF)
	{
		chan = build_channel_tree(chan_list, evflag_list, 0, vp->type, mk_var_node(vp), pv_init);
		assert(!vp->chan || vp->chan->type==vp->type);
#ifdef DEBUG
		report("analyse_declaration(): channel_node=\n");
		dump_channel_node(chan, 1);
#endif
	}

	if (scope->tag == D_PROG || scope->tag == D_SS || scope->tag == D_STATE || scope->tag == D_FUNCDEF)
		vp->chan = chan;
	else if (chan)
		error_at_node(defn, "declaring channels or event flags is not allowed here\n");
}

uint eval_subscript(Node *expr, uint num_elems, uint *pindex)
{
	assert(expr->tag == E_CONST);
	if (expr->extra.e_const)
	{
		*pindex = *expr->extra.e_const;
		return TRUE;
	}
	if (!strtoui(expr->token.str, num_elems, pindex))
	{
		error_at_node(expr, "subscript '[%s]' out of range\n", expr->token.str);
		return FALSE;
	}
	expr->extra.e_const = new(uint);
	*expr->extra.e_const = *pindex;
	return TRUE;
}

static ChanNode *traverse_var_expr(SymTable st, Node *scope, Node *vxp, const char *what, uint same_scope)
{
	Type *t;
	Var *vp;
	ChanNode *chan_node;
	uint index;
	Node *member;
	char *var_name;

	switch (vxp->tag)
	{
	case E_VAR:
		var_name = vxp->token.str;
		vp = find_var(st, var_name, scope);
		if (!vp)
		{
			error_at_node(vxp,
				"cannot %s variable '%s': not declared\n", what, var_name);
			return 0;
		}
		vxp->extra.e_var = vp;
		if (same_scope && vp->scope != scope)
		{
			error_at_node(vxp, "cannot %s variable '%s': "
				"%s must be in the same scope as declaration\n", what, var_name, what);
			return 0;
		}
		if (!vp->chan)
		{
			error_at_node(vxp, "cannot %s variable '%s': type does not contain pv marker\n",
				what, vp->name);
		}
#ifdef DEBUG
		report("traverse_var_expr: E_VAR\n");
		dump_channel_node(vp->chan, 1);
#endif
		return vp->chan;
	case E_SUBSCR:
		chan_node = traverse_var_expr(st, scope, vxp->subscr_operand, what, same_scope);
		if (!chan_node)
			return 0;
#ifdef DEBUG
		report("traverse_var_expr: E_SUBSCR\n");
		dump_channel_node(chan_node, 1);
#endif
		t = chan_node->type;
		if (t->tag == T_PV)
		{
			error_at_node(vxp, "type error: cannot %s an array element of a single channel\n", what);
			return 0;
		}
		if (t->tag != T_ARRAY)
		{
			error_at_node(vxp, "type error in subscript: expression '");
			report_expr(vxp->subscr_operand);
			report("' is not an array\n");
			dump_type(t, 0);
			return 0;
		}
		if (!eval_subscript(vxp->subscr_index, t->val.array.num_elems, &index))
			return 0;
		return chan_node->val.nodes[index];
	case E_SELECT:
		chan_node = traverse_var_expr(st, scope, vxp->select_left, what, same_scope);
		if (!chan_node)
			return 0;
		t = chan_node->type;
#ifdef DEBUG
		report("traverse_var_expr: E_SELECT\n");
		dump_channel_node(chan_node, 1);
#endif
		assert(t->tag != T_PV);
		if (t->tag != T_STRUCT)
		{
			error_at_node(vxp, "type error in member selection: expression '");
			report_expr(vxp->select_left);
			report("' is not a struct\n");
			return 0;
		}
		assert(vxp->select_right->tag == E_MEMBER);
		index = 0;
		foreach (member, t->val.structure.member_decls)
		{
			if (strcmp(member->token.str, vxp->select_right->token.str) == 0)
				break;
			index++;
		}
		if (!member)
		{
			error_at_node(vxp, "'struct %s' has no member '%s'\n",
				t->val.structure.name,
				vxp->select_right->token.str);
			return 0;
		}
		return chan_node->val.nodes[index];
	default:
		assert(impossible);
	}
}

/* Find the variable node in a var_expr. Excerpt from the grammar:
var_expr(r) ::= variable(v).					{ r = node(E_VAR, v); }
var_expr(r) ::= var_expr(x) LBRACKET(t) INTCON(y) RBRACKET.	{ r = node(E_SUBSCR, t, x, node(E_CONST, y)); }
var_expr(r) ::= var_expr(x) PERIOD(t) member(y).		{ r = node(E_SELECT, t, x, y); }
*/
static Node *find_var_node(Node *xp)
{
	switch (xp->tag)
	{
	case E_VAR:
		return xp;
	case E_SUBSCR:
		return find_var_node(xp->subscr_operand);
	case E_SELECT:
		return find_var_node(xp->select_left);
	default:
		assert(impossible);
	}
}

static void assign_error(Node *vxp, Node *ixp, const char *reason)
{
	error_at_node(ixp, "cannot assign expression: '");
	report_expr(vxp);
	report("' to '");
	report_expr(ixp);
	report("': %s\n", reason);
}

static void analyse_assign(SymTable st, ChanList *chan_list, Node *scope, Node *defn)
{
	Node *var_node, *vxp, *ixp;
	Var *vp;
	ChanNode *chan_node;

	assert(chan_list);		/* precondition */
	assert(scope);			/* precondition */
	assert(defn);			/* precondition */
	assert(defn->tag == D_ASSIGN);	/* precondition */

	vxp = defn->assign_expr;
	if (!defn->assign_pvs)
		defn->assign_pvs = mk_string_node(defn, "");
	ixp = defn->assign_pvs;

	var_node = find_var_node(vxp);
	assert(var_node);		/* syntax + postcondition */
	connect_variable(var_node, scope, &st);
	vp = var_node->extra.e_var;
	assert(vp);			/* invariant */

#ifdef DEBUG
	report("assign ");
        report_expr(vxp);
	report(" to ");
        report_expr(ixp);
	report(";\n");
#endif

	if (vp->scope != scope)
	{
		assign_error(vxp, ixp, "assign must be in the same scope as declaration");
	}
	if (!vp->chan)
	{
		if (type_is_valid_pv_child(vp->type) ||
			(vp->type->tag == T_ARRAY && type_is_valid_pv_child(vp->type->val.array.elem_type)))
		{
			/* compatibility work-around for old programs; we only support the two
			   special cases which were allowed in earlier versions */
			extra_warning_at_node(defn, "assign adds missing pv qualifier to type of expression [deprecated]\n");
			if (ixp->tag == E_INIT)
			{
				if (vp->type->tag != T_ARRAY)
				{
					assign_error(vxp, ixp, "variable is not an array");
					return;
				}
				if (!type_is_valid_pv_child(vp->type->val.array.elem_type))
				{
					assign_error(vxp, ixp, "array element type is not a valid channel type");
					return;
				}
				vp->type->val.array.elem_type = mk_pv_type(vp->type->val.array.elem_type);
			}
			else
			{
				assert(ixp->tag == E_STRING);	/* syntax */
				if (!type_is_valid_pv_child(vp->type))
				{
					assign_error(vxp, ixp, "variable type is not a valid channel type");
					return;
				}
				vp->type = mk_pv_type(vp->type);
			}
			assert(!vp->chan);
			vp->chan = build_channel_tree(chan_list, 0, 0, vp->type, var_node, 0);
		}
		else
		{
			assign_error(vxp, ixp, "the expression's type contains no pv qualifier\n");
			return;
		}
	}
	assert(vp->chan);

#ifdef DEBUG
        report("analyse_assign: before traverse_var_expr\n");
	assert(vp->chan->type == vp->type);
	dump_type(vp->type, 1);
	dump_channel_node(vp->chan, 1);
#endif

	chan_node = traverse_var_expr(st, scope, vxp, "assign", TRUE);
	if (!chan_node)
		return;
	*chan_node = *build_channel_tree(chan_list, 0, chan_node, chan_node->type, vxp, ixp);
	assert(vp->chan->type == vp->type);
}

uint traverse_channel_tree(
	ChanNode	*node,		/* start node */
	channel_action	*chan_iter,	/* function to call for each channel */
	evflag_action	*evflag_iter,	/* function to call for each event flag */
	void		*env		/* environment, passed to iteratees */
)
{
	uint num_nodes, n;

	if (!node)
		return FALSE;
	switch (node->type->tag)
	{
	case T_PV:
		assert(node->val.chan);
		if (chan_iter)
			return chan_iter(node->val.chan, env);
		else
			return FALSE;
	case T_EVFLAG:
		assert(node->val.evflag);
		if (evflag_iter)
			return evflag_iter(node->val.evflag, env);
		else
			return FALSE;
	case T_ARRAY:
		num_nodes = node->type->val.array.num_elems;
		break;
	case T_STRUCT:
		num_nodes = node->type->val.structure.num_members;
		break;
	default:
		return FALSE;
	}
	assert(node->val.nodes);
	for (n = 0; n < num_nodes; n++)
	{
		uint res = traverse_channel_tree(node->val.nodes[n], chan_iter, evflag_iter, env);
		if (res)
			return res;
	}
	return FALSE;
}

static uint monitor_iteratee(Chan *chan, void *env)
{
	Node *scope = (Node *)env;
	Monitor *mon;

	foreach (mon, chan->monitor)
		if (mon->scope == scope || mon->scope->tag == D_PROG)
			return TRUE;
	mon = new(Monitor);
	mon->scope = scope;
	mon->next = chan->monitor;
	chan->monitor = mon;
	return FALSE;
}

static void analyse_monitor(SymTable st, Node *scope, Node *defn)
{
	ChanNode *chan_node;

	assert(scope);
	assert(defn);
	assert(defn->tag == D_MONITOR);

#ifdef DEBUG
	report("monitor ");
        report_expr(defn->monitor_expr);
	report(";\n");
#endif

	chan_node = traverse_var_expr(st, scope, defn->monitor_expr, "monitor", FALSE);
	if (!chan_node)
		return;
	if (traverse_channel_tree(chan_node, monitor_iteratee, 0, scope))
	{
		warning_at_node(defn, "expression or parts of it are already monitored\n");
	}
}

static uint sync_iteratee(Chan *chan, void *env)
{
	Var *ef = (Var *)env;
	if (chan->sync)
		return TRUE;
	chan->sync = ef;
	return FALSE;
}

static void analyse_sync(SymTable st, Node *scope, Node *defn)
{
	char		*ef_name;
	Var		*evp;
	ChanNode	*chan_node;

	assert(scope);
	assert(defn);
	assert(defn->tag == D_SYNC);

	assert(defn->sync_evflag);
	ef_name = defn->sync_evflag->token.str;
	assert(ef_name);

	evp = find_var(st, ef_name, scope);
	if (!evp)
	{
		error_at_node(defn, "event flag '%s' not declared\n", ef_name);
		return;
	}
	if (evp->type->tag != T_EVFLAG)
	{
		error_at_node(defn, "variable '%s' is not an event flag\n", ef_name);
		return;
	}

#ifdef DEBUG
	report("sync ");
        report_expr(defn->sync_expr);
	report(";\n");
#endif

	chan_node = traverse_var_expr(st, scope, defn->sync_expr, "sync", TRUE);
	if (!chan_node)
		return;
	if (traverse_channel_tree(chan_node, sync_iteratee, 0, evp))
	{
		error_at_node(defn, "expression or parts of it are already synced\n");
	}
}

static uint syncq_iteratee(Chan *chan, void *env)
{
	SyncQ *qp = (SyncQ *)env;

	if (chan->syncq)
		return TRUE;
	chan->syncq = qp;
	return FALSE;
}

static void analyse_syncq(SymTable st, SyncQList *syncq_list, Node *scope, Node *defn)
{
	Var		*evp = 0;
	SyncQ		*qp;
	uint		n_size = 0;
	ChanNode	*chan_node;

	assert(scope);
	assert(defn);
	assert(defn->tag == D_SYNCQ);

	if (!defn->syncq_size)
	{
		warning_at_node(defn, "leaving out the queue size is deprecated"
			", queue size defaults to 100 elements\n");
	}
	else if (!strtoui(defn->syncq_size->token.str, UINT_MAX, &n_size) || n_size < 1)
	{
		error_at_node(defn->syncq_size, "queue size '%s' out of range\n",
			defn->syncq_size->token.str);
		return;
	}
	if (defn->syncq_evflag)
	{
		char *ef_name = defn->syncq_evflag->token.str;
		assert(ef_name);

		evp = find_var(st, ef_name, scope);
		if (!evp)
		{
			error_at_node(defn, "event flag '%s' not declared\n", ef_name);
			return;
		}
		if (evp->type->tag != T_EVFLAG)
		{
			error_at_node(defn, "variable '%s' is not an event flag\n", ef_name);
			return;
		}
	}
	qp = new_sync_queue(syncq_list, n_size);

#ifdef DEBUG
	report("syncq ");
        report_expr(defn->syncq_expr);
	report(";\n");
#endif

	chan_node = traverse_var_expr(st, scope, defn->syncq_expr, "syncq", TRUE);
	if (!chan_node)
		return;

	if (evp)
		if (traverse_channel_tree(chan_node, sync_iteratee, 0, evp))
			error_at_node(defn, "expression or parts of it are already synced\n");

	if (traverse_channel_tree(chan_node, syncq_iteratee, 0, qp))
		error_at_node(defn, "expression or parts of it are already syncqed\n");
}

/* Allocate a channel structure for this variable, add it to the channel list,
   and initialise members var, and count. Also increase channel
   count in the list. */
static Chan *new_channel(ChanList *chan_list, Type *type, Node *expr)
{
	Chan *cp = new(Chan);

	cp->type = type;
	cp->expr = expr;
	cp->index = chan_list->num_elems++;

	/* add to chan_list */
	if (!chan_list->first)
		chan_list->first = cp;
	else
		chan_list->last->next = cp;
	chan_list->last = cp;
	cp->next = 0;
	return cp;
}

static EvFlag *new_event_flag(EvFlagList *evflag_list, Node *expr)
{
	EvFlag *ef = new(EvFlag);

	ef->expr = expr;
	ef->index = evflag_list->num_elems++;

	/* add to evflag_list */
	if (!evflag_list->first)
		evflag_list->first = ef;
	else
		evflag_list->last->next = ef;
	evflag_list->last = ef;
	ef->next = 0;
	return ef;
}

/* Allocate a sync queue structure, add it to the sync queue list,
   and initialise members index, var, and size. Also increase sync queue
   count in the list. */
static SyncQ *new_sync_queue(SyncQList *syncq_list, uint size)
{
	SyncQ *qp = new(SyncQ);

	qp->index = syncq_list->num_elems++;
	qp->size = size;

	/* add new syncqnel to syncq_list */
	if (!syncq_list->first)
		syncq_list->first = qp;
	else
		syncq_list->last->next = qp;
	syncq_list->last = qp;
	qp->next = 0;

	return qp;
}

/* Add a variable to a scope (append to the end of the var_list) */
void add_var_to_scope(Var *vp, Node *scope)
{
	VarList	*var_list = var_list_from_scope(scope);

	if (!var_list->first)
		var_list->first = vp;
	else
		var_list->last->next = vp;
	var_list->last = vp;
	vp->next = 0;

	vp->scope = scope;
}

/* Find a variable by name, given a scope; first searches the given
   scope, then the parent scope, and so on. Returns a pointer to the
   var struct or 0 if the variable is not found. */
Var *find_var(SymTable st, char *name, Node *scope)
{
	VarList *var_list = var_list_from_scope(scope);
	Var	*vp;

#ifdef DEBUG
	report("searching %s in %s:%s, ", name, scope->token.str,
		node_name(scope));
#endif
	vp = (Var *)sym_table_lookup(st, name, var_list);
	if (vp)
	{
#ifdef DEBUG
		report("found\n");
#endif
		return vp;
	}
	else if (!var_list->parent_scope)
	{
#ifdef DEBUG
		report("not found\n");
#endif
		return 0;
	}
	else
		return find_var(st, name, var_list->parent_scope);
}

/* Connect a variable reference (tag E_VAR) to the Var structure.
   If there is no such structure, e.g. because the variable has not been
   declared, then allocate one, assign type T_NONE, and assign the
   top-level scope for the variable. */
static int connect_variable(Node *ep, Node *scope, void *parg)
{
	SymTable	st = *(SymTable *)parg;
	Var		*vp;

	assert(ep);
	assert(ep->tag == E_VAR);
	assert(scope);

	if (ep->extra.e_var)
		return FALSE;	/* already connected */

#ifdef DEBUG
	report("connect_variable: %s, line %d\n", ep->token.str, ep->token.line);
#endif

	vp = find_var(st, ep->token.str, scope);

#ifdef DEBUG
	if (vp)
	{
		report_at_node(ep, "var %s found in scope (%s:%s)\n", ep->token.str,
			node_name(vp->scope),
			vp->scope->token.str);
	}
	else
		report_at_node(ep, "var %s not found\n", ep->token.str);
#endif
	if (!vp)
	{
		VarList *var_list = var_list_from_scope(scope);
		Node *builtin_const = lookup_builtin_const(st, ep->token.str);
		if (builtin_const)
		{
			Node *next = ep->next;
			Node *last = ep->last;
			*ep = *builtin_const;
			/* restore next and last members */
			ep->next = next;
			ep->last = last;
			return FALSE;
		}

		extra_warning_at_node(ep, "treating undeclared object '%s' as foreign\n",
			ep->token.str);
		/* create a variable without declaration and with no type */
		vp = new(Var);
		vp->name = ep->token.str;
		vp->type = mk_no_type();	/* undeclared type */
		/* add this variable to the top-level scope, NOT the current scope */
		while (var_list->parent_scope) {
			scope = var_list->parent_scope;
			var_list = var_list_from_scope(scope);
		}
		sym_table_insert(st, ep->token.str, var_list, vp);
		add_var_to_scope(vp, scope);
	}
	ep->extra.e_var = vp;		/* make connection */
	return FALSE;			/* there are no children anyway */
}

static void connect_variables(SymTable st, Node *scope)
{
#ifdef DEBUG
	report("**begin** connect_variables\n");
#endif
	traverse_syntax_tree(scope, bit(E_VAR), 0, 0, connect_variable, &st);
#ifdef DEBUG
	report("**end** connect_variables\n");
#endif
}

/* Check for duplicate state set and state names and resolve transitions between states */
static uint connect_states(SymTable st, Node *prog)
{
	Node	*ssp;
	uint	num_ss = 0;

	foreach (ssp, prog->prog_statesets)
	{
		Node *sp;
		uint num_states = 0;

#ifdef DEBUG
		report("connect_states: ss = %s\n", ssp->token.str);
#endif
		if (!sym_table_insert(st, ssp->token.str, prog, ssp))
		{
			Node *ssp2 = (Node *)sym_table_lookup(st, ssp->token.str, prog);
			error_at_node(ssp,
				"a state set with name '%s' was already "
				"declared at line %d\n", ssp->token.str, ssp2->token.line);
		}
		foreach (sp, ssp->ss_states)
		{
			if (!sym_table_insert(st, sp->token.str, ssp, sp))
			{
				Node *sp2 = (Node *)sym_table_lookup(st, sp->token.str, ssp);
				error_at_node(sp,
					"a state with name '%s' in state set '%s' "
					"was already declared at line %d\n",
					sp->token.str, ssp->token.str, sp2->token.line);
			}
			assert(sp->extra.e_state);
#ifdef DEBUG
			report("connect_states: ss = %s, state = %s, index = %d\n",
				ssp->token.str, sp->token.str, num_states);
#endif
			sp->extra.e_state->index = num_states++;
		}
		ssp->extra.e_ss->num_states = num_states;
#ifdef DEBUG
		report("connect_states: ss = %s, num_states = %d\n", ssp->token.str, num_states);
#endif
		foreach (sp, ssp->ss_states)
		{
			Node *tp;

			foreach (tp, sp->state_whens)
			{
				Node *next_sp = 0;

				if (tp->token.str)
				{
					next_sp = (Node *)sym_table_lookup(st, tp->token.str, ssp);
					if (!next_sp)
					{
						error_at_node(tp,
							"a state with name '%s' does not "
							"exist in state set '%s'\n",
					 		tp->token.str, ssp->token.str);
					}
				}
				tp->extra.e_when->next_state = next_sp;
				assert(!next_sp || strcmp(tp->token.str,next_sp->token.str) == 0);
#ifdef DEBUG
				report("connect_states: ss = %s, state = %s, when(...){...} state (%s,%d)\n",
					ssp->token.str, sp->token.str, tp->token.str,
					next_sp ? next_sp->extra.e_state->index : -1);
#endif
			}
		}
		ssp->extra.e_ss->index = num_ss++;
	}
	return num_ss;
}

typedef struct {
	SymTable	st;
	Node		*ssp;
	int		in_when;
} connect_state_change_arg;

static int iter_connect_state_change_stmts(Node *ep, Node *scope, void *parg)
{
	connect_state_change_arg *pcsc_arg = (connect_state_change_arg *)parg;

	assert(pcsc_arg);
	assert(ep);
	if (ep->tag == D_SS)
	{
		pcsc_arg->ssp = ep;
		return TRUE;
	}
	else if (ep->tag == D_ENTEX)
	{
		/* to flag erroneous state change statements, see below */
		pcsc_arg->in_when = FALSE;
		return TRUE;
	}
	else if (ep->tag == D_WHEN)
	{
		pcsc_arg->in_when = TRUE;
		return TRUE;
	}
	else
	{
		assert(ep->tag == S_CHANGE);
		if (!pcsc_arg->ssp || !pcsc_arg->in_when)
		{
			error_at_node(ep, "state change statement not allowed here\n");
		}
		else
		{
			Node *sp = (Node *)sym_table_lookup(
				pcsc_arg->st, ep->token.str, pcsc_arg->ssp);
			if (!sp)
			{
				error_at_node(ep,
					"a state with name '%s' does not "
					"exist in state set '%s'\n",
				 	ep->token.str, pcsc_arg->ssp->token.str);
				return FALSE;
			}
			ep->extra.e_change = sp;
		}
		return FALSE;
	}
}

static void connect_state_change_stmts(SymTable st, Node *scope)
{
	connect_state_change_arg csc_arg;

	csc_arg.st = st;
	csc_arg.ssp = 0;
	csc_arg.in_when = FALSE;
	traverse_syntax_tree(scope,
		bit(S_CHANGE)|bit(D_SS)|bit(D_ENTEX)|bit(D_WHEN),
		expr_mask, 0, iter_connect_state_change_stmts, &csc_arg);
}

static void mark_states_reachable_from(Node *sp);

static int iter_mark_states_reachable(Node *ep, Node *scope, void *parg)
{
	Node *target_state = 0;

	switch (ep->tag ) {
	case S_CHANGE:
		target_state = ep->extra.e_change;
		break;
	case D_WHEN:
		target_state = ep->extra.e_when->next_state;
		break;
	default:
		assert(impossible);
	}
	if (target_state && !target_state->extra.e_state->is_target)
	{
		target_state->extra.e_state->is_target = 1;
		mark_states_reachable_from(target_state);
	}
	return (ep->tag == D_WHEN);
}

static void mark_states_reachable_from(Node *sp)
{
	assert(sp);
	assert(sp->tag == D_STATE);

	traverse_syntax_tree(
		sp,				/* start node */
		bit(S_CHANGE)|bit(D_WHEN),	/* when to call iteratee */
		expr_mask,			/* when to stop descending */
		sp,				/* current scope, 0 at top-level */
		iter_mark_states_reachable,	/* function to call */
		0				/* argument to pass to function */
	);
}

static void check_states_reachable_from_first(Node *ssp)
{
	Node *sp;

	assert(ssp);
	assert(ssp->tag == D_SS);

	sp = ssp->ss_states;
	assert(sp);
	assert(sp->tag == D_STATE);
	assert(sp->extra.e_state->index == 0);

	sp->extra.e_state->is_target = 1;
	mark_states_reachable_from(sp);

	foreach (sp, ssp->ss_states)
	{
		if (!sp->extra.e_state->is_target)
		{
			warning_at_node(sp, "state '%s' in state set '%s' cannot "
				"be reached from start state\n",
				sp->token.str, ssp->token.str);
		}
	}
}
