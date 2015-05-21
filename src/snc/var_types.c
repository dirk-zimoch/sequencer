/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#include <limits.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define prim_types_GLOBAL
#define var_types_GLOBAL

#include "main.h"
#include "snl.h"
#include "gen_code.h"   /* implicit parameter names */
#include "node.h"
#include "var_types.h"

/* #define DEBUG */

static const int impossible = FALSE;

static Node *add_implicit_parameters(Node *fun_decl, Node *param_decls)
{
    Node *p1;
    Token t;

    /* implicit parameter: "SS_ID " NM_ENV */
    t.symbol = TOK_NAME;
    t.str = NM_ENV;
    t.line = fun_decl->token.line;
    t.file = fun_decl->token.file;
    p1 = mk_decl(node(E_VAR, t), mk_foreign_type(F_TYPENAME, "SS_ID"));
    return link_node(p1, param_decls);
}

static Node *remove_void_parameter(Node *param_decls)
{
    if (param_decls &&
            param_decls->extra.e_decl->type->tag == T_VOID &&
            param_decls->extra.e_decl->type->tag != T_PV) {
        /* no other params should be there */
        if (param_decls->next) {
            error_at_node(param_decls->next, "void must be the only parameter\n");
        }
        if (param_decls->extra.e_decl->name) {
            error_at_node(param_decls, "void parameter should not have a name\n");
        }
        /* void means empty parameter list */
        return 0;
    } else {
        return param_decls;
    }
}

Node *new_decl(Token k, Type *type)
{
    Var *var = new(Var);
    Node *decl = node(D_DECL, k, 0);
#ifdef DEBUG
    report("new_decl: %s\n", k.str);
#endif
    var->name = decl->token.str;
    var->type = type;
    var->decl = decl;
    decl->extra.e_decl = var;
    return decl;
}

/*
 * build a declaration (a syntax node with tag D_DECL)
 * from declarator node 'd' and child type 't'
 */
Node *mk_decl(Node *d, Type *t)
{
    uint num_elems = 0;
    Node *r;

    if (!d) {
        /* abstract declarator */
        Token k = {0,0,0,0};
        return new_decl(k, t);
    }
    switch (d->tag) {
    case E_ASSOP:
        /* initialiser */
        assert(d->token.symbol == TOK_EQUAL);
        r = mk_decl(d->assop_left, t);
        assert(r->tag == D_DECL);      /* post condition */
        r->decl_init = d->assop_right;
        return r;
    case E_VAR:
        return new_decl(d->token, t);
    case E_PRE:
        /* prefix type operator */
        switch (d->token.symbol) {
        case TOK_ASTERISK:
            /* pointer */
            switch (t->tag)
            {
            case T_NONE:
                error_at_node(d, "pointer to foreign entity\n");
                assert(impossible);
                break;
            default:
                break;
            }
            return mk_decl(d->pre_operand, mk_pointer_type(t));
        case TOK_CONST:
            /* const */
            switch (t->tag) {
            case T_NONE:
                error_at_node(d, "constant foreign entity\n");
                break;
            case T_EVFLAG:
                error_at_node(d, "constant event flag\n");
                break;
            case T_ARRAY:
                error_at_node(d, "constant array\n");
                break;
            case T_FUNCTION:
                warning_at_node(d, "discarding redundant const from function type\n");
                return mk_decl(d->pre_operand, t);
            case T_VOID:
            case T_PRIM:
            case T_FOREIGN:
            case T_POINTER:
            case T_STRUCT:
                break;
            case T_PV:
                warning_at_node(d, "discarding redundant const from pv type\n");
                return mk_decl(d->pre_operand, t);
            }
            if (t->is_const) {
                warning_at_node(d, "discarding repeated const\n");
                return mk_decl(d->pre_operand, t);
            } else {
                return mk_decl(d->pre_operand, mk_const_type(t));
            }
        case TOK_PV:
            /* pv */
            if (t->is_const) {
                error_at_node(d, "pv of constant\n");
                return mk_decl(d->pre_operand, t);
            }
#if 0
            /* type_is_valid_pv_child below already catches this error */
            if (type_contains_pv(t)) {
                error_at_node(d, "repeated pv\n");
                return mk_decl(d->pre_operand, t->val.pv.value_type);
            }
#endif
            if (t->tag == T_VOID || type_is_valid_pv_child(t)) {
                return mk_decl(d->pre_operand, mk_pv_type(t));
            } else {
                error_at_node(d, "invalid pv type\n");
                return mk_decl(d->pre_operand, t);
            }
        default:
            assert(impossible);
        }
        break;
    case E_SUBSCR:
        /* array */
        switch (t->tag) {
        case T_NONE:
            error_at_node(d, "array of foreign entities\n");
            assert(impossible);
            break;
        case T_VOID:
            error_at_node(d, "array of void\n");
            break;
        default:
            break;
        }
        assert(d->subscr_index->tag == E_CONST);
        if (!strtoui(d->subscr_index->token.str, UINT_MAX, &num_elems) || num_elems == 0) {
            error_at_node(d, "invalid array size (must be >= 1)\n");
            num_elems = 1;
        }
        return mk_decl(d->subscr_operand, mk_array_type(t, num_elems));
    case E_FUNC:
        /* function */
        switch (t->tag) {
        case T_NONE:
            error_at_node(d, "function returning foreign entity\n");
            assert(impossible);
            break;
        default:
            break;
        }
        return mk_decl(d->func_expr, mk_function_type(t,
            add_implicit_parameters(d, remove_void_parameter(d->func_args))));
    default:
        assert(impossible);
    }
    return 0;   /* pacify compiler */
}

/* multi-variable declaration */
Node *mk_decls(Node *ds, Type *t)
{
    Node *d, *r = 0;

    foreach(d, ds) {
        r = link_node(r, mk_decl(d, t));
    }
    return r;
}

Type *mk_prim_type(enum prim_type_tag tag)
{
    Type *t = new(Type);
    t->tag = T_PRIM;
    t->val.prim = tag;
    return t;
}

Type *mk_foreign_type(enum foreign_type_tag tag, char *name)
{
    Type *t = new(Type);
    t->tag = T_FOREIGN;
    t->val.foreign.tag = tag;
    t->val.foreign.name = name;
    return t;
}

Type *mk_ef_type()
{
    Type *t = new(Type);
    t->tag = T_EVFLAG;
    return t;
}

Type *mk_void_type()
{
    Type *t = new(Type);
    t->tag = T_VOID;
    return t;
}

Type *mk_no_type()
{
    Type *t = new(Type);
    t->tag = T_NONE;
    return t;
}

Type *mk_pointer_type(Type *t)
{
    Type *r = new(Type);
    r->tag = T_POINTER;
    r->val.pointer.value_type = t;
    return r;
}

Type *mk_array_type(Type *t, unsigned n)
{
    Type *r = new(Type);
    r->tag = T_ARRAY;
    r->val.array.elem_type = t;
    r->val.array.num_elems = n;
    return r;
}

Type *mk_const_type(Type *t)
{
    t->is_const = TRUE;
    return t;
}

Type *mk_pv_type(Type *t)
{
    Type *r = new(Type);
    r->tag = T_PV;
    r->val.pv.value_type = t;
    return r;
}

Type *mk_function_type(Type *t, Node *ps)
{
    Type *r = new(Type);
    r->tag = T_FUNCTION;
    r->val.function.return_type = t;
    r->val.function.param_decls = ps;
    return r;
}

Type *mk_structure_type(const char *name, Node *members)
{
    Type *r = new(Type);
    Node *member;

    r->tag = T_STRUCT;
    r->val.structure.member_decls = members;
    foreach (member, members) {
        if (member->tag == D_DECL) {
            /* do not count escaped code blocks */
            r->val.structure.num_members++;
        }
    }
    r->val.structure.name = name;
    r->val.structure.mark = 0;
    return r;
}

unsigned type_array_length(Type *t)
{
    switch (t->tag) {
    case T_ARRAY:
        return t->val.array.num_elems;
    default:
        return 1;
    }
}

unsigned type_assignable(Type *t)
{
    Node *member;

    if (t->is_const)
        return FALSE;
    switch (t->tag) {
    case T_NONE:
    case T_FOREIGN:
    case T_POINTER:
    case T_FUNCTION:
    case T_EVFLAG:
    case T_VOID:
        return FALSE;
    case T_STRUCT:
        foreach (member, t->val.structure.member_decls) {
            if (member->tag != D_DECL || !type_assignable(member->extra.e_decl->type))
                return FALSE;
        }
        return TRUE;
    case T_ARRAY:
        return type_assignable(t->val.array.elem_type);
    case T_PRIM:
        return TRUE;
    case T_PV:
        return TRUE;    /* already checked, see mk_decl */
#if 0
        return type_assignable(t->val.pv.value_type);
#endif
    }
    /* avoid bogus compiler warning: */
    assert(impossible);
    return FALSE;
}

enum assoc {
    L,
    R,
};

static void gen_pre(Type *t, enum assoc prev_assoc, int letter, int gen_pv)
{
    const char *sep = letter ? " " : "";
    if (t->is_const) {
        t->is_const = FALSE;
        gen_pre(t, L, TRUE, gen_pv);
        t->is_const = TRUE;
        gen_code("const%s", sep);
        return;
    }
    switch (t->tag) {
    case T_NONE:
        assert(impossible);
        break;
    case T_POINTER:
        gen_pre(t->val.pointer.value_type, L, TRUE, gen_pv);
        gen_code("*");
        break;
    case T_ARRAY:
        gen_pre(t->val.array.elem_type, R, letter || prev_assoc == L, gen_pv);
        if (prev_assoc == L)
            gen_code("(");
        break;
    case T_FUNCTION:
        gen_pre(t->val.function.return_type, R, letter || prev_assoc == L, gen_pv);
        if (prev_assoc == L)
            gen_code("(");
        break;
    case T_EVFLAG:
        gen_code("evflag%s", sep);
        break;
    case T_VOID:
        gen_code("void%s", sep);
        break;
    case T_PRIM:
        gen_code("%s%s", prim_type_name[t->val.prim], sep);
        break;
    case T_FOREIGN:
        gen_code("%s%s%s", foreign_type_prefix[t->val.foreign.tag], t->val.foreign.name, sep);
        break;
    case T_STRUCT:
        gen_code("struct %s%s", t->val.structure.name, sep);
        break;
    case T_PV:
        if (gen_pv) {
            gen_code("CH_ID%s", sep);
        } else {
            gen_pre(t->val.pv.value_type, prev_assoc, letter, FALSE);
        }
        break;
    }
}

static void gen_post(Type *t, enum assoc prev_assoc, int gen_pv)
{
    Node *pd;

    if (t->is_const) {
        t->is_const = FALSE;
        gen_post(t, L, gen_pv);
        t->is_const = TRUE;
        return;
    }
    switch (t->tag) {
    case T_POINTER:
        gen_post(t->val.pointer.value_type, L, gen_pv);
        break;
    case T_ARRAY:
        if (prev_assoc == L)
            gen_code(")");
        gen_code("[%d]", t->val.array.num_elems);
        gen_post(t->val.array.elem_type, R, gen_pv);
        break;
    case T_FUNCTION:
        if (prev_assoc == L)
            gen_code(")");
        gen_code("(");
        foreach (pd, t->val.function.param_decls) {
            Var *var = pd->extra.e_decl;
            if (type_contains_pv(var->type)) {
                gen_pv_type(var->type, "", var->name);
            } else {
                gen_type(var->type, "", var->name);
            }
            if (pd->next)
                gen_code (", ");
        }
        gen_code(")");
        gen_post(t->val.function.return_type, R, gen_pv);
        break;
    case T_PV:
        if (!gen_pv)
            gen_post(t->val.pv.value_type, prev_assoc, gen_pv);
        break;
    default:
        break;
    }
}

void gen_type(Type *t, const char *prefix, const char *name)
{
    gen_pre(t, R, name != NULL, FALSE);
    if (name)
        gen_code("%s%s", prefix, name);
    gen_post(t, R, FALSE);
}

void gen_pv_type(Type *t, const char *prefix, const char *name)
{
    gen_pre(t, R, TRUE, TRUE);
    if (name)
        gen_code("%s%s%s", prefix, NM_CHID, name);
    gen_post(t, R, TRUE);
}

static void ind(int level)
{
    while (level--)
        report("  ");
}

void dump_type(Type *t, int l)
{
    ind(l); 
    report("%s", type_tag_names[t->tag]);
    if (t->is_const)
        report(", is_const");
    switch (t->tag) {
    case T_NONE:
    case T_EVFLAG:
    case T_VOID:
        report("\n");
        break;
    case T_PRIM:
        report(" %s\n", prim_type_tag_name[t->val.prim]);
        break;
    case T_FOREIGN:
        report(" %s%s\n",
            foreign_type_prefix[t->val.foreign.tag], t->val.foreign.name);
        break;
    case T_POINTER:
        report(" to\n");
        dump_type(t->val.pointer.value_type, l+1);
        break;
    case T_ARRAY:
        report("[%d] of\n", t->val.array.num_elems);
        dump_type(t->val.array.elem_type, l+1);
        break;
    case T_FUNCTION:
        report(" returning\n");
        dump_type(t->val.function.return_type, l+1);
        break;
    case T_STRUCT:
        report(" %s\n", t->val.structure.name);
        break;
    case T_PV:
        report(" of\n");
        dump_type(t->val.pv.value_type, l+1);
    }
}

Type *child_type(Type *t)
{
    switch (t->tag) {
    case T_NONE:
    case T_EVFLAG:
    case T_VOID:
    case T_PRIM:
    case T_FOREIGN:
    case T_STRUCT:
        return 0;
    case T_POINTER:
        return t->val.pointer.value_type;
    case T_ARRAY:
        return t->val.array.elem_type;
    case T_FUNCTION:
        return t->val.function.return_type;
    case T_PV:
        return t->val.pv.value_type;
    }
    return 0;   /* pacify compiler */
}

Type *base_type(Type *t)
{
    Type *child = child_type(t);
    if (child)
        return base_type(child);
    else
        return t;
}

void dump_node(Node *e, int level);

/* Note: this function does intentionally *not* consider struct members */
Type *type_contains_pv(Type *t)
{
    Type *ct = child_type(t);
    if (t->tag == T_PV)
        return t;
    else if (ct)
        return type_contains_pv(ct);
    else
        return 0;
}
