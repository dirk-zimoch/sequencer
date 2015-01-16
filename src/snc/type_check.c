/*************************************************************************\
Copyright (c) 2013-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/* Extremely rudimentary type checker. The function type_of takes an
   expression and returns an approximation of its type. Used for code
   generation when we want to find out
    (a) whether a given expression has function type (so we can add
        implicit parameters)
    (b) for which parts of an expression we have to substitute the
        channel id instead of the value of variable
   Error reporting is done only in a few cases
*/

#include <assert.h>
#include <string.h>
#include "main.h"
#include "snl.h"
#include "types.h"
#include "var_types.h"
#include "type_check.h"

static const int impossible = FALSE;

/* some type "constants" */
#define num_type    mk_prim_type(P_INT)
#define char_type   mk_prim_type(P_CHAR)
#define no_type     mk_no_type()


void dump_expr(Node *e, int level)
{
    int i, l;
    for (l = 0; l < level; l++) report("  ");
    if (e) {
        Node *ce;
        report("%s '%s'\n", node_name(e), e->token.str);
        for (i = 0; i < node_info[e->tag].num_children; i++) {
            foreach (ce, e->children[i]) {
                dump_expr(ce, level+1);
            }
        }
    } else {
        report("***NULL***\n");
    }
}

static Type *member_type(Node *members, const char *name)
{
    Node *member;
    foreach(member, members) {
        if (member->tag == D_DECL && strcmp(member->token.str, name) == 0) {
            return member->extra.e_decl->type;
        }
    }
    return no_type;
}

/* Infer the type of an expression. A result of no_type means we can't say,
   either because of a type error, or because we don't have enough information,
   e.g. due to involvement of foreign types, functions, or variables. */
Type *type_of(Node *e)
{
    Type *t, *l, *r;

#ifdef DEBUG
    report("type_of()\n");
    dump_expr(e, 1);
#endif

    switch (e->tag) {
    case E_ASSOP:                       /* assignment operator [left,right] */
        l = type_of(e->assop_left);
        r = type_of(e->assop_right);
        switch (e->token.symbol) {
        case TOK_ADDEQ:
        case TOK_SUBEQ:
            if (type_is_pointer(l) && !type_is_pointer(r)) {
                return l;
            }
            if (!type_is_pointer(l) && type_is_pointer(r)) {
                return r;
            }
            if (type_is_pointer(l) && type_is_pointer(r)) {
                return error_at_node(e, "invalid operands of %s", e->token.str), no_type;
            }
            /* fall through */
        default:
            if (l->tag == T_FOREIGN || r->tag == T_FOREIGN)
                return no_type;
            if (l->tag == T_NONE || r->tag == T_NONE)
                return no_type;
            return strip_pv_type(l);
            break;
        }
        break;
    case E_BINOP:                       /* binary operator [left,right] */
        l = type_of(e->binop_left);
        r = type_of(e->binop_right);
        switch (e->token.symbol) {
        case TOK_SUB:
        case TOK_ADD:
            if (type_is_pointer(l) && !type_is_pointer(r)) {
                return l;
            }
            if (!type_is_pointer(l) && type_is_pointer(r)) {
                return r;
            }
            if (type_is_pointer(l) && type_is_pointer(r)) {
                return error_at_node(e, "invalid operands of binary %s", e->token.str), no_type;
            }
            /* fall through */
        case TOK_ASTERISK:
        case TOK_SLASH:
        case TOK_GT:
        case TOK_GE:
        case TOK_EQ:
        case TOK_NE:
        case TOK_LE:
        case TOK_LT:
        case TOK_OROR:
        case TOK_ANDAND:
        case TOK_LSHIFT:
        case TOK_RSHIFT:
        case TOK_VBAR:
        case TOK_CARET:
        case TOK_AMPERSAND:
        case TOK_MOD:
            if (l->tag == T_FOREIGN || r->tag == T_FOREIGN)
                return no_type;
            if (l->tag == T_NONE || r->tag == T_NONE)
                return no_type;
            return num_type;
        case TOK_COMMA:
            return strip_pv_type(r);
        }
    case E_CAST:                        /* type cast [type,operand] */
        assert(e->cast_type);
        assert(e->cast_type->extra.e_decl);
        return e->cast_type->extra.e_decl->type;
    case E_CONST:                       /* numeric (inkl. character) constant [] */
        return num_type;
    case E_FUNC:                        /* function call [expr,args] */
        t = type_of(e->func_expr);
        if (t->tag == T_FUNCTION) {
            return t->val.function.return_type;
        } else {
            return no_type;
        }
    case E_INIT:                        /* array or struct initializer [elems] */
        return no_type;
    case E_MEMBER:                      /* struct or union member [] */
        assert(impossible);             /* handled in case E_SELECT */
        return no_type;
    case E_PAREN:                       /* parenthesis around an expression [expr] */
        return type_of(e->paren_expr);
    case E_POST:                        /* unary postfix operator [operand] */
        return type_of(e->post_operand);
    case E_PRE:                         /* unary prefix operator [operand] */
        t = type_of(e->pre_operand);
        switch (e->token.symbol) {
        case TOK_ADD:
        case TOK_SUB:
            return strip_pv_type(t);
        case TOK_ASTERISK:
            if (t->tag == T_POINTER) {
                return t->val.pointer.value_type;
            } else if (t->tag == T_ARRAY) {
                return t->val.array.elem_type;
            } else if (t->tag == T_PRIM && t->val.prim == P_STRING) {
                return char_type;
            } else {
                return no_type;
            }
        case TOK_AMPERSAND:
            return mk_pointer_type(t);
        case TOK_NOT:
        case TOK_TILDE:
            return num_type;
        case TOK_INCR:
        case TOK_DECR:
            return strip_pv_type(t);
        case TOK_SIZEOF:
            return num_type;
        }
    case E_SELECT:                      /* member selection [left,right] */
        t = type_of(e->select_left);
        switch (e->token.symbol) {
        case TOK_POINTER:
            if (t->tag == T_POINTER)
                t = t->val.pointer.value_type;
            else
                return no_type;
            /* fall through */
        case TOK_PERIOD:
            if (t->tag == T_STRUCT)
                return member_type(t->val.structure.member_decls, e->select_right->token.str);
            else
                return no_type;
        default:
            assert(impossible);
            return no_type;
        }
    case E_SIZEOF:                      /* sizeof [decl] */
        return num_type;
    case E_STRING:                      /* string constant [] */
        return mk_pointer_type(mk_const_type(char_type));
    case E_SUBSCR:                      /* subscripted expr [operand,index] */
        t = strip_pv_type(type_of(e->subscr_operand));
        switch (t->tag) {
        case T_ARRAY:
            return t->val.array.elem_type;
        case T_POINTER:
            return t->val.pointer.value_type;
        case T_PRIM:
            if (t->val.prim == P_STRING)
                return char_type;
            else
                return no_type;
        default:
            return error_at_node(e->subscr_operand, "subscript operand has wrong type\n"), no_type;
        }
    case E_TERNOP:                      /* ternary operator [cond,then,else] */
        return type_of(e->ternop_then);
    case E_VAR:                         /* variable [] */
        if (!e->extra.e_var)
            return mk_no_type();
        else
            return e->extra.e_var->type;
    default:
        error_at_node(e, "unexpected expression type %s\n", node_name(e));
        assert(impossible);
        return no_type;
    }
}
