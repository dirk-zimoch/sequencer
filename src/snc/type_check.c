/*************************************************************************\
Copyright (c) 2013-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/* Extremely rudimentary type checker.

   The function type_of takes an expression and returns an approximation of
   its type. Used for code generation when we want to find out

    (a) whether a given expression has function type (so we can add
        implicit parameters)

    (b) for which parts of an expression we have to substitute the
        channel id instead of the value of variable

   It does almost no actual type checking, delegating this to the C compiler.
   The returned type may differ from the real type because we appoximate e.g.
   the result of binary operators to 'int'.

   In contrast, the function pv_type_check really does type checking.
   This is needed because channels are translated to the monomorphic type CH_ID,
   so the C side cannot do any type checking for channels.
*/

#include <assert.h>
#include <string.h>
#include "main.h"
#include "snl.h"
#include "types.h"
#include "var_types.h"
#include "node.h"
#include "type_check.h"

static const int impossible = FALSE;

/* some type "constants" */
#define num_type    mk_prim_type(P_INT)
#define char_type   mk_prim_type(P_CHAR)
#define no_type     mk_no_type()


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
    dump_node(e, 1);
#endif

    if (e->type)
        return e->type;

    switch (e->tag) {
    case E_ASSOP:                       /* assignment operator [left,right] */
        return e->type = strip_pv_type(type_of(e->assop_left));
    case E_BINOP:                       /* binary operator [left,right] */
        l = strip_pv_type(type_of(e->binop_left));
        r = strip_pv_type(type_of(e->binop_right));
        switch (e->token.symbol) {
        case TOK_SUB:
        case TOK_ADD:
            if (type_is_pointer(l) && !type_is_pointer(r)) {
                return e->type = l;
            }
            if (!type_is_pointer(l) && type_is_pointer(r)) {
                return e->type = r;
            }
            if (type_is_pointer(l) && type_is_pointer(r)) {
                error_at_node(e, "invalid operands of binary %s", e->token.str);
                return e->type = no_type;
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
            if (type_is_dynamic(l) || type_is_dynamic(r))
                return e->type = no_type;
            return e->type = num_type;
        case TOK_COMMA:
            return e->type = r;
        default:
            error_at_node(e, "unexpected binary operator %s\n", e->token.str);
            dump_node(e, 0);
            assert(impossible);
            return e->type = no_type;
        }
    case E_CAST:                        /* type cast [type,operand] */
        assert(e->cast_type);
        assert(e->cast_type->extra.e_decl);
        return e->type = e->cast_type->extra.e_decl->type;
    case E_CONST:                       /* numeric (inkl. character) constant [] */
        return e->type = num_type;
    case E_FUNC:                        /* function call [expr,args] */
        t = type_of(e->func_expr);
        if (t->tag == T_FUNCTION) {
            return e->type = t->val.function.return_type;
        } else {
            return e->type = no_type;
        }
    case E_INIT:                        /* array or struct initialiser [elems] */
        return e->type = no_type;
    case E_MEMBER:                      /* struct or union member [] */
        assert(impossible);             /* handled in case E_SELECT */
        return e->type = no_type;
    case E_PAREN:                       /* parenthesis around an expression [expr] */
        return e->type = type_of(e->paren_expr);
    case E_POST:                        /* unary postfix operator [operand] */
        return e->type = strip_pv_type(type_of(e->post_operand));
    case E_PRE:                         /* unary prefix operator [operand] */
        t = type_of(e->pre_operand);
        switch (e->token.symbol) {
        case TOK_ADD:
        case TOK_SUB:
            return e->type = strip_pv_type(t);
        case TOK_ASTERISK:
            t = type_is_pointer(t);
            if (t) {
                return e->type = t;
            } else {
                return e->type = no_type;
            }
        case TOK_AMPERSAND:
            return e->type = mk_pointer_type(t);
        case TOK_NOT:
        case TOK_TILDE:
            return e->type = num_type;
        case TOK_INCR:
        case TOK_DECR:
            return e->type = strip_pv_type(t);
        case TOK_SIZEOF:
            return e->type = num_type;
        default:
            error_at_node(e, "unexpected prefix operator %s\n", e->token.str);
            dump_node(e, 0);
            assert(impossible);
            return e->type = no_type;
        }
    case E_SELECT:                      /* member selection [left,right] */
        t = type_of(e->select_left);
        switch (e->token.symbol) {
        case TOK_POINTER:
            if (t->tag == T_POINTER)
                t = t->val.pointer.value_type;
            else
                return e->type = no_type;
            /* fall through */
        case TOK_PERIOD:
            if (t->tag == T_STRUCT)
                return e->type = member_type(t->val.structure.member_decls, e->select_right->token.str);
            else
                return e->type = no_type;
        default:
            error_at_node(e, "unexpected member selection operator %s\n", e->token.str);
            dump_node(e, 0);
            assert(impossible);
            return e->type = no_type;
        }
    case E_SIZEOF:                      /* sizeof [decl] */
        return e->type = num_type;
    case E_STRING:                      /* string constant [] */
        return e->type = mk_pointer_type(mk_const_type(char_type));
    case E_SUBSCR:                      /* subscripted expr [operand,index] */
        t = type_is_pointer(strip_pv_type(type_of(e->subscr_operand)));
        if (t) {
            return e->type = t;
        } else {
            return e->type = no_type;
        }
    case E_TERNOP:                      /* ternary operator [cond,then,else] */
        return e->type = type_of(e->ternop_then);
    case E_VAR:                         /* variable [] */
        if (!e->extra.e_var)
            return e->type = no_type;
        else
            return e->type = e->extra.e_var->type;
    default:
        error_at_node(e, "unexpected node tag %s\n", node_name(e));
        dump_node(e, 0);
        assert(impossible);
        return e->type = no_type;
    }
}

int pv_type_check(Type *expected, Type *inferred)
{
    switch (expected->tag)
    {
    case T_NONE:
        /* if this is due to an earlier type error, don't bother */
        return TRUE;
    case T_PV:
        return inferred->tag == T_PV && pv_type_check(
            expected->val.pv.value_type, inferred->val.pv.value_type);
    case T_PRIM:
        return inferred->tag == T_PRIM && expected->val.prim == inferred->val.prim;
    case T_ARRAY:
        return inferred->tag == T_ARRAY && pv_type_check(
            expected->val.array.elem_type, inferred->val.array.elem_type);
    case T_POINTER:
        return (inferred->tag == T_ARRAY && pv_type_check(
                expected->val.pointer.value_type, inferred->val.array.elem_type))
            || (inferred->tag == T_POINTER && pv_type_check(
                expected->val.pointer.value_type, inferred->val.pointer.value_type));
    case T_VOID:
        return type_is_valid_pv_child(inferred);
    default:
        dump_type(expected, 0);
        assert(impossible);
        return FALSE;
    }
}
