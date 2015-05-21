/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#ifndef INCLvar_typesh
#define INCLvar_typesh

#define declare_prim_type_names
#include "seq_prim_types.h"
#include "types.h"

enum type_tag {
    T_NONE,     /* undeclared (or declared as foreign) variable */
    T_EVFLAG,   /* event flag */
    T_VOID,     /* void type */
    T_PRIM,     /* primitive types: numbers, char, string */
    T_FOREIGN,  /* foreign types (declared in C code) */
    T_POINTER,
    T_ARRAY,
    T_FUNCTION,
    T_STRUCT,   /* struct defined in SNL code */
    T_PV,
};

enum foreign_type_tag {
    F_ENUM,
    F_STRUCT,
    F_UNION,
    F_TYPENAME,
};

struct type {
    enum type_tag tag;
    unsigned is_const:1;
    unsigned contains_pv;
    union {
        enum prim_type_tag prim;
        struct {
            enum foreign_type_tag tag;
            char *name;
        } foreign;
        struct {
            Type *value_type;
        } pointer;
        struct {
            unsigned num_elems;
            Type *elem_type;
        } array;
        struct {
            Type *return_type;
            Node *param_decls;
        } function;
        struct {
            const char *name;
            unsigned num_members;   /* only SNL members, not escaped code */
            Node *member_decls;     /* all members, including escaped code */
            int mark;               /* avoid infinite recursion when resolving struct names */
        } structure;
        struct {
            Type *value_type;
        } pv;
    } val;
};

/* base type for any combination of pointers and arrays */
Type *base_type(Type *t);

/* child type for pointer, array, function, and pv */
Type *child_type(Type *t);

/* array length */
unsigned type_array_length(Type *t);

/* whether type can be assign'ed to a PV */
unsigned type_assignable(Type *t);

/* whether type contains PV marker */
Type *type_contains_pv(Type *t);

#define type_is_valid_pv_child(t) ((t)->tag == T_PRIM || (t)->tag == T_VOID || \
    ((t)->tag == T_ARRAY && (t)->val.array.elem_type->tag == T_PRIM))

#define type_is_function(t) ((t)->tag == T_FUNCTION ? t : \
    (t)->tag == T_POINTER && (t)->val.pointer.value_type->tag == T_FUNCTION ? \
    (t)->val.pointer.value_type : 0)

#define type_is_pointer(t) ((t)->tag == T_POINTER ? (t)->val.pointer.value_type :\
    (t)->tag == T_ARRAY ? (t)->val.array.elem_type :\
    ((t)->tag == T_PRIM && (t)->val.prim == P_STRING) ? mk_prim_type(P_CHAR) : 0)

#define strip_pv_type(t) ((t)->tag == T_PV ? (t)->val.pv.value_type : (t))

/*
 * The type T_NONE roughly corresponds to the "dynamic type" in the
 * gradual typing literature: it is compatible with everything.
 * However, a foreign typedef has the same status (it could be anything).
 */
#define type_is_dynamic(t) ((t)->tag == T_NONE || \
    ((t)->tag == T_FOREIGN && (t)->val.foreign.tag == F_TYPENAME))

/* generate type, name is an optional variable name */
void gen_type(Type *t, const char *prefix, const char *name);
/* generate channel id type */
void gen_pv_type(Type *t, const char *prefix, const char *name);

/* creating types */
Type *mk_prim_type(enum prim_type_tag tag);
Type *mk_foreign_type(enum foreign_type_tag tag, char *name);
Type *mk_ef_type();
Type *mk_void_type();
Type *mk_no_type();
Type *mk_pointer_type(Type *t);
Type *mk_array_type(Type *t, unsigned n);
Type *mk_const_type(Type *t);
Type *mk_function_type(Type *t, Node *ps);
Type *mk_structure_type(const char *name, Node *members);
Type *mk_pv_type(Type *t);

Node *mk_decl(Node *d, Type *t);
Node *mk_decls(Node *ds, Type *t);

Node *new_decl(Token k, Type *type);

void dump_type(Type *t, int l);

#ifndef var_types_GLOBAL
extern
#endif
const char *foreign_type_prefix[]
#ifdef var_types_GLOBAL
= {
    "enum ",
    "struct ",
    "union ",
    "",
}
#endif
;

#ifndef var_types_GLOBAL
extern
#endif
const char *type_tag_names[]
#ifdef var_types_GLOBAL
= {
    "T_NONE",
    "T_EVFLAG",
    "T_VOID",
    "T_PRIM",
    "T_FOREIGN",
    "T_POINTER",
    "T_ARRAY",
    "T_FUNCTION",
    "T_STRUCT",
    "T_PV",
}
#endif
;

#endif /*INCLvar_typesh */
