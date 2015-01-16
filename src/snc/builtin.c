/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#include <stdlib.h>

#include "builtin.h"
#include "var_types.h"
#include "snl.h"
#include "node.h"
#include "gen_code.h"
#include "analysis.h"

static Node *mk_const(char *code, int token)
{
    Token k;

    k.symbol = token;
    k.str = code;
    k.line = 1;
    k.file = "<builtin>";
    return node(E_CONST, k);
}

static Var *mk_var(Node *scope, char *name, Type *type)
{
    Var *vp = new(Var);
    vp->name = name;
    vp->type = type;
    if (type->tag == T_EVFLAG)
        vp->chan->val.evflag = new(EvFlag);
    add_var_to_scope(vp, scope);
    return vp;
}

static Var *mk_func(Node *scope, char *name, Type *return_type, Node *param_decls)
{
    return mk_var(scope, name, mk_function_type(return_type, param_decls));
}

static Node *mk_def_param(char *name, Type *t, Node *def)
{
    Token k;
    Node *d;

    k.symbol = TOK_NAME;
    k.str = name;
    k.line = 1;
    k.file = "<builtin>";
    d = new_decl(k, t);
    if (def) {
        d->decl_init = def;
    }
    return d;
}

#define mk_param(n,t)           mk_def_param(n,t,0)

/* just some unique object */
static void *builtin_const = &builtin_const;

/* Insert builtin functions and constants into symbol table */
void register_builtins(SymTable st, Node *scope)
{
    VarList *vl = var_list_from_scope(scope);

    Node *defaultCompType = mk_const("DEFAULT",         TOK_INTCON);
    Node *defaultTimeout  = mk_const("DEFAULT_TIMEOUT", TOK_FPCON);
    Node *defaultNumElems = mk_const("1",               TOK_INTCON);
    Node *defaultAll      = mk_const("FALSE",           TOK_INTCON);
    Node *defaultNull     = mk_const("NULL",            TOK_INTCON);

    Type *unsignedT       = mk_prim_type(P_UINT);
    Type *charT           = mk_prim_type(P_CHAR);
    Type *doubleT         = mk_prim_type(P_DOUBLE);
    Type *voidT           = mk_void_type();
    Type *efT             = mk_ef_type();
    Type *pvT             = mk_pv_type(voidT);
    Type *pvPtrT          = mk_pointer_type(pvT);
    Type *charPtrT        = mk_pointer_type(charT);
    Type *charConstPtrT   = mk_pointer_type(mk_const_type(charT));
    Type *pvStatT         = mk_foreign_type(F_ENUM, "pvStat");
    Type *pvSevrT         = mk_foreign_type(F_ENUM, "pvSevr");
    Type *seqBoolT        = mk_foreign_type(F_TYPENAME, "seqBool");
    Type *seqBoolPtrT     = mk_pointer_type(seqBoolT);
    Type *timestampT      = mk_foreign_type(F_STRUCT, "epicsTimeStamp");
    Type *compT           = mk_foreign_type(F_ENUM, "compType");
    Type *ssT             = mk_foreign_type(F_TYPENAME, "SS_ID");

    #define cins(n)                 sym_table_insert(st, n, builtin_const, mk_const(n, TOK_INTCON))
    #define fins(n,r,ps)            sym_table_insert(st, n, vl, mk_func(scope, "seq_" n, r,\
                                        link_node(mk_param(NM_ENV, ssT),ps)))
    #define finsa(n,a,r,ps)         sym_table_insert(st, n, vl, mk_func(scope, "seq_" a, r,\
                                        link_node(mk_param(NM_ENV, ssT),ps)))
    #define fins0(n,r)              fins(n,r,0)
    #define fins1(n,r,p1)           fins(n,r,p1)
    #define fins2(n,r,p1,p2)        fins(n,r,link_node(p1,p2))
    #define fins3(n,r,p1,p2,p3)     fins(n,r,link_node(p1,link_node(p2,p3)))
    #define fins3a(n,a,r,p1,p2,p3)  finsa(n,a,r,link_node(p1,link_node(p2,p3)))
    #define fins4(n,r,p1,p2,p3,p4)  fins(n,r,link_node(p1,link_node(p2,link_node(p3,p4))))

    /*** constants ***/

    /* seqBool */
    cins("TRUE");
    cins("FALSE");

    /* enum compType */
    cins("DEFAULT");
    cins("SYNC");
    cins("ASYNC");

    /* for pvSync */
    cins("NOEVFLAG");

    /* enum pvSevr */
    cins("pvStatOK");
    cins("pvStatERROR");
    cins("pvStatDISCONN");
    cins("pvStatREAD");
    cins("pvStatWRITE");
    cins("pvStatHIHI");
    cins("pvStatHIGH");
    cins("pvStatLOLO");
    cins("pvStatLOW");
    cins("pvStatSTATE");
    cins("pvStatCOS");
    cins("pvStatCOMM");
    cins("pvStatTIMEOUT");
    cins("pvStatHW_LIMIT");
    cins("pvStatCALC");
    cins("pvStatSCAN");
    cins("pvStatLINK");
    cins("pvStatSOFT");
    cins("pvStatBAD_SUB");
    cins("pvStatUDF");
    cins("pvStatDISABLE");
    cins("pvStatSIMM");
    cins("pvStatREAD_ACCESS");
    cins("pvStatWRITE_ACCESS");

    /* enum pvSevr */
    cins("pvSevrOK");
    cins("pvSevrERROR");
    cins("pvSevrNONE");
    cins("pvSevrMINOR");
    cins("pvSevrMAJOR");
    cins("pvSevrINVALID");

    cins("seqg_var");
    cins("seqg_env");

    /*** functions ***/

    /* event flag operations */
    fins1("efSet",              seqBoolT,       mk_param("event_flag", efT));
    fins1("efClear",            seqBoolT,       mk_param("event_flag", efT));
    fins1("efTest",             seqBoolT,       mk_param("event_flag", efT));
    fins1("efTestAndClear",     seqBoolT,       mk_param("event_flag", efT));

    /* pv operations */
    fins3a("pvGet", "pvGetTmo", pvStatT,        mk_param("channel", pvT),
                                                mk_def_param("comp_type", compT, defaultCompType),
                                                mk_def_param("timeout", doubleT, defaultTimeout));
    fins1("pvGetQ",             seqBoolT,       mk_param("channel", pvT));
    fins1("pvFlushQ",           voidT,          mk_param("channel", pvT));
    fins3a("pvPut", "pvPutTmo", pvStatT,        mk_param("channel", pvT),
                                                mk_def_param("comp_type", compT, defaultCompType),
                                                mk_def_param("timeout", doubleT, defaultTimeout));
    fins1("pvGetComplete",      seqBoolT,       mk_param("channel", pvT));
    fins4("pvArrayGetComplete", seqBoolT,       mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT),
                                                mk_def_param("any", seqBoolT, defaultAll),
                                                mk_def_param("done", seqBoolPtrT, defaultNull));
    fins4("pvPutComplete",      seqBoolT,       mk_param("channel", pvT),
                                                mk_def_param("num_elems", unsignedT, defaultNumElems),
                                                mk_def_param("any", seqBoolT, defaultAll),
                                                mk_def_param("done", seqBoolPtrT, defaultNull));
    fins4("pvArrayPutComplete", seqBoolT,       mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT),
                                                mk_def_param("any", seqBoolT, defaultAll),
                                                mk_def_param("done", seqBoolPtrT, defaultNull));
    fins1("pvGetCancel",        pvStatT,        mk_param("channel", pvT));
    fins2("pvArrayGetCancel",   pvStatT,        mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT));
    fins1("pvPutCancel",        pvStatT,        mk_param("channel", pvT));
    fins2("pvArrayPutCancel",   pvStatT,        mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT));
    fins2("pvAssign",           pvStatT,        mk_param("channel", pvT),
                                                mk_param("pv_name", charConstPtrT));
    fins2("pvAssignSubst",      pvStatT,        mk_param("channel", pvT),
                                                mk_param("pv_name", charConstPtrT));
    fins1("pvMonitor",          pvStatT,        mk_param("channel", pvT));
    fins2("pvArrayMonitor",     pvStatT,        mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT));
    fins1("pvStopMonitor",      pvStatT,        mk_param("channel", pvT));
    fins2("pvArrayStopMonitor", pvStatT,        mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT));

    /* pv info */
    fins1("pvName",             charPtrT,       mk_param("channel", pvT));
    fins1("pvCount",            unsignedT,      mk_param("channel", pvT));
    fins1("pvStatus",           pvStatT,        mk_param("channel", pvT));
    fins1("pvSeverity",         pvSevrT,        mk_param("channel", pvT));
    fins1("pvTimeStamp",        timestampT,     mk_param("channel", pvT));
    fins1("pvMessage",          charConstPtrT,  mk_param("channel", pvT));
    fins1("pvAssigned",         pvStatT,        mk_param("channel", pvT));
    fins1("pvConnected",        seqBoolT,       mk_param("channel", pvT));
    fins2("pvArrayConnected",   seqBoolT,       mk_param("channel", pvT),
                                                mk_param("num_elems", unsignedT));
    fins1("pvIndex",            pvStatT,        mk_param("channel", pvT));

    /* global operations */
    fins0("pvFlush",            pvStatT);
    fins2("pvSync",             pvStatT,        mk_param("channel", pvT),
                                                mk_param("event_flag", efT));
    fins3("pvArraySync",        pvStatT,        mk_param("channel", pvPtrT),
                                                mk_param("num_elems", unsignedT),
                                                mk_param("event_flag", efT));
    fins1("delay",              seqBoolT,       mk_param("delay_in_seconds", doubleT));
    fins1("macValueGet",        charPtrT,       mk_param("param_name", charConstPtrT));

    /* global info */
    fins0("pvChannelCount",     unsignedT);
    fins0("pvConnectCount",     unsignedT);
    fins0("pvAssignCount",      unsignedT);
    fins1("optGet",             seqBoolT,       mk_param("opt_name", charConstPtrT));
}

Node *lookup_builtin_const(SymTable sym_table, const char *name)
{
    return (Node *)sym_table_lookup(sym_table, name, builtin_const);
}
