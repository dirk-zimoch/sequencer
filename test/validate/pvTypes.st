/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program pvTypesTest

%%#include "../testSupport.h"

option +s;

int pv pv_i;        /* pv of int */
int pv pv_j;        /* pv of int */
int pv *p_pv_i;     /* ptr to pv of int */
int pv *q_pv_i;     /* ptr to pv of int */
int pv a_pv_i[3];   /* array of pv of int */
int (pv pv_a_i)[2]; /* pv of array of int */

monitor pv_i;
monitor pv_a_i;
syncq pv_i 10;

entry {
    seq_test_init(22);
}

#define testOk1(cond) testOk(cond, "%s", #cond)

ss set {
    state one {
        when () {
            int i = 1;

            testOk1(1 == (i = 1));
            testOk1(1 == (pv_i = i));
            testDiag("&pv_i -> %p", &pv_i);
            testOk1(++pv_i == 2);
            testOk1(1 == (pv_j = pv_i-1));
            testOk1((--pv_j)==0);

            testOk1(*(&pv_i) == pv_i);
            testDiag("p_pv_i=&pv_i+0 -> %p", p_pv_i=&pv_i+0);
            testOk1(*p_pv_i == pv_i);

            testOk1(3 == (pv_i+1));
            testOk1(2 == *p_pv_i);

            testDiag("a_pv_i[0]=0 -> %d", a_pv_i[0]=0);
            testDiag("a_pv_i[1]=1 -> %d", a_pv_i[1]=1);
            testDiag("a_pv_i[2]=2 -> %d", a_pv_i[2]=2);

            testDiag("q_pv_i=a_pv_i+1 -> %p", q_pv_i=a_pv_i+1);
            testOk1(*q_pv_i == a_pv_i[1]);
            testOk1((*q_pv_i=*p_pv_i, *q_pv_i==2));

            testDiag("a_pv_i[0]==*a_pv_i -> %d", a_pv_i[0]==*a_pv_i);
            testDiag("pv_a_i[0]=pv_i -> %d", pv_a_i[0]=pv_i);

            testOk1((pv_a_i[0]=a_pv_i[1]) == 2);
            testOk1((pv_a_i[1]=a_pv_i[2]+1) == 3);
            testPvAi(pv_a_i);

            testDiag("p_pv_i=&(a_pv_i[0]) -> %p", p_pv_i=&(a_pv_i[0]));
            testDiag("*(p_pv_i+1) -> %d", *(p_pv_i+1));

            testDiag("p_pv_i=&pv_i+0 -> %p", p_pv_i=&pv_i);
            testOk1(*(&pv_i+0)==*p_pv_i);
            pvSetIntP(p_pv_i, 5);
            testOk1(pv_i==5);
            pvSetInt(pv_i, 3);
            testOk1(pv_i==3);
            pvSetInts(p_pv_i, 1, 4);
            testOk1(pv_i==4);

            pvSetInts(a_pv_i, 3, 4);
            testOk1(a_pv_i[0]==4);
            testOk1(a_pv_i[1]==4);
            testOk1(a_pv_i[2]==4);

#if 0 /* dirty conversion no longer allowed due to fix in the type checker */
            p_pv_i = &pv_a_i;
            pvSetInts(a_pv_i, 3, 5);
            testOk1(a_pv_i[0]==5);
            testOk1(a_pv_i[1]==5);
            testOk1(a_pv_i[2]==5);
#endif
#if 0
            /* C compiler correctly warns:
                expected 'struct channel **' but argument is of type 'int' */
            pvSetIntP(pv_i, 6);
            testOk1(pv_i==6);
#endif
        } state two
    }
    state two {
        when (delay(0.5)) {
        } exit
    }
}

ss get {
    state one {
        when (pvGetQ(pv_i)) {
            testDiag("ss get: pv_i=%d", pv_i);
        } state one
    }
}

exit {
    seq_test_done();
}

void pvSetInt(int pv x, int v)
{
    x = v;
    %%testDiag("pvSetInt(): seqg_chid_x=%p, v=%d", seqg_chid_x, v);
    pvPut(x);
}

void pvSetIntP(int pv *p, int v)
{
    *p = v;
    %%testDiag("pvSetIntP(): *seqg_chid_p=%p, v=%d", *seqg_chid_p, v);
    pvPut(*p);
}

void pvSetInts(int pv *x, int n, int v)
{
    int i;
    for (i=0; i<n; i++) {
	%%testDiag("pvSetInts(): i=%d, seqg_chid_x[i]=%p, v=%d", i, seqg_chid_x[i], v);
        pvSetInt(x[i], v);
    }
}

void testPvAi(int (pv pv_a_i)[2])
{
    testOk1((pv_a_i[0]=a_pv_i[1]) == 2);
    testOk1((pv_a_i[1]=a_pv_i[2]+1) == 3);
}
