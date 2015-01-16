/*************************************************************************\
Copyright (c) 2010-2012 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program pv_typesTest

%%#include "../testSupport.h"

option +s;

int pv pv_i;        /* pv of int */
int pv pv_j;        /* pv of int */
int pv *p_pv_i;     /* ptr to pv of int */
int pv *q_pv_i;     /* ptr to pv of int */
int pv a_pv_i[3];   /* array of pv of int */
int (pv pv_a_i)[2]; /* pv of array of int */

#if 0
monitor pv_i;
monitor pv_ai;

syncq pv_i 10;
#endif

entry {
    seq_test_init(0);
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

            testDiag("pv_a_i[0]=a_pv_i[1] -> %d", pv_a_i[0]=a_pv_i[1]);
            testDiag("pv_a_i[1]=a_pv_i[2] -> %d", pv_a_i[1]=a_pv_i[2]);

            testDiag("p_pv_i=&(a_pv_i[0]) -> %p", p_pv_i=&(a_pv_i[0]));
            testDiag("*(p_pv_i+1) -> %d", *(p_pv_i+1));
            testDiag("*(&pv_i+1) -> %d", *(&pv_i+1));

            pvSetInts(p_pv_i, 1, 4);
            pvSetIntP(p_pv_i, 5);
            pvSetInt(pv_i, 3);
#if 0
            p_pv_i = &pv_ai;
            pvSetInts(p_pv_i, 1, 4);
            pvSetIntP(p_pv_i, 5);
#endif
        } state two
    }
    state two {
        when (delay(0.5)) {
        } exit
    }
}

#if 0
ss get {
    state one {
        when (pvGetQ(pv_i)) {
            testDiag("pv_i=%d", pv_i);
        } state one
    }
}
#endif

exit {
    testDiag("pv_i=%d", pv_i);
    testDiag("*p_pv_i=%d", *p_pv_i);
    seq_test_done();
}

void pvSetInt(int pv x, int v)
{
    x = v;
    %%testDiag("pvSetInt(): seqg_chid_x=%d, v=%d", seqg_chid_x, v);
    pvPut(x);
}

void pvSetIntP(int pv *p, int v)
{
    *p = v;
#if 0
    %%testDiag("pvSetIntP(): *p=%d, *seqg_chid_p=%d, v=%d", *p, *seqg_chid_p, v);
#endif
    pvPut(*p);
}

void pvSetInts(int pv *x, int n, int v)
{
    int i;
    for (i=0; i<n; i++) {
#if 0
	%%testDiag("pvSetInts(): i=%d, x[i]=%d, seqg_chid_x[i]=%d, v=%d", i, x[i], seqg_chid_x[i], v);
        pvSetInt(x[i], v);
#endif
#if 0
	%%testDiag("pvSetInts(): i=%d, x[i]=%d, seqg_chid_x[i]=%d, v=%d", i, x[i], seqg_chid_x[i], v);
#endif
        pvSetIntP(x+i, v);
    }
}

#if 0
void pv *pvPlusAddr(void pv *p, int n)
{
    return p + n;
}
#endif
