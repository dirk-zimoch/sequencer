program pv_types

#include "prelude.stm"

option +r;

int pv pv_i;
int pv *p_pv_i;

void pvSetInt(int pv x, int v)
{
    pvPut(x);
}



entry {
    pvSetInt(pv_i, 3);
}

#include "../simple.st"
