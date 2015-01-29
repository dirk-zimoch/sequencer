/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program pass_pv_type

float pv f;

void mypvfun(int pv x)
{
    printf("%d\n", x);
}

void myptrfun(int *x)
{
    printf("%d\n", *x);
}

entry {
    mypvfun(f);
    myptrfun(&f);
}

#include "../simple.st"
