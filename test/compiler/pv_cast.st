/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program pv_cast

option +r;

int i;
float pv fch;
int pv ich;

entry {
    fch = (float pv)ich;
    ich = (int pv)i;
}

#include "../simple.st"
