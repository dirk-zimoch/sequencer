/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program efGlobalTest

ss x {
    evflag a; /* ok */
    state y {
        evflag b; /* ok */
        when () {
            evflag c; /* error: declaring channels or event flags is not allowed here */
        } exit
    }
}
