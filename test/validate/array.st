/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program arrayTest

%%#include "../testSupport.h"

#define H 2
#define W 3

entry {
    seq_test_init(H*W);
}

ss array {
    string aas[H][W] = {
        { "00", "01", "02" },
        { "10", "11", "12" }
    };
    assign aas to {};
    state init {
        string (*aps[H])[W] = {
            &aas[0],
            &aas[1]
        };
        when () {
            int i, j;
            for (i = 0; i < H; i++) {
                for (j = 0; j < W; j++) {
                    testOk(aas[i][j] == (*aps[i])[j],
                        "aas[%d][%d]=%s==%s=(*aps[%d])[%d])",
                        i, j, aas[i][j], (*aps[i])[j], i, j);
                }
            }
            for (i = 0; i < H; i++) {
                testDiag("aps[%d]=%p", i, aps[i]);
            }
        } exit
    }
}

exit {
    seq_test_done();
}
