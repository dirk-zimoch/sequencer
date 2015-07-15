/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program channelIndirectionTest

option +s;

%%#include "../testSupport.h"
%%#include "epicsTime.h"

#define MAX_TEST 2

int pv x = pv ("", monitor);

entry {
    seq_test_init(MAX_TEST);
}

ss main {
    double nom_delay = 1.0;
    typename epicsTimeStamp before, after;
    int pv *xp;

    state zero {
#if 0
        when () {
            wait {
                when (delay(nom_delay)) {
                    testDiag("delay has expired, x = %d", x);
                }
                when (x) {
                    testDiag("monitor event on pv x");
                }
            }

            epicsTimeGetCurrent(&before);
            delay_task(nom_delay, x);
            epicsTimeGetCurrent(&after);
            real_delay = epicsTimeDiffInSeconds(&after,&before);
            testOk(real_delay < nom_delay, "real_delay(=%f) < nom_delay(=%f)", real_delay, nom_delay);
        } exit
#endif

        entry {
            epicsTimeGetCurrent(&before);
            xp = &x;
        }

        when (delay(nom_delay)) {
            double real_delay;
            typename epicsTimeStamp after;

            testDiag("delay has expired, x = %d", x);
            epicsTimeGetCurrent(&after);
            real_delay = epicsTimeDiffInSeconds(&after,&before);
            testOk(real_delay >= nom_delay, "real_delay(=%f) < nom_delay(=%f)", real_delay, nom_delay);
        } state zero

        when (*xp) {
            double real_delay;
            typename epicsTimeStamp after;

            testDiag("monitor event on pv parameter x");
            epicsTimeGetCurrent(&after);
            real_delay = epicsTimeDiffInSeconds(&after,&before);
            testOk(real_delay < nom_delay, "real_delay(=%f) < nom_delay(=%f)", real_delay, nom_delay);
        } exit
    }
}

ss help {
    int xx = 0;
    state zero {
        when(delay(0.7)) {
            xx++;
            x = xx;
            pvPut(x);
        } state zero
    }
}

exit {
    seq_test_done();
}
