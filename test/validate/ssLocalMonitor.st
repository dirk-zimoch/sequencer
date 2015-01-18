program ssLocalMonitorTest

%%#include "../testSupport.h"

option +s;

int pv x;

evflag e[3];

entry {
    seq_test_init(0);
    x = 0;
}

ss write {
    state init {
        when (delay(0.1)) {
            pvGet(x);
        } state write
    }
    state write {
        when (delay(0.1)) {
            x++;
            testDiag("write x=%d", x);
            pvPut(x);
            efSet(e[0]);
            efSet(e[1]);
            efSet(e[2]);
            if (x==10)
                state done;
        } state write
    }
    state done {
        when (delay(0.1)) {
        } exit
    }
}

ss no_mon1 {
    state test {
        when (efTestAndClear(e[0])) {
            testOk(x==0, "no_mon1: x==%d", x);
        } state test
    }
}

ss no_mon2 {
    state test {
        when (efTestAndClear(e[1])) {
            testOk(x==0, "no_mon2: x==%d", x);
        } state test
    }
}

ss mon {
    monitor x;
    int y = 0;
    state test {
        when (efTestAndClear(e[2])) {
            y++;
            testOk(x==y, "mon: x=%d==y=%d", x, y);
        } state test
    }
}

exit {
    seq_test_done();
}
