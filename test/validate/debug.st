program debugTest

%%#include "../testSupport.h"

option +s;

int x;
assign x;
monitor x;

#define NTESTS 5

entry {
    seq_test_init(NTESTS);
}

ss write {
    state write {
        when (delay(0.5)) {
            x++;
            if (x == NTESTS) state done;
            pvPut(x);
            testDiag("wrote x=%d",x);
        } state write
    }
    state done {
        when () {
        } exit
    }
}

ss read {
    int old_x = 0;
    state check {
        entry {
            testPass("enter state check, x=%d, old_x=%d", x, old_x);
        }
        when (x != old_x) {
            old_x = x;
            testPass("x=%d", x);
        } state check
    }
}

exit {
    seq_test_done();
}
