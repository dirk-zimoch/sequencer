program pvInStructTest

option +s;

%%#include "../testSupport.h"

struct test {
    double pv d;
    int pv i[2];
    float pv f;
};

struct test x;

assign x to {};
monitor x;

#if 0
monitor x.d;
monitor x.i[0];
monitor x.i[1];
#endif

entry {
    seq_test_init(14);
    testDiag("start");
}

ss put {
    state test {
        when (delay(.2)) {
            x.d = 1.0;
            testOk1(pvPut(x.d)==pvStatOK);
            x.i[0] = 2;
            testOk1(pvPut(x.i[0], SYNC)==pvStatOK);
            x.i[1] = 3;
            testOk1(pvPut(x.i[1], SYNC)==pvStatOK);
            test4(&x);
        } state done
    }
    state done {
        when (delay(1)) {
            testFail("timeout");
        } exit
    }
}

ss get {
    state test1 {
        entry {
            testOk1(pvGet(x.d)==pvStatOK);
            testOk1(x.d==0.0);
            testOk1(pvGet(x.i[0])==pvStatOK);
            testOk1(x.i[0]==0);
            testOk1(pvGet(x.i[1])==pvStatOK);
            testOk1(x.i[1]==0);
        }
        when (x.d) {
            testOk1(x.d==1.0);
        } state test2
    }
    state test2 {
        when (x.i[0]) {
            testOk1(x.i[0]==2);
        } state test3
    }
    state test3 {
        when (x.i[1]) {
            testOk1(x.i[1]==3);
        } state test4
    }
    state test4 {
        when (x.f > 3.0) {
            testOk1(x.f>3.99 && x.f<4.01);
        } state done
    }
    state done {
        when () {} exit
    }
}

exit {
    testDiag("exit");
    seq_test_done();
}

void test4(struct test *pt)
{
    pt->f = 4;
    testOk1(pvPut(pt->f)==pvStatOK);
}
