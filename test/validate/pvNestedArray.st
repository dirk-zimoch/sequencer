program pvNestedArrayTest

option +s; 

%%#include "../testSupport.h"

#define I 2
#define J 3
#define K 4

int pv a[I][J][K];

evflag done = 0;

entry {
    seq_test_init(I*J*K);
}

ss set {
    state set {
        when () {
            setf(a);
            efSet(done);
        } state done
    }
    state done {
        when (delay(1)) {
            testFail("timeout waiting for ss test to finish");
        } exit
    }
}

ss test {
    monitor a;
    state test {
        when (efTest(done)) {
            testf(a);
        } exit
    }
}

exit {
    seq_test_done();
}

int value(int i, int j, int k)
{
    return (i*J+j)*K+k;
}

void setf(int pv (*a)[J][K])
{
    int i, j, k;
    for(i=0; i<I; i++)
        for(j=0; j<J; j++)
            for(k=0; k<K; k++) {
                a[i][j][k] = value(i,j,k);
                pvPut(a[i][j][k]);
            }
}

void testf(int pv (*a)[J][K])
{
    int i, j, k;
    for(i=0; i<I; i++)
        for(j=0; j<J; j++)
            for(k=0; k<K; k++) {
                testOk(a[i][j][k] == value(i,j,k), "%d != %d", a[i][j][k], value(i,j,k));
            }
}
