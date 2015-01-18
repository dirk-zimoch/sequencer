program pv_and_assign_test

struct test {
    double pv m0;
    int pv m1[2];
    float m2;
};

struct test x = pv {};

struct test y[3];

assign y[0] to {"a"};
assign y[1] to {};
assign y[1].m0 to "b";
assign y to {{},{},{"",{"","c"}}};

/* warning: channel name changed from '' to 'd' */
/* warning: channel name changed from 'c' to 'e' */
assign y[2].m1 to {"d","e"};

monitor y;

#include "../simple.st"
