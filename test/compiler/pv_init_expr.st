program pv_init_expr

struct mixing_pv_and_plain_in_struct {
    int plain_int;
    int pv pv_of_int;
    double pv array_of_3_pv_of_double[3];
};

struct mixing_pv_and_plain_in_struct contrived = {
    42,
    pv "{DEV}:stat",
    pv { "{DEV}:readback1", "{DEV}:readback2", "{DEV}:readback3" }
};

#include "../simple.st"
