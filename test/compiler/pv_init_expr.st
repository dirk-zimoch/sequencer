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

evflag ef;

struct mixing_pv_and_plain_in_struct with_pv_args = {
    42,
    pv ("{DEV}:stat", monitor),
    pv ({ "{DEV}:readback1", "{DEV}:readback2", "{DEV}:readback3" }, sync ef, syncq 5)
};

struct mixing_pv_and_plain_in_struct with_pv_args2 = {
    42,
    pv ("{DEV}:stat"),
    pv { ("{DEV}:readback1", syncq 7), ("{DEV}:readback2",monitor), "{DEV}:readback3" }
};

#include "../simple.st"
