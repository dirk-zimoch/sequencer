/*************************************************************************\
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
program exOpt_DuplOptTest
float v;
assign v;
monitor v;

ss ss1 {
    state low {
        option + t;
        entry {
            float x = DEFAULT;
            printf("v = %f, x = %f\n", v, x);
        }
        when(v > 5.0) {
            printf("now changing to high\n");
        } state high
        when(delay(.1)) {
        } state low
        exit {
            printf("Something to do on exit");
        }
    } state high {
        option - t;
        option + t;
        when(v <= 5.0) {
            printf("changing to low\n");
        } state low
        when(delay(.1)) {
        } state high
    }
}
