program subscript

float single[2];
assign single;                  /* one single channel, count = 2 */

float multiple3[3];
assign multiple3 to {"x", "y"}; /* should be 3 channels, last one assigned to "" */

int multiple2[2];
assign multiple2 to {};         /* should be 2 channels, all assigned to "" */

monitor multiple2;              /* should all have monitored flag */

ss test {
    state test {
        when (FALSE) {
            %%multiple2[0] = 0;
        } state test
    }
}