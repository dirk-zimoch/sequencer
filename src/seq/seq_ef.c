#include "seq.h"
#include "seq_debug.h"

/*
 * Create an event flag.
 */
evflag seq_efCreate(PROG_ID sp, unsigned ef_num, unsigned val)
{
    evflag r = sp->eventFlags + ef_num;

    assert(ef_num > 0 && ef_num <= sp->numEvFlags);
    epicsMutexMustLock(sp->lock);
    if (val)
        bitSet(sp->events, ef_num);
    else
        bitClear(sp->events, ef_num);
    epicsMutexUnlock(sp->lock);
    /* allocate the set of channel numbers synced to this event flag */
    r->synced = newArray(bitMask, NWORDS(sp->numChans));
    DEBUG("efCreate returns %p\n", r);
    return r;
}

/*
 * Synchronize pv with an event flag.
 * ev_flag == 0 means unSync.
 */
epicsShareFunc void seq_pvSync(SS_ID ss, CH_ID ch, evflag new_ev_flag)
{
	seq_pvArraySync(ss, &ch, 1, new_ev_flag);
}

/*
 * Array variant of seq_pvSync.
 */
epicsShareFunc void seq_pvArraySync(SS_ID ss, CH_ID *chs, unsigned length, evflag new_ev_flag)
{
    PROG *sp = ss->prog;
    unsigned n;

    DEBUG("pvSync: new_ev_flag=%p\n", new_ev_flag);
    epicsMutexMustLock(sp->lock);
    for (n = 0; n < length; n++) {
        CH_ID ch = chs[n];
        unsigned nch = ch - sp->chan;

        if (ch->syncedTo != new_ev_flag) {
            if (ch->syncedTo) {
                bitClear(ch->syncedTo->synced, nch);
            }
            if (new_ev_flag) {
                bitSet(new_ev_flag->synced, nch);
            }
            ch->syncedTo = new_ev_flag;
            DEBUG("pvSync: syncedTo=%p\n", ch->syncedTo);
        }
    }
    epicsMutexUnlock(sp->lock);
}

/*
 * Set an event flag, then wake up each state
 * set that might be waiting on that event flag.
 */
epicsShareFunc void seq_efSet(SS_ID ss, evflag ev_flag)
{
    PROG *sp = ss->prog;
    unsigned efNum = ev_flag - sp->eventFlags;

    assert(efNum > 0 && efNum <= sp->numEvFlags);
    epicsMutexMustLock(sp->lock);
    bitSet(sp->events, efNum);
    ss_wakeup(sp, efNum);
    epicsMutexUnlock(sp->lock);
}

/*
 * Return whether event flag is set.
 */
epicsShareFunc boolean seq_efTest(SS_ID ss, evflag ev_flag)
/* event flag */
{
    PROG *sp = ss->prog;
    boolean isSet;
    unsigned efNum = ev_flag - sp->eventFlags;

    assert(efNum > 0 && efNum <= sp->numEvFlags);
    epicsMutexMustLock(sp->lock);
    isSet = bitTest(sp->events, efNum);
    if (isSet && optTest(sp, OPT_SAFE))
        ss_read_buffer_selective(sp, ss, ev_flag);
    epicsMutexUnlock(sp->lock);
    return isSet;
}

/*
 * Clear event flag.
 */
epicsShareFunc boolean seq_efClear(SS_ID ss, evflag ev_flag)
{
    PROG *sp = ss->prog;
    boolean isSet;
    unsigned efNum = ev_flag - sp->eventFlags;

    assert(efNum > 0 && efNum <= sp->numEvFlags);
    epicsMutexMustLock(sp->lock);
    isSet = bitTest(sp->events, efNum);
    bitClear(sp->events, efNum);
    ss_wakeup(sp, efNum);
    epicsMutexUnlock(sp->lock);
    return isSet;
}

/*
 * Clear event flag and return whether it was set.
 */
epicsShareFunc boolean seq_efTestAndClear(SS_ID ss, evflag ev_flag)
{
    PROG *sp = ss->prog;
    boolean isSet;
    unsigned efNum = ev_flag - sp->eventFlags;

    assert(efNum > 0 && efNum <= sp->numEvFlags);
    epicsMutexMustLock(sp->lock);
    isSet = bitTest(sp->events, efNum);
    bitClear(sp->events, efNum);
    if (isSet && optTest(sp, OPT_SAFE))
        ss_read_buffer_selective(sp, ss, ev_flag);
    epicsMutexUnlock(sp->lock);
    return isSet;
}
