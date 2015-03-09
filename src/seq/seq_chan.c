/*************************************************************************\
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#include "seq_debug.h"
#include "seq.h"

/*
 * types for DB put/get, element size based on user variable type.
 * pvTypeTIME_* types for gets/monitors return status, severity, and time stamp
 * in addition to the value.
 */
static PVTYPE pv_type_map[] =
{
    /* tag          putType         getType             size                    */
    { P_CHAR,       pvTypeCHAR,     pvTypeTIME_CHAR,    sizeof(char)            },
    { P_UCHAR,      pvTypeCHAR,     pvTypeTIME_CHAR,    sizeof(unsigned char)   },
    { P_SHORT,      pvTypeSHORT,    pvTypeTIME_SHORT,   sizeof(short)           },
    { P_USHORT,     pvTypeSHORT,    pvTypeTIME_SHORT,   sizeof(unsigned short)  },
    { P_INT,        pvTypeLONG,     pvTypeTIME_LONG,    sizeof(int)             },
    { P_UINT,       pvTypeLONG,     pvTypeTIME_LONG,    sizeof(unsigned int)    },
    { P_LONG,       pvTypeLONG,     pvTypeTIME_LONG,    sizeof(long)            },
    { P_ULONG,      pvTypeLONG,     pvTypeTIME_LONG,    sizeof(unsigned long)   },
    { P_INT8T,      pvTypeCHAR,     pvTypeTIME_CHAR,    sizeof(epicsInt8)       },
    { P_UINT8T,     pvTypeCHAR,     pvTypeTIME_CHAR,    sizeof(epicsUInt8)      },
    { P_INT16T,     pvTypeSHORT,    pvTypeTIME_SHORT,   sizeof(epicsInt16)      },
    { P_UINT16T,    pvTypeSHORT,    pvTypeTIME_SHORT,   sizeof(epicsUInt16)     },
    { P_INT32T,     pvTypeLONG,     pvTypeTIME_LONG,    sizeof(epicsInt32)      },
    { P_UINT32T,    pvTypeLONG,     pvTypeTIME_LONG,    sizeof(epicsUInt32)     },
    { P_FLOAT,      pvTypeFLOAT,    pvTypeTIME_FLOAT,   sizeof(float)           },
    { P_DOUBLE,     pvTypeDOUBLE,   pvTypeTIME_DOUBLE,  sizeof(double)          },
    { P_STRING,     pvTypeSTRING,   pvTypeTIME_STRING,  sizeof(string)          },
};

static void seq_pvAddMonitor(
    PROG        *sp,            /* program instance */
    CH_ID       ch,             /* channel object */
    unsigned    ssNum)          /* state set number */
{
    assert(ssNum < sp->numSS);
    sp->ss[ssNum].monitored[ch - sp->chan] = TRUE;
    if (ch->dbch && !ch->dbch->monitored) {
        ch->dbch->monitored = TRUE;
        sp->monitorCount++;
    }
}

CH_ID seq_pvCreate(
    PROG            *sp,            /* program instance */
    unsigned        chNum,          /* index of channel */
    const char      *chName,        /* assigned channel name */
    size_t          offset,         /* offset to value */
    const char      *varName,       /* variable name, including subscripts*/
    PTYPE           varType,        /* variable (base) type */
    unsigned        count,          /* element count for arrays */
    const seqMask   *monMask,       /* set of state sets with a monitor */
    unsigned        efNum,          /* event flag number if synced */
    unsigned        queueSize,      /* queue size (0=not queued) */
    unsigned        queueIndex)     /* queue index */
{
    unsigned nss;
    CH_ID ch = sp->chan + chNum;

    assert(chNum < sp->numChans);
    ch->prog = sp;
    ch->varName = varName;
    ch->offset = offset;
    ch->count = count;
    if (ch->count == 0)
        ch->count = 1;

    if (efNum)
        ch->syncedTo = sp->eventFlags + efNum;
    else
        ch->syncedTo = 0;
    if (ch->syncedTo) {
        bitSet(ch->syncedTo->synced, chNum);
    }
    ch->eventNum = sp->numEvFlags + chNum + 1;

    /* Fill in request type info */
    ch->type = pv_type_map + varType;
    assert(ch->type->tag == varType);

    if (chName) {              /* skip anonymous PVs */
        char name_buffer[100];

        seqMacEval(sp, chName, name_buffer, sizeof(name_buffer));
        if (name_buffer[0]) {           /* skip anonymous PVs */
            DBCHAN *dbch = new(DBCHAN);
            if (!dbch) {
                errlogSevPrintf(errlogFatal, "init_chan: calloc failed\n");
                return NULL;
            }
            dbch->dbName = epicsStrDup(name_buffer);
            if (!dbch->dbName) {
                errlogSevPrintf(errlogFatal,
                    "init_chan: epicsStrDup failed\n");
                return NULL;
            }
            ch->dbch = dbch;
            sp->assignCount++;
        }
    }

    for (nss = 0; nss < sp->numSS; nss++)
        if (bitTest(monMask,nss))
            seq_pvAddMonitor(sp, ch, nss);

    if (queueSize) {
        /* We want to store the whole pv message in the queue,
           so that we can extract status etc when we remove
           the message. */
        size_t size = pv_size_n(ch->type->getType, ch->count);
        QUEUE *q = sp->queues + queueIndex;

        if (*q == NULL) {
            *q = seqQueueCreate(queueSize, size);
            if (!*q) {
                errlogSevPrintf(errlogFatal,
                    "init_chan: seqQueueCreate failed\n");
                return NULL;
            }
        } else if (seqQueueNumElems(*q) != queueSize ||
            seqQueueElemSize(*q) != size) {
            errlogSevPrintf(errlogFatal,
                "init_chan(varname=%s): inconsistent shared queue definitions\n",
                varName);
            return NULL;
        }
        ch->queue = *q;
    }
    ch->varLock = epicsMutexCreate();
    if (!ch->varLock) {
        errlogSevPrintf(errlogFatal, "init_chan: epicsMutexCreate failed\n");
        return NULL;
    }
    return ch;
}

epicsShareFunc size_t seq_pvOffset(CH_ID ch)
{
    return ch->offset;
}
