/*************************************************************************\
Copyright (c) 2010-2013 Helmholtz-Zentrum Berlin f. Materialien
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
	{ P_CHAR,	pvTypeCHAR,	pvTypeTIME_CHAR,	sizeof(char)		},
	{ P_UCHAR,	pvTypeCHAR,	pvTypeTIME_CHAR,	sizeof(unsigned char)	},
	{ P_SHORT,	pvTypeSHORT,	pvTypeTIME_SHORT,	sizeof(short)		},
	{ P_USHORT,	pvTypeSHORT,	pvTypeTIME_SHORT,	sizeof(unsigned short)	},
	{ P_INT,	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(int)		},
	{ P_UINT,	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(unsigned int)	},
	{ P_LONG,	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(long)		},
	{ P_ULONG,	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(unsigned long)	},
	{ P_INT8T,	pvTypeCHAR,	pvTypeTIME_CHAR,	sizeof(epicsInt8)	},
	{ P_UINT8T,	pvTypeCHAR,	pvTypeTIME_CHAR,	sizeof(epicsUInt8)	},
	{ P_INT16T,	pvTypeSHORT,	pvTypeTIME_SHORT,	sizeof(epicsInt16)	},
	{ P_UINT16T,	pvTypeSHORT,	pvTypeTIME_SHORT,	sizeof(epicsUInt16)	},
	{ P_INT32T,	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(epicsInt32)	},
	{ P_UINT32T,	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(epicsUInt32)	},
	{ P_FLOAT,	pvTypeFLOAT,	pvTypeTIME_FLOAT,	sizeof(float)		},
	{ P_DOUBLE,	pvTypeDOUBLE,	pvTypeTIME_DOUBLE,	sizeof(double)		},
	{ P_STRING,	pvTypeSTRING,	pvTypeTIME_STRING,	sizeof(string)		},
};

epicsShareFunc CH_ID seq_pvCreate(
    PROG        *sp,            /* program instance */
    unsigned    chNum,          /* index of channel */
    const char  *chName,        /* assigned channel name */
    size_t      offset,         /* offset to value */
    const char  *varName,       /* variable name, including subscripts*/
    enum prim_type_tag varType, /* variable (base) type */
    unsigned    count,          /* element count for arrays */
    evflag      ef,             /* event flag if synced */
    seqBool     monitored,      /* whether channel should be monitored */
    unsigned    queueSize,      /* syncQ queue size (0=not queued) */
    unsigned    queueIndex)     /* syncQ queue index */
{
    CH_ID ch = sp->chan + chNum;

    assert(chNum < sp->numChans);
    ch->prog = sp;
    ch->varName = varName;
    ch->offset = offset;
    ch->count = count;
    if (ch->count == 0)
        ch->count = 1;
    ch->syncedTo = ef;
    if (ch->syncedTo) {
        bitSet(ef->synced, chNum);
    }
    ch->monitored = monitored;
    ch->eventNum = sp->numEvFlags + chNum + 1;

    /* Fill in request type info */
    ch->type = pv_type_map + varType;

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
            if (ch->monitored)
                sp->monitorCount++;
        }
    }

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
