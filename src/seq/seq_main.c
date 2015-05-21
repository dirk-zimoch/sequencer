/*************************************************************************\
Copyright (c) 1990-1994 The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
#define prim_types_GLOBAL
#include "seq.h"
#include "seq_debug.h"

static boolean init_sprog(PROG *sp, seqProgram *seqProg);
static boolean init_sscb(PROG *sp, SSCB *ss, seqSS *seqSS);

/*
 * seq: Run a state program.
 * Usage:  seq(<sp>, <macros string>, <stack size>)
 *	sp is the ptr to the state program structure.
 *	Example:  seq(&myprog, "logfile=mylog", 0)
 * When called from the shell, the 2nd & 3rd parameters are optional.
 *
 * Creates the initial state program thread and returns its thread id.
 * Most initialization is performed here.
 */
epicsShareFunc epicsThreadId epicsShareAPI seq(
	seqProgram *seqProg, const char *macroDef, unsigned stackSize)
{
	epicsThreadId	tid;
	PROG		*sp;
	char		*str;
	const char	*threadName;
	unsigned int	smallStack;

	/* Register this program (if not yet done) */
	seqRegisterSequencerProgram(seqProg);

	/* Print version & date of sequencer */
	errlogSevPrintf(errlogInfo, SEQ_RELEASE "\n");

	/* Exit if no parameters specified */
	if (!seqProg)
	{
		errlogSevPrintf(errlogFatal, "seq: bad first argument seqProg (is NULL)\n");
		return 0;
	}

	/* Check for correct state program format */
	if (seqProg->magic != MAGIC)
	{
		errlogSevPrintf(errlogFatal, "seq: illegal magic number in state program.\n"
			"      - probable mismatch between SNC & SEQ versions\n"
			"      - re-compile your program?\n");
		return 0;
	}

	sp = new(PROG);
	if (!sp)
	{
		errlogSevPrintf(errlogFatal, "seq: calloc failed\n");
		return 0;
	}

	/* Parse the macro definitions from the "program" statement */
	seqMacParse(sp, seqProg->params);

	/* Parse the macro definitions from the command line */
	seqMacParse(sp, macroDef);

	/* Initialize program struct */
	if (!init_sprog(sp, seqProg))
		return 0;

	/* Specify stack size */
	if (stackSize == 0)
		stackSize = epicsThreadGetStackSize(THREAD_STACK_SIZE);
	str = seqMacValGet(sp, "stack");
	if (str && str[0] != '\0')
	{
		sscanf(str, "%ud", &stackSize);
	}
	smallStack = epicsThreadGetStackSize(epicsThreadStackSmall);
	if (stackSize < smallStack)
		stackSize = smallStack;
	sp->stackSize = stackSize;

	/* Specify thread name */
	str = seqMacValGet(sp, "name");
	if (str && str[0] != '\0')
		threadName = str;
	else
		threadName = sp->progName;

	/* Specify thread priority */
	sp->threadPriority = THREAD_PRIORITY;
	str = seqMacValGet(sp, "priority");
	if (str && str[0] != '\0')
	{
		sscanf(str, "%ud", &(sp->threadPriority));
	}
	if (sp->threadPriority > THREAD_PRIORITY)
		sp->threadPriority = THREAD_PRIORITY;

	tid = epicsThreadCreate(threadName, sp->threadPriority,
		sp->stackSize, sequencer, sp);
	if (!tid)
	{
		errlogSevPrintf(errlogFatal, "seq: epicsThreadCreate failed");
		return 0;
	}

	errlogSevPrintf(errlogInfo,
		"Spawning sequencer program \"%s\", thread %p: \"%s\"\n",
		sp->progName, tid, threadName);

	return tid;
}

/*
 * Copy data from seqCom.h structures into this thread's dynamic structures
 * as defined in seq.h.
 */
static boolean init_sprog(PROG *sp, seqProgram *seqProg)
{
	unsigned nss, nch, nef;

	/* Copy information for state program */
	sp->numSS = seqProg->numSS;
	sp->numChans = seqProg->numChans;
	sp->numEvFlags = seqProg->numEvFlags;
	sp->options = seqProg->options;
	sp->progName = seqProg->progName;
	sp->initFunc = seqProg->initFunc;
	sp->entryFunc = seqProg->entryFunc;
	sp->exitFunc = seqProg->exitFunc;
	sp->varSize = seqProg->varSize;
	sp->numQueues = seqProg->numQueues;

	/* Allocate user variable area if reentrant option (+r) is set */
	if (optTest(sp, OPT_REENT) && sp->varSize > 0)
	{
		sp->var = (SEQ_VARS *)newArray(char, sp->varSize);
		if (!sp->var)
		{
			errlogSevPrintf(errlogFatal, "init_sprog: calloc failed\n");
			return FALSE;
		}
	}

	DEBUG("init_sprog: numSS=%d, numChans=%d, numEvFlags=%u, "
		"progName=%s, varSize=%u\n", sp->numSS, sp->numChans,
		sp->numEvFlags, sp->progName, sp->varSize);

	/* Create semaphores */
	sp->lock = epicsMutexCreate();
	if (!sp->lock)
	{
		errlogSevPrintf(errlogFatal, "init_sprog: epicsMutexCreate failed\n");
		return FALSE;
	}
	sp->ready = epicsEventCreate(epicsEventEmpty);
	if (!sp->ready)
	{
		errlogSevPrintf(errlogFatal, "init_sprog: epicsEventCreate failed\n");
		return FALSE;
	}

	/* Allocate an array for event flag bits. Note this does
	   *not* reserve space for all event numbers (i.e. including
	   channels), only for event flags. The + 1 is for the zero dummy event. */
	assert(NWORDS(sp->numEvFlags) > 0);
	sp->events = newArray(bitMask, NWORDS(sp->numEvFlags + 1));
	if (!sp->events)
	{
		errlogSevPrintf(errlogFatal, "init_sprog: calloc failed\n");
		return FALSE;
	}
        /* Allocate the array of event flag structures */
	sp->eventFlags = newArray(EVFLAG, sp->numEvFlags + 1);

	/* Allocate and initialize syncQ queues */
	if (sp->numQueues > 0)
	{
		sp->queues = newArray(QUEUE, sp->numQueues);
		if (!sp->queues)
		{
			errlogSevPrintf(errlogFatal, "init_sprog: calloc failed\n");
			return FALSE;
		}
	}
	/* Initial pool for pv requests is 1kB on 32-bit systems */
	freeListInitPvt(&sp->pvReqPool, 128, sizeof(PVREQ));
	if (!sp->pvReqPool)
	{
		errlogSevPrintf(errlogFatal, "init_sprog: freeListInitPvt failed\n");
		return FALSE;
	}

	/* Allocate array of state set structs and initialize it */
	if (sp->numSS > 0)
	{
		sp->ss = newArray(SSCB, sp->numSS);
		if (!sp->ss)
		{
			errlogSevPrintf(errlogFatal, "init_sprog: calloc failed\n");
			return FALSE;
		}
	}
	for (nss = 0; nss < sp->numSS; nss++)
	{
		if (!init_sscb(sp, sp->ss + nss, seqProg->ss + nss))
			return FALSE;
	}

	/* Allocate array of channel structs */
	if (sp->numChans > 0)
	{
		sp->chan = newArray(CHAN, sp->numChans);
		if (!sp->chan)
		{
			errlogSevPrintf(errlogFatal, "init_sprog: calloc failed\n");
			return FALSE;
		}
	}
	for (nef = 0; nef < sp->numEvFlags; nef++)
	{
		seqEvFlag *pef = seqProg->evFlags + nef;
		*(evflag*)((char*)sp->var + pef->offset) = seq_efCreate(sp, nef+1, pef->initVal);
	}
	for (nch = 0; nch < sp->numChans; nch++)
	{
		seqChan *pch = seqProg->chans + nch;
		CH_ID ch = seq_pvCreate(sp,
			nch,
			pch->chName,
			pch->valOffset,
			pch->expr,
			pch->type,
			pch->count,
			pch->monMask,
			pch->efNum,
			pch->queueSize,
			pch->queueIndex);
		*(CH_ID*)((char*)sp->var + pch->chOffset) = ch;
	}
	return TRUE;
}

/*
 * Initialize a state set control block
 */
static boolean init_sscb(PROG *sp, SSCB *ss, seqSS *seqSS)
{
	/* Fill in SSCB */
	ss->ssName = seqSS->ssName;
	ss->numStates = seqSS->numStates;

	ss->currentState = 0; /* initial state */
	ss->nextState = 0;
	ss->prevState = 0;
	ss->threadId = 0;
	ss->timeEntered = epicsINF;
	ss->wakeupTime = epicsINF;
	ss->prog = sp;

	ss->syncSem = epicsEventCreate(epicsEventEmpty);
	if (!ss->syncSem)
	{
		errlogSevPrintf(errlogFatal, "init_sscb: epicsEventCreate failed\n");
		return FALSE;
	}

	if (sp->numChans > 0)
	{
		ss->getReq = newArray(PVREQ*, sp->numChans);
		if (!ss->getReq)
		{
			errlogSevPrintf(errlogFatal, "init_sscb: calloc failed\n");
			return FALSE;
		}
		ss->putReq = newArray(PVREQ*, sp->numChans);
		if (!ss->putReq)
		{
			errlogSevPrintf(errlogFatal, "init_sscb: calloc failed\n");
			return FALSE;
		}
		if (optTest(sp, OPT_SAFE))
		{
			ss->metaData = newArray(PVMETA, sp->numChans);
			if (!ss->metaData)
			{
				errlogSevPrintf(errlogFatal, "init_ss: calloc failed\n");
				return FALSE;
			}
		}
		ss->monitored = newArray(boolean, sp->numChans);
		if (!ss->monitored)
		{
			errlogSevPrintf(errlogFatal, "init_sscb: calloc failed\n");
			return FALSE;
		}
	}
	/* note: do not pre-allocate request structures */
	ss->dead = epicsEventCreate(epicsEventEmpty);
	if (!ss->dead)
	{
		errlogSevPrintf(errlogFatal, "init_sscb: epicsEventCreate failed\n");
		return FALSE;
	}

	/* No need to copy the state structs, they can be shared
	   because nothing gets mutated. */
	ss->states = seqSS->states;

	/* Allocate separate user variable area if safe mode option (+s) is set */
	if (optTest(sp, OPT_SAFE))
	{
		if (sp->numChans > 0)
		{
			ss->dirty = newArray(boolean, sp->numChans);
			if (!ss->dirty)
			{
				errlogSevPrintf(errlogFatal, "init_sscb: calloc failed\n");
				return FALSE;
			}
		}
		if (sp->varSize > 0)
		{
			ss->var = (SEQ_VARS *)newArray(char, sp->varSize);
			if (!ss->var)
			{
				errlogSevPrintf(errlogFatal, "init_sscb: calloc failed\n");
				return FALSE;
			}
		}
	}
	else
	{
		ss->dirty = NULL;
		ss->var = sp->var;
	}
	return TRUE;
}

/* Free all allocated memory in a program structure */
void seq_free(PROG *sp)
{
	unsigned nss, nch, nq;

	/* Delete state sets */
	for (nss = 0; nss < sp->numSS; nss++)
	{
		SSCB *ss = sp->ss + nss;

		epicsEventDestroy(ss->syncSem);
		free(ss->metaData);
		free(ss->monitored);

		epicsEventDestroy(ss->dead);

		if (optTest(sp, OPT_SAFE)) free(ss->dirty);
		if (optTest(sp, OPT_SAFE)) free(ss->var);
	}

	free(sp->ss);

	/* Delete program-wide semaphores */
	epicsMutexDestroy(sp->lock);
	epicsEventDestroy(sp->ready);

	seqMacFree(sp);

	for (nch = 0; nch < sp->numChans; nch++)
	{
		CHAN *ch = sp->chan + nch;

		if (ch->dbch)
		{
			free(ch->dbch->dbName);
			free(ch->dbch);
		}
		epicsMutexDestroy(ch->varLock);
	}
	free(sp->chan);
	free(sp->eventFlags);

	for (nq = 0; nq < sp->numQueues; nq++)
		seqQueueDestroy(sp->queues[nq]);
	free(sp->queues);

	free(sp->events);
	if (optTest(sp, OPT_REENT)) free(sp->var);
	free(sp);
}
