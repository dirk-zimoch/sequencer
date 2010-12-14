/**************************************************************************
			GTA PROJECT   AT division
	Copyright, 1990-1994, The Regents of the University of California
	and the University of Chicago.
	Los Alamos National Laboratory

	Copyright, 2010, Helmholtz-Zentrum Berlin f. Materialien
		und Energie GmbH, Germany (HZB)
		(see file Copyright.HZB included in this distribution)
***************************************************************************/
#include "seq.h"

/* #define DEBUG printf */
#define DEBUG nothing

/* function prototypes for local routines */
static void seqInitTables(SPROG *, seqProgram *);
static void init_sprog(seqProgram *, SPROG *);
static void init_sscb(seqProgram *, SPROG *);
static void init_chan(seqProgram *, SPROG *);
static PVTYPE *find_type(const char *pUserType);

/*	Globals */

/*	Auxiliary sequencer thread id; used to share PV context. */
epicsThreadId seqAuxThreadId = (epicsThreadId) 0;

/*
 * seq: User-callable routine to run a state program.
 * Usage:  seq(<pSP>, <macros string>, <stack size>)
 *	pSP is the ptr to the state program structure.
 *	Example:  seq(&myprog, "logfile=mylog", 0)
 * When called from the shell, the 2nd & 3rd parameters are optional.
 *
 * Creates the initial state program thread and returns its thread id.
 * Most initialization is performed here.
 */
epicsShareFunc void epicsShareAPI seq(
	seqProgram *pSeqProg, const char *macroDef, unsigned stackSize)
{
	epicsThreadId	tid;
	SPROG		*pSP;
	char		*pValue;
	const char	*pThreadName;
	unsigned	smallStack;
	AUXARGS		auxArgs;

	/* Print version & date of sequencer */
	printf(SEQ_VERSION "\n");

	/* Exit if no parameters specified */
	if (pSeqProg == 0)
	{
		return;
	}

	/* Check for correct state program format */
	if (pSeqProg->magic != MAGIC)
	{	/* Oops */
		errlogPrintf("Illegal magic number in state program.\n");
		errlogPrintf(" - Possible mismatch between SNC & SEQ "
			"versions\n");
		errlogPrintf(" - Re-compile your program?\n");
		epicsThreadSleep( 1.0 );	/* let error messages get printed */
		return;
	}

	pSP = (SPROG *)calloc(1, sizeof (SPROG));

	/* Parse the macro definitions from the "program" statement */
	seqMacParse(pSP, pSeqProg->pParams);

	/* Parse the macro definitions from the command line */
	seqMacParse(pSP, macroDef);

	/* Initialize the sequencer tables */
	seqInitTables(pSP, pSeqProg);


	/* Specify stack size */
	if (stackSize == 0)
		stackSize = epicsThreadGetStackSize(THREAD_STACK_SIZE);
	pValue = seqMacValGet(pSP, "stack");
	if (pValue != NULL && strlen(pValue) > 0)
	{
		sscanf(pValue, "%ud", &stackSize);
	}
	smallStack = epicsThreadGetStackSize(epicsThreadStackSmall);
	if (stackSize < smallStack)
		stackSize = smallStack;
	pSP->stackSize = stackSize;

	/* Specify thread name */
	pValue = seqMacValGet(pSP, "name");
	if (pValue != NULL && strlen(pValue) > 0)
		pThreadName = pValue;
	else
		pThreadName = pSP->pProgName;

	/* Specify PV system name (defaults to CA) */
	pValue = seqMacValGet(pSP, "pvsys");
	if (pValue != NULL && strlen(pValue) > 0)
		auxArgs.pPvSysName = pValue;
	else
		auxArgs.pPvSysName = "ca";

	/* Determine debug level (currently only used for PV-level debugging) */
	pValue = seqMacValGet(pSP, "debug");
	if (pValue != NULL && strlen(pValue) > 0)
		auxArgs.debug = atol(pValue);
	else
		auxArgs.debug = 0;

	/* Spawn the sequencer auxiliary thread */
	if (seqAuxThreadId == (epicsThreadId) 0)
	{
		unsigned auxStack = epicsThreadGetStackSize(epicsThreadStackMedium);
		epicsThreadCreate("seqAux", THREAD_PRIORITY+1, auxStack,
				(EPICSTHREADFUNC)seqAuxThread, &auxArgs);
		while (seqAuxThreadId == (epicsThreadId) 0)
			/* wait for thread to init. message system */
			epicsThreadSleep(0.1);

		if (seqAuxThreadId == (epicsThreadId) -1)
		{
			epicsThreadSleep( 1.0 );	/* let error messages get printed */
			return;
		}
		DEBUG("thread seqAux spawned, tid=%p\n", seqAuxThreadId);
	}

	/* Spawn the initial sequencer thread */
	DEBUG("Spawning thread %s, stackSize=%d\n", pThreadName,
		pSP->stackSize);
	/* Specify thread priority */
	pSP->threadPriority = THREAD_PRIORITY;
	pValue = seqMacValGet(pSP, "priority");
	if (pValue != NULL && strlen(pValue) > 0)
	{
		sscanf(pValue, "%ud", &(pSP->threadPriority));
	}
	if (pSP->threadPriority > THREAD_PRIORITY)
		pSP->threadPriority = THREAD_PRIORITY;

	tid = epicsThreadCreate(pThreadName, pSP->threadPriority, pSP->stackSize,
		(EPICSTHREADFUNC)sequencer, pSP);

	printf("Spawning state program \"%s\", thread %p: \"%s\"\n",
		pSP->pProgName, tid, pThreadName);
}

/* seqInitTables - initialize sequencer tables */
static void seqInitTables(SPROG *pSP, seqProgram *pSeqProg)
{
	/* Initialize state program block */
	init_sprog(pSeqProg, pSP);

	/* Initialize state set control blocks */
	init_sscb(pSeqProg, pSP);

	/* Initialize database channel blocks */
	init_chan(pSeqProg, pSP);
}

/*
 * Copy data from seqCom.h structures into this thread's dynamic structures
 * as defined in seq.h.
 */
static void init_sprog(seqProgram *pSeqProg, SPROG *pSP)
{
	unsigned i, nWords;

	/* Copy information for state program */
	pSP->numSS = pSeqProg->numSS;
	pSP->numChans = pSeqProg->numChans;
	pSP->numEvents = pSeqProg->numEvents;
	pSP->options = pSeqProg->options;
	pSP->pProgName = pSeqProg->pProgName;
	pSP->initFunc = pSeqProg->initFunc;
	pSP->entryFunc = pSeqProg->entryFunc;
	pSP->exitFunc = pSeqProg->exitFunc;
	pSP->varSize = pSeqProg->varSize;
	/* Allocate user variable area if reentrant option (+r) is set */
	if (pSP->options & OPT_REENT)
	{
		pSP->pVar = (char *)calloc(pSP->varSize, 1);
	}

	DEBUG("init_sprog: num SS=%ld, num Chans=%ld, num Events=%ld, "
		"Prog Name=%s, var Size=%ld\n", pSP->numSS, pSP->numChans,
		pSP->numEvents, pSP->pProgName, pSP->varSize);

	/* Create a semaphore for resource locking on PV events */
	pSP->programLock = epicsMutexMustCreate();
	pSP->connectCount = 0;
	pSP->assignCount = 0;
	pSP->allDisconnected = TRUE;

	/* Allocate an array for event flag bits */
	nWords = (pSP->numEvents + NBITS - 1) / NBITS;
	if (nWords == 0)
		nWords = 1;
	pSP->pEvents = (bitMask *)calloc(nWords, sizeof(bitMask));

	/* Allocate and initialize syncQ queues */
	pSP->numQueues = pSeqProg->numQueues;
	pSP->pQueues = NULL;

	if (pSP->numQueues > 0 )
	{
		pSP->pQueues = (ELLLIST *)calloc(pSP->numQueues,
						 sizeof(ELLLIST));
		for (i = 0; i < pSP->numQueues; i++)
			ellInit(&pSP->pQueues[i]);
	}
	/* initial pool for pv requests is 1kB on 32-bit systems */
	freeListInitPvt(&pSP->pvReqPool, 128, sizeof(PVREQ));
}

/*
 * Initialize the state set control blocks
 */
static void init_sscb(seqProgram *pSeqProg, SPROG *pSP)
{
	SSCB		*pSS;
	unsigned	nss;
	seqSS		*pSeqSS;


	/* Allocate space for the SSCB structures */
	pSP->pSS = pSS = (SSCB *)calloc(pSeqProg->numSS, sizeof(SSCB));

	/* Copy information for each state set and state */
	pSeqSS = pSeqProg->pSS;
	for (nss = 0; nss < pSeqProg->numSS; nss++, pSS++, pSeqSS++)
	{
		/* Fill in SSCB */
		pSS->pSSName = pSeqSS->pSSName;
		pSS->numStates = pSeqSS->numStates;
		pSS->maxNumDelays = pSeqSS->numDelays;

		pSS->delay = (double *)calloc(pSS->maxNumDelays, sizeof(double));
		pSS->delayExpired = (boolean *)calloc(pSS->maxNumDelays, sizeof(boolean));
		pSS->currentState = 0; /* initial state */
		pSS->nextState = 0;
		pSS->prevState = 0;
		pSS->threadId = 0;
		/* Initialize to start time rather than zero time! */
		pvTimeGetCurrentDouble(&pSS->timeEntered);
		pSS->sprog = pSP;

		DEBUG("init_sscb: SS Name=%s, num States=%ld, pSS=%p\n",
			pSS->pSSName, pSS->numStates, pSS);
		pSS->allFirstConnectAndMonitorSemId = epicsEventMustCreate(epicsEventEmpty);
		/* Create a binary semaphore for synchronizing events in a SS */
		pSS->syncSemId = epicsEventMustCreate(epicsEventEmpty);

		/* Create binary semaphores for synchronous pvGet() and
		   pvPut() */
		pSS->getSemId = epicsEventMustCreate(epicsEventFull);

		/* Create binary semaphores for thread death */
		pSS->death1SemId = epicsEventMustCreate(epicsEventEmpty);
		pSS->death2SemId = epicsEventMustCreate(epicsEventEmpty);
		pSS->death3SemId = epicsEventMustCreate(epicsEventEmpty);
		pSS->death4SemId = epicsEventMustCreate(epicsEventEmpty);

		/* No need to copy the state structs, they can be shared
		   because nothing gets mutated. */
		pSS->pStates = pSeqSS->pStates;

		/* Allocate user variable area if safe mode option (+s) is set */
		if (pSP->options & OPT_SAFE)
		{
			pSP->pVar = (char *)calloc(pSP->varSize, 1);
		}
	}

	DEBUG("init_sscb: numSS=%ld\n", pSP->numSS);
}

/*
 * init_chan--Build the database channel structures.
 * Note:  Actual PV name is not filled in here. */
static void init_chan(seqProgram *pSeqProg, SPROG *pSP)
{
	unsigned	nchan;
	CHAN		*pDB;
	seqChan		*pSeqChan;

	/* Allocate space for the CHAN structures */
	pSP->pChan = (CHAN *)calloc(pSP->numChans, sizeof(CHAN));
	pDB = pSP->pChan;

	pSeqChan = pSeqProg->pChan;
	for (nchan = 0; nchan < pSP->numChans; nchan++, pDB++, pSeqChan++)
	{
		DEBUG("init_chan: pDB=%p\n", pDB);
		pDB->sprog = pSP;
		pDB->pVarName = pSeqChan->pVarName;
		pDB->offset = pSeqChan->offset;
		pDB->count = pSeqChan->count;
		pDB->efId = pSeqChan->efId;
		pDB->monFlag = pSeqChan->monitored;
		pDB->eventNum = pSeqChan->eventNum;
		pDB->queued = pSeqChan->queued;
		pDB->maxQueueSize = pSeqChan->queueSize ?
				    pSeqChan->queueSize : MAX_QUEUE_SIZE;
		pDB->queueIndex = pSeqChan->queueIndex;
		pDB->assigned = 0;

		if (pSeqChan->dbAsName != NULL)
		{
			char name_buffer[100];

			seqMacEval(pSP, pSeqChan->dbAsName, name_buffer, sizeof(name_buffer));
			if (name_buffer[0])
			{
				pDB->dbAsName = epicsStrDup(name_buffer);
			}
			DEBUG("  assigned name=%s, expanded name=%s\n",
				pSeqChan->dbAsName, pDB->dbAsName);
		}
		else
			DEBUG("  pv name=<anonymous>\n");

		/* Latest error message (dynamically allocated) */
		pDB->message = NULL;

		/* Fill in get/put db types, element size */
		pDB->type = find_type(pSeqChan->pVarType);

		DEBUG(" Assigned Name=%s, VarName=%s, VarType=%s, count=%ld\n"
			"   size=%u, efId=%ld, monFlag=%u, eventNum=%ld\n",
			pDB->dbAsName, pDB->pVarName,
			pDB->type->pTypeStr, pDB->count,
			pDB->type->size,
			pDB->efId, pDB->monFlag, pDB->eventNum);

		pDB->varLock = epicsMutexMustCreate();
		pDB->dirty = (boolean *)calloc(pSP->numSS,sizeof(boolean));
		pDB->getComplete = (boolean *)calloc(pSP->numSS,sizeof(boolean));
		pDB->putSemId = epicsEventMustCreate(epicsEventFull);
	}
}

/*
 * find_type -- returns types for DB put/get and element size
 * based on user variable type.
 * Mapping is determined by the following pv_type_map[] array.
 * pvTypeTIME_* types for gets/monitors return status and time stamp.
 */
static PVTYPE pv_type_map[] =
{
	{ "char",		pvTypeCHAR,	pvTypeTIME_CHAR,	sizeof(char)	},
	{ "short",		pvTypeSHORT,	pvTypeTIME_SHORT,	sizeof(short)	},
	{ "int",		pvTypeLONG,	pvTypeTIME_LONG,	sizeof(long)	},
	{ "long",		pvTypeLONG,	pvTypeTIME_LONG,	sizeof(long)	},
	{ "unsigned char",	pvTypeCHAR,	pvTypeTIME_CHAR,	sizeof(char)	},
	{ "unsigned short",	pvTypeSHORT,	pvTypeTIME_SHORT,	sizeof(short)	},
	{ "unsigned int",	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(long)	},
	{ "unsigned long",	pvTypeLONG,	pvTypeTIME_LONG,	sizeof(long)	},
	{ "float",		pvTypeFLOAT,	pvTypeTIME_FLOAT,	sizeof(float)	},
	{ "double",		pvTypeDOUBLE,	pvTypeTIME_DOUBLE,	sizeof(double)	},
	{ "string",		pvTypeSTRING,	pvTypeTIME_STRING,	sizeof(string)	},
	{ NULL,			pvTypeERROR,	pvTypeERROR,		0		}
};

static PVTYPE *find_type(const char *pUserType)
{
	PVTYPE	*pt = pv_type_map;

	while (pt->pTypeStr)
	{
		if (strcmp(pUserType, pt->pTypeStr) == 0)
		{
			break;
		}
		pt++;
	}
	return pt;
}
