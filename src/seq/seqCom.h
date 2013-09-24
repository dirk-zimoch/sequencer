/*************************************************************************\
Copyright (c) 1993      The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2012 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/*	External interface to the sequencer run-time library
 *
 *	Author:		Andy Kozubal
 *	Date:		01mar94
 *
 *	Experimental Physics and Industrial Control System (EPICS)
 *
 *	This software was produced under  U.S. Government contracts:
 *	(W-7405-ENG-36) at the Los Alamos National Laboratory,
 *	and (W-31-109-ENG-38) at Argonne National Laboratory.
 *
 *	Initial development by:
 *		The Controls and Automation Group (AT-8)
 *		Ground Test Accelerator
 *		Accelerator Technology Division
 *		Los Alamos National Laboratory
 */
#ifndef INCLseqComh
#define INCLseqComh

#include "shareLib.h"
#include "pvAlarm.h"
#include "epicsThread.h"
#include "epicsTime.h"

#include "seq_release.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Bit encoding for run-time options */
#define OPT_DEBUG		((seqMask)1u<<0)	/* turn on debugging */
#define OPT_ASYNC		((seqMask)1u<<1)	/* use async. gets */
#define OPT_CONN		((seqMask)1u<<2)	/* wait for all connections */
#define OPT_REENT		((seqMask)1u<<3)	/* generate reentrant code */
#define OPT_NEWEF		((seqMask)1u<<4)	/* new event flag mode */
#define OPT_SAFE		((seqMask)1u<<5)	/* safe mode */

/* Bit encoding for State Specific Options */
#define OPT_NORESETTIMERS	((seqMask)1u<<0)	/* Don't reset timers on */
							/* entry to state from same state */
#define OPT_DOENTRYFROMSELF	((seqMask)1u<<1)	/* Do entry{} even if from same state */
#define OPT_DOEXITTOSELF	((seqMask)1u<<2)	/* Do exit{} even if to same state */

/* seqMask macros */
#define NBITS			(8*sizeof(seqMask))	/* # bits in seqMask word */
#define NWORDS(maxBitNum)	(1+(maxBitNum)/NBITS)	/* # words in seqMask */

#define bitSet(words, bitnum)	( words[(bitnum)/NBITS] |=  (1u<<((bitnum)%NBITS)))
#define bitClear(words, bitnum)	( words[(bitnum)/NBITS] &= ~(1u<<((bitnum)%NBITS)))
#define bitTest(words, bitnum)	((words[(bitnum)/NBITS] &  (1u<<((bitnum)%NBITS))) != 0)

#define optTest(sp,opt)		(((sp)->options & (opt)) != 0)
					/* test if opt is set in program instance sp */

#define NOEVFLAG		0	/* argument to pvSync to remove sync */

#define DEFAULT_QUEUE_SIZE	2	/* default queue size (elements) */

/* I/O completion type (extra argument passed to seq_pvGet() and seq_pvPut()) */
enum compType {
	DEFAULT,
        ASYNC,
        SYNC
};

#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif

typedef	struct state_set *const SS_ID;	/* state set id, opaque */
typedef struct _seq_var SEQ_VARS;	/* defined by program, opaque */
typedef char string[MAX_STRING_SIZE];	/* the string typedef */

typedef SEQ_VARS USER_VAR;              /* for compatibility */

/* these typedefs make the code more self documenting */
typedef epicsUInt32 seqMask;		/* for event masks and options */
typedef unsigned EV_ID;			/* identifier for an event */
typedef unsigned VAR_ID;		/* identifier for a pv */
typedef int seqBool;

/* Prototypes for functions generated by snc */
typedef void ACTION_FUNC(SS_ID ssId, SEQ_VARS *const var, int transNum, int *nextState);
typedef seqBool EVENT_FUNC(SS_ID ssId, SEQ_VARS *const var, int *transNum, int *nextState);
typedef void ENTRY_FUNC(SS_ID ssId, SEQ_VARS *const var);
typedef void EXIT_FUNC(SS_ID ssId, SEQ_VARS *const var);
typedef void INIT_FUNC(SEQ_VARS *const var);

typedef const struct seqChan seqChan;
typedef const struct seqState seqState;
typedef const struct seqSS seqSS;
typedef struct seqProgram seqProgram;

/* Static information about a channel */
struct seqChan
{
	const char	*chName;	/* assigned channel name */
	size_t		offset;		/* offset to value */
	const char	*varName;	/* variable name, including subscripts*/
	const char	*varType;	/* variable type, e.g. "int" */
	unsigned	count;		/* element count for arrays */
	unsigned	eventNum;	/* event number for this channel */
	EV_ID		efId;		/* event flag id if synced */
	seqBool		monitored;	/* whether channel should be monitored */
	unsigned	queueSize;	/* syncQ queue size (0=not queued) */
	unsigned	queueIndex;	/* syncQ queue index */
};

/* Static information about a state */
struct seqState
{
	const char	*stateName;	/* state name */
	ACTION_FUNC	*actionFunc;	/* action routine for this state */
	EVENT_FUNC	*eventFunc;	/* event routine for this state */
	ENTRY_FUNC	*entryFunc;	/* statements performed on entry to state */
	EXIT_FUNC	*exitFunc;	/* statements performed on exit from state */
	const seqMask	*eventMask;	/* event mask for this state */
	seqMask		options;	/* state option mask */
};

/* Static information about a state set */
struct seqSS
{
	const char	*ssName;	/* state set name */
	seqState	*states;	/* array of state blocks */
	unsigned	numStates;	/* number of states in this state set */
};

/* Static information about a state program */
struct seqProgram
{
	unsigned	magic;		/* magic number */
	const char	*progName;	/* program name (for debugging) */
	seqChan		*chan;		/* table of channels */
	unsigned	numChans;	/* number of db channels */
	seqSS		*ss;		/* array of state set info structs */
	unsigned	numSS;		/* number of state sets */
	unsigned	varSize;	/* # bytes in user variable area */
	const char	*params;	/* program paramters */
	unsigned	numEvFlags;	/* number of event flags */
	seqMask		options;	/* program option mask */
	INIT_FUNC	*initFunc;	/* init function */
	ENTRY_FUNC	*entryFunc;	/* entry function */
	EXIT_FUNC	*exitFunc;	/* exit function */
	unsigned	numQueues;	/* number of syncQ queues */
};

/*
 * Function declarations for interface between state program & sequencer.
 * Prefix "seq_" is added by SNC to reduce probability of name clashes.
 * Implementations are in module seq_if.c.
 */

/* event flag operations */
epicsShareFunc void seq_efSet(SS_ID, EV_ID);
epicsShareFunc seqBool seq_efTest(SS_ID, EV_ID);
epicsShareFunc seqBool seq_efClear(SS_ID, EV_ID);
epicsShareFunc seqBool seq_efTestAndClear(SS_ID, EV_ID);
/* pv operations */
epicsShareFunc pvStat seq_pvGet(SS_ID, VAR_ID, enum compType);
epicsShareFunc pvStat seq_pvGetMultiple(SS_ID, VAR_ID,
	unsigned, enum compType);
epicsShareFunc seqBool seq_pvGetQ(SS_ID, VAR_ID);
epicsShareFunc void seq_pvFlushQ(SS_ID, VAR_ID);
/* retain seq_pvFreeQ for compatibility */
#define seq_pvFreeQ seq_pvFlushQ
epicsShareFunc pvStat seq_pvPut(SS_ID, VAR_ID, enum compType);
epicsShareFunc pvStat seq_pvPutMultiple(SS_ID, VAR_ID,
	unsigned, enum compType);
epicsShareFunc seqBool seq_pvGetComplete(SS_ID, VAR_ID);
epicsShareFunc seqBool seq_pvPutComplete(SS_ID, VAR_ID,
	unsigned, seqBool, seqBool*);
epicsShareFunc pvStat seq_pvAssign(SS_ID, VAR_ID, const char *);
epicsShareFunc pvStat seq_pvMonitor(SS_ID, VAR_ID);
epicsShareFunc void seq_pvSync(SS_ID, VAR_ID, unsigned, EV_ID);
epicsShareFunc pvStat seq_pvStopMonitor(SS_ID, VAR_ID);
/* pv info */
epicsShareFunc char *seq_pvName(SS_ID, VAR_ID);
epicsShareFunc unsigned seq_pvCount(SS_ID, VAR_ID);
epicsShareFunc pvStat seq_pvStatus(SS_ID, VAR_ID);
epicsShareFunc pvSevr seq_pvSeverity(SS_ID, VAR_ID);
epicsShareFunc epicsTimeStamp seq_pvTimeStamp(SS_ID, VAR_ID);
epicsShareFunc const char *seq_pvMessage(SS_ID, VAR_ID);
epicsShareFunc seqBool seq_pvAssigned(SS_ID, VAR_ID);
epicsShareFunc seqBool seq_pvConnected(SS_ID, VAR_ID);

#define seq_pvIndex(ssId, varId)	varId
#define seq_ssId(ssId)			ssId
#define seq_pVar(ssId)			_seq_var

/* global operations */
epicsShareFunc void seq_pvFlush(SS_ID);
epicsShareFunc seqBool seq_delay(SS_ID, double);
epicsShareFunc char *seq_macValueGet(SS_ID, const char *);
epicsShareFunc void seq_exit(SS_ID);
/* global info */
epicsShareFunc unsigned seq_pvChannelCount(SS_ID);
epicsShareFunc unsigned seq_pvConnectCount(SS_ID);
epicsShareFunc unsigned seq_pvAssignCount(SS_ID);
epicsShareFunc seqBool seq_optGet(SS_ID, const char *);

/* shell commands */
epicsShareFunc void seqShow(epicsThreadId);
epicsShareFunc void seqChanShow(epicsThreadId, const char *);
epicsShareFunc void seqcar(int level);
epicsShareFunc void seqQueueShow(epicsThreadId);
epicsShareFunc void seqStop(epicsThreadId);
epicsShareFunc epicsThreadId seq(seqProgram *, const char *, unsigned);
/* called by generated main and registrar routines */
epicsShareFunc void seqRegisterSequencerProgram(seqProgram *p);
epicsShareFunc void seqRegisterSequencerCommands(void);

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif	/*INCLseqComh*/
