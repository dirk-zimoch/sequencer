/*************************************************************************\
Copyright (c) 1991-1994 The Regents of the University of California
                        and the University of Chicago.
                        Los Alamos National Laboratory
Copyright (c) 2010-2015 Helmholtz-Zentrum Berlin f. Materialien
                        und Energie GmbH, Germany (HZB)
This file is distributed subject to a Software License Agreement found
in the file LICENSE that is included with this distribution.
\*************************************************************************/
/*	Interface functions from state program to run-time sequencer.
 *
 *	Author:  Andy Kozubal
 *	Date:    1 March, 1994
 *
 *	Experimental Physics and Industrial Control System (EPICS)
 *
 *	This software was produced under  U.S. Government contracts:
 *	(W-7405-ENG-36) at the Los Alamos National Laboratory,
 *	and (W-31-109-ENG-38) at Argonne National Laboratory.
 *
 *	Initial development by:
 *	  The Controls and Automation Group (AT-8)
 *	  Ground Test Accelerator
 *	  Accelerator Technology Division
 *	  Los Alamos National Laboratory
 *
 *	Co-developed with
 *	  The Controls and Computing Group
 *	  Accelerator Systems Division
 *	  Advanced Photon Source
 *	  Argonne National Laboratory
 */
#include "seq.h"
#include "seq_debug.h"

static void completion_failure(pvEventType evtype, PVMETA *meta)
{
	meta->status = pvStatERROR;
	meta->severity = pvSevrERROR;
	meta->message = pvEventGet ? "get completion failure" : "put completion failure";
}

static void completion_timeout(pvEventType evtype, PVMETA *meta)
{
	meta->status = pvStatTIMEOUT;
	meta->severity = pvSevrERROR;
	meta->message = pvEventGet ? "get completion failure" : "put completion failure";;
}

static void pv_call_failure(DBCHAN *dbch, PVMETA *meta, pvStat status)
{
	meta->status = status;
	meta->severity = pvSevrERROR;
	meta->message = pvVarGetMess(dbch->pvid);
}

static pvStat check_connected(DBCHAN *dbch, PVMETA *meta)
{
	if (!dbch->connected)
	{
		meta->status = pvStatDISCONN;
		meta->severity = pvSevrERROR;
		meta->message = "disconnected";
		return meta->status;
	}
	else
	{
		return pvStatOK;
	}
}

static pvStat check_pending(
	pvEventType evtype,
	SS_ID ss,
	PVREQ **req,
	const char *varName,
	DBCHAN *dbch,
	PVMETA *meta,
	enum compType compType,
	double tmo)
{
	const char *call = evtype == pvEventGet ? "pvGet" : "pvPut";

	assert(evtype != pvEventMonitor);
	if (compType == SYNC)
	{
		if (tmo <= 0.0)
		{
			errlogSevPrintf(errlogMajor,
				"%s(%s,SYNC,%f): user error (timeout must be positive)\n",
				call, varName, tmo);
			return pvStatERROR;
		}
		while (*req)
		{
			/* a request is already pending (must be an async request) */
			double before, after;
			pvStat status;

			pvTimeGetCurrentDouble(&before);
			switch (epicsEventWaitWithTimeout(ss->syncSem, tmo))
			{
			case epicsEventWaitOK:
				status = check_connected(dbch, meta);
				if (status != pvStatOK)
					return status;
				pvTimeGetCurrentDouble(&after);
				tmo -= (after - before);
				if (tmo > 0.0)
					break;
				/* else: fall through to timeout */
			case epicsEventWaitTimeout:
				errlogSevPrintf(errlogMajor,
					"%s(ss %s, var %s, pv %s): failed (timeout "
					"waiting for other %s requests to finish)\n",
					call, ss->ssName, varName, dbch->dbName, call
				);
				completion_timeout(evtype, meta);
				return meta->status;
			case epicsEventWaitError:
				errlogSevPrintf(errlogFatal,
					"%s: epicsEventWaitWithTimeout() failure\n", call);
				completion_failure(evtype, meta);
				return meta->status;
			}
		}
	}
	else if (compType == ASYNC)
	{
		if (*req) {
			errlogSevPrintf(errlogMajor,
				"%s(ss %s, var %s, pv %s): user error "
				"(there is already a %s pending for this channel/"
				"state set combination)\n",
				call, ss->ssName, varName, dbch->dbName, call
			);
			return pvStatERROR;
		}
	}
	return pvStatOK;
}

static pvStat wait_complete(
	pvEventType evtype,
	SS_ID ss,
	PVREQ **req,
	DBCHAN *dbch,
	PVMETA *meta,
	double tmo)
{
	const char *call = evtype == pvEventGet ? "pvGet" : "pvPut";
	while (*req)
	{
		DEBUG("wait_complete: dbName=%s, *req=%p, tmo=%f\n", dbch->dbName, *req, tmo);
		switch (epicsEventWaitWithTimeout(ss->syncSem, tmo))
		{
		case epicsEventWaitOK:
			break;
		case epicsEventWaitTimeout:
			*req = NULL;			/* cancel the request */
			completion_timeout(evtype, meta);
			return meta->status;
		case epicsEventWaitError:
			errlogSevPrintf(errlogFatal,
				"%s: epicsEventWaitWithTimeout() failure\n", call);
			*req = NULL;			/* cancel the request */
			completion_failure(evtype, meta);
			return meta->status;
		}
	}
	return check_connected(dbch, meta);
}

/*
 * Get value from a channel.
 */
epicsShareFunc pvStat seq_pvGet(SS_ID ss, CH_ID ch, enum compType compType)
{
	return seq_pvGetTmo(ss, ch, compType, DEFAULT_TIMEOUT);
}

/*
 * Get value from a channel, with timeout.
 */
epicsShareFunc pvStat seq_pvGetTmo(SS_ID ss, CH_ID ch, enum compType compType, double tmo)
{
	PROG		*sp = ss->prog;
	unsigned nch = ch - sp->chan;
	pvStat		status;
	PVREQ		*req;
	DBCHAN		*dbch = ch->dbch;
	PVMETA		*meta = metaPtr(ch,ss);

	/* Anonymous PV and safe mode, just copy from shared buffer.
	   Note that completion is always immediate, so no distinction
	   between SYNC and ASYNC needed. See also pvGetComplete. */
	if (optTest(sp, OPT_SAFE) && !dbch)
	{
		/* Copy regardless of whether dirty flag is set or not */
		ss_read_buffer(ss, ch, FALSE);
		return pvStatOK;
	}
	/* No named PV and traditional mode => user error */
	if (!dbch)
	{
		errlogSevPrintf(errlogMajor,
			"pvGet(%s): user error (not assigned to a PV)\n",
			ch->varName
		);
		return pvStatERROR;
	}

	if (compType == DEFAULT)
	{
		compType = optTest(sp, OPT_ASYNC) ? ASYNC : SYNC;
	}

	status = check_pending(pvEventGet, ss, ss->getReq + nch, ch->varName,
		dbch, meta, compType, tmo);
	if (status != pvStatOK)
		return status;

	/* Allocate and initialize a pv request */
	req = (PVREQ *)freeListMalloc(sp->pvReqPool);
	req->ss = ss;
	req->ch = ch;

	assert(ss->getReq[nch] == NULL);
	ss->getReq[nch] = req;

	/* Perform the PV get operation with a callback routine specified.
	   Requesting more than db channel has available is ok. */
	status = pvVarGetCallback(
			&dbch->pvid,		/* PV id */
			ch->type->getType,	/* request type */
			ch->count,		/* element count */
			req);			/* user arg */
	if (status != pvStatOK)
	{
		pv_call_failure(dbch, meta, status);
		errlogSevPrintf(errlogFatal,
			"pvGet(var %s, pv %s): pvVarGetCallback() failure: %s\n",
			ch->varName, dbch->dbName, pvVarGetMess(dbch->pvid));
		ss->getReq[nch] = NULL;	/* cancel the request */
		freeListFree(sp->pvReqPool, req);
		check_connected(dbch, meta);
		return status;
	}

	/* Synchronous: wait for completion */
	if (compType == SYNC)
	{
		pvSysFlush(sp->pvSys);
		status = wait_complete(pvEventGet, ss, ss->getReq + nch, dbch, meta, tmo);
		if (status != pvStatOK)
			return status;
		if (optTest(sp, OPT_SAFE))
			/* Copy regardless of whether dirty flag is set or not */
			ss_read_buffer(ss, ch, FALSE);
	}

	return pvStatOK;
}

/*
 * Return whether the last get completed. In safe mode, as a
 * side effect, copy value from shared buffer to state set local buffer.
 */
epicsShareFunc boolean seq_pvGetComplete(
	SS_ID	ss,
	CH_ID	ch)
{
	PROG	*sp = ss->prog;
	unsigned nch = ch - sp->chan;

	if (!ch->dbch)
	{
		/* Anonymous PVs always complete immediately */
		if (!optTest(sp, OPT_SAFE))
			errlogSevPrintf(errlogMajor,
				"pvGetComplete(%s): user error (not assigned to a PV)\n",
				ch->varName);
		return TRUE;
	}
	else if (!ss->getReq[nch])
	{
		pvStat status = check_connected(ch->dbch, metaPtr(ch,ss));
		if (status == pvStatOK && optTest(sp, OPT_SAFE))
		{
			/* In safe mode, copy value and meta data from shared buffer
			   to ss local buffer. */
			/* Copy regardless of whether dirty flag is set or not */
			ss_read_buffer(ss, ch, FALSE);
		}
		return TRUE;
	}
	return FALSE;
}

/*
 * Array variant of seq_pvGetComplete.
 */
epicsShareFunc boolean seq_pvArrayGetComplete(
	SS_ID		ss,
	CH_ID		*chs,
	unsigned	length,
	boolean		any,
	boolean		*complete)
{
	boolean		anyDone = FALSE, allDone = TRUE;
	unsigned	n;

	for (n = 0; n < length; n++)
	{
		boolean done = seq_pvGetComplete(ss, chs[n]);

		anyDone = anyDone || done;
		allDone = allDone && done;

		if (complete)
		{
			complete[n] = done;
		}
		else if (any == done)
		{
			break;
		}
	}

	return any?anyDone:allDone;
}

/*
 * Cancel the last asynchronous get request.
 */
epicsShareFunc void seq_pvGetCancel(
	SS_ID	ss,
	CH_ID	ch)
{
	PROG	*sp = ss->prog;

	if (!ch->dbch)
	{
		if (!optTest(sp, OPT_SAFE))
			errlogSevPrintf(errlogMinor,
				"pvGetCancel(%s): user error (not assigned to a PV)\n",
				ch->varName);
	}
	else
	{
		ss->getReq[chNum(ch)] = NULL;	/* cancel the request */
	}
}

/*
 * Array variant of seq_pvGetCancel.
 */
epicsShareFunc void seq_pvArrayGetCancel(
	SS_ID		ss,
	CH_ID		*chs,
	unsigned	length)
{
	unsigned	n;

	for (n = 0; n < length; n++)
		seq_pvGetCancel(ss, chs[n]);
}

/* -------------------------------------------------------------------------- */

struct putq_cp_arg {
	CHAN	*ch;
	void	*var;
};

static void *putq_cp(void *dest, const void *src, size_t elemSize)
{
	struct putq_cp_arg *arg = (struct putq_cp_arg *)src;
	CHAN *ch = arg->ch;

	/* Note: we use ch->type->getType for all queue operations */
	return memcpy(pv_value_ptr(dest, ch->type->getType),
		arg->var, ch->type->size * ch->count);
}

static void anonymous_put(SS_ID ss, CHAN *ch)
{
	char *var = valPtr(ch,ss);

	DEBUG("anonymous_put: enter, varName=%s\n", ch->varName);
	if (ch->queue)
	{
		QUEUE queue = ch->queue;
		/* Note: we use ch->type->getType for all queue operations */
		pvType type = ch->type->getType;
		size_t size = ch->type->size;
		boolean full;
		struct putq_cp_arg arg = {ch, var};

		DEBUG("anonymous_put(queued): type=%d, size=%d, count=%d, buf_size=%d, q=%p\n",
			type, size, ch->count, pv_size_n(type, ch->count), queue);
		print_channel_value(DEBUG, ch, var);

		/* Note: Must lock here because multiple state sets can issue
		   pvPut calls concurrently. OTOH, no need to lock against CA
		   callbacks, because anonymous and named PVs are disjoint. */
		epicsMutexMustLock(ch->varLock);

		full = seqQueuePutF(queue, putq_cp, &arg);
		if (full)
		{
			errlogSevPrintf(errlogMinor,
			  "pvPut on queued channel '%s' (anonymous): "
			  "last queue element overwritten (queue is full)\n",
			  ch->varName
			);
		}

		epicsMutexUnlock(ch->varLock);
	}
	else
	{
		/* Set dirty flag for those state sets that monitor us */
		ss_write_buffer(ch, var, 0, TRUE);
	}
	/* If there's an event flag associated with this channel, set it */
	if (ch->syncedTo)
	{
		DEBUG("anonymous_put: ch->syncedTo->synced=%u\n", *ch->syncedTo->synced);
		seq_efSet(ss, ch->syncedTo);
	}
	/* Wake up each state set that uses this channel in an event */
	ss_wakeup(ss->prog, ch->eventNum);
	DEBUG("anonymous_put: leave, ch->syncedTo=%p\n", ch->syncedTo);
}

/*
 * Put a variable's value to a PV.
 */
epicsShareFunc pvStat seq_pvPut(SS_ID ss, CH_ID ch, enum compType compType)
{
	return seq_pvPutTmo(ss, ch, compType, DEFAULT_TIMEOUT);
}

/*
 * Put a variable's value to a PV, with timeout.
 */
epicsShareFunc pvStat seq_pvPutTmo(SS_ID ss, CH_ID ch, enum compType compType, double tmo)
{
	PROG	*sp = ss->prog;
	unsigned nch = ch - sp->chan;
	pvStat	status;
	unsigned count;
	char	*var = valPtr(ch,ss);	/* ptr to value */
	PVREQ	*req;
	DBCHAN	*dbch = ch->dbch;
	PVMETA	*meta = metaPtr(ch,ss);

	DEBUG("pvPut: pv name=%s, var=%p\n", dbch ? dbch->dbName : "<anonymous>", var);

	/* First handle anonymous PV (safe mode only) */
	if (optTest(sp, OPT_SAFE) && !dbch)
	{
		anonymous_put(ss, ch);
		return pvStatOK;
	}
	if (!dbch)
	{
		errlogSevPrintf(errlogMajor,
			"pvPut(%s): user error (not assigned to a PV)\n",
			ch->varName
		);
		return pvStatERROR;
	}

	/* Check for channel connected */
	status = check_connected(dbch, meta);
	if (status != pvStatOK) return status;

	/* Determine whether to perform synchronous, asynchronous, or
	   plain put ((+a) option was never honored for put, so DEFAULT
	   means fire-and-forget) */
	status = check_pending(pvEventPut, ss, ss->putReq + nch, ch->varName,
		dbch, meta, compType, tmo);
	if (status != pvStatOK)
		return status;

	assert(ss->putReq[chId] == NULL);

	/* Determine number of elements to put (don't try to put more
	   than db count) */
	count = dbch->dbCount;

	/* Perform the PV put operation (either non-blocking or with a
	   callback routine specified) */
	if (compType == DEFAULT)
	{
		status = pvVarPutNoBlock(
				&dbch->pvid,		/* PV id */
				ch->type->putType,	/* data type */
				count,			/* element count */
				(pvValue *)var);	/* data value */
		if (status != pvStatOK)
		{
			pv_call_failure(dbch, meta, status);
			errlogSevPrintf(errlogFatal, "pvPut(var %s, pv %s): pvVarPutNoBlock() failure: %s\n",
				ch->varName, dbch->dbName, pvVarGetMess(dbch->pvid));
			return status;
		}
	}
	else
	{
		/* Allocate and initialize a pv request */
		req = (PVREQ *)freeListMalloc(sp->pvReqPool);
		req->ss = ss;
		req->ch = ch;

		assert(ss->putReq[nch] == NULL);
		ss->putReq[nch] = req;

		status = pvVarPutCallback(
				&dbch->pvid,		/* PV id */
				ch->type->putType,	/* data type */
				count,			/* element count */
				(pvValue *)var,		/* data value */
				req);			/* user arg */
		if (status != pvStatOK)
		{
			pv_call_failure(dbch, meta, status);
			errlogSevPrintf(errlogFatal, "pvPut(var %s, pv %s): pvVarPutCallback() failure: %s\n",
				ch->varName, dbch->dbName, pvVarGetMess(dbch->pvid));
			ss->putReq[nch] = NULL;	/* cancel the request */
			freeListFree(sp->pvReqPool, req);
			check_connected(dbch, meta);
			return status;
		}

		if (compType == SYNC)			/* wait for completion */
		{
			pvSysFlush(sp->pvSys);
			status = wait_complete(pvEventPut, ss, ss->putReq + chId, dbch, meta, tmo);
			if (status != pvStatOK)
				return status;
		}
	}

	/* Synchronous: wait for completion */
	if (compType == SYNC)
	{
		pvSysFlush(sp->pvSys);
		status = wait_complete(pvEventPut, ss, ss->putReq + chId, dbch, meta, tmo);
		if (status != pvStatOK)
			return status;
	}

	return pvStatOK;
}

/*
 * Return whether the last put completed.
 */
epicsShareFunc boolean seq_pvPutComplete(
	SS_ID		ss,
	CH_ID		ch,
	unsigned	length,
	boolean		any,
	boolean		*complete)
{
	return seq_pvArrayPutComplete(ss, &ch, length, any, complete);
}

/*
 * Return whether the last put completed.
 */
static boolean seq_pvSinglePutComplete(
	SS_ID	ss,
	CH_ID	ch)
{
	PROG	*sp = ss->prog;
	unsigned nch = ch - sp->chan;

	if (!ch->dbch)
	{
		/* Anonymous PVs always complete immediately */
		if (!(sp->options & OPT_SAFE))
			errlogSevPrintf(errlogMajor,
				"pvPutComplete(%s): user error (not assigned to a PV)\n",
				ch->varName);
		return TRUE;
	}
	else if (!ss->putReq[nch])
	{
		check_connected(ch->dbch, metaPtr(ch,ss));
		return TRUE;
	}

	return FALSE;
}

/*
 * Return whether the last put completed.
 */
epicsShareFunc boolean seq_pvArrayPutComplete(
	SS_ID		ss,
	CH_ID		*chs,
	unsigned	length,
	boolean		any,
	boolean		*complete)
{
	boolean		anyDone = FALSE, allDone = TRUE;
	unsigned	n;

	for (n = 0; n < length; n++)
	{
		boolean	done = seq_pvSinglePutComplete(ss, chs[n]);

		anyDone = anyDone || done;
		allDone = allDone && done;

		if (complete)
		{
			complete[n] = done;
		}
		else if (any == done)
		{
			break;
		}
	}

	return any?anyDone:allDone;
}

/*
 * Cancel the last asynchronous put request.
 */
epicsShareFunc void seq_pvPutCancel(
	SS_ID	ss,
	CH_ID	ch)
{
	PROG	*sp = ss->prog;
	unsigned nch = ch - sp->chan;

	if (!ch->dbch)
	{
		if (!optTest(sp, OPT_SAFE))
			errlogSevPrintf(errlogMinor,
				"pvPutCancel(%s): user error (not assigned to a PV)\n",
				ch->varName);
	}
	else
	{
		ss->putReq[nch] = NULL;	/* cancel the request */
	}
}

/*
 * Cancel the last asynchronous put request.
 */
epicsShareFunc void seq_pvArrayPutCancel(
	SS_ID		ss,
	CH_ID		*chs,
	unsigned	length)
{
	unsigned	n;

	for (n = 0; n < length; n++)
		seq_pvPutCancel(ss, chs[n]);
}

/* -------------------------------------------------------------------------- */

/*
 * Assign/Connect to a channel.
 * Like seq_pvAssign, but replaces program parameters in the pv name,
 * as in an assign clause.
 */
epicsShareFunc pvStat seq_pvAssignSubst(SS_ID ss, CH_ID ch, const char *pvName)
{
	char	new_pv_name[100];

	seqMacEval(ss->prog, pvName, new_pv_name, sizeof(new_pv_name));
	return seq_pvAssign(ss, ch, new_pv_name);
}

/*
 * Assign/Connect to a channel.
 * Assign to a zero-length string ("") disconnects/de-assigns,
 * in safe mode, creates an anonymous PV.
 */
epicsShareFunc pvStat seq_pvAssign(SS_ID ss, CH_ID ch, const char *pvName)
{
	PROG	*sp = ss->prog;
	pvStat	status = pvStatOK;
	DBCHAN	*dbch;

	if (!pvName) pvName = "";

	DEBUG("Assign %s to \"%s\"\n", ch->varName, pvName);

	epicsMutexMustLock(sp->lock);

	dbch = ch->dbch;

	if (dbch)	/* was assigned to a named PV */
	{
		ch->dbch = 0;

		epicsMutexUnlock(sp->lock);

		status = pvVarDestroy(&dbch->pvid);

		epicsMutexMustLock(sp->lock);

		sp->assignCount--;

		if (dbch->connected)	/* see connection handler */
		{
			dbch->connected = FALSE;
			sp->connectCount--;

			/* Must not call seq_camonitor(ch, FALSE), it would give an
			error because channel is already dead. pvVarDestroy takes
			care that the monid inside the pvid gets invalidated. */

			/* Note ch->monitored remains on because it is a configuration
			value that belongs to the variable and newly created channels
			for the same variable should inherit this configuration. */
		}

		if (status != pvStatOK)
		{
			errlogSevPrintf(errlogFatal, "pvAssign(var %s, pv %s): pvVarDestroy() failure: "
				"%s\n", ch->varName, dbch->dbName, pvVarGetMess(dbch->pvid));
		}

		free(dbch->dbName);
	}

	if (pvName[0] == 0)	/* new name is empty -> free resources */
	{
		if (dbch) {
			free(dbch);
		}
	}
	else		/* new name is non-empty -> create resources */
	{
		if (!dbch)
		{
			dbch = new(DBCHAN);
			if (!dbch)
			{
				errlogSevPrintf(errlogFatal, "pvAssign: calloc failed\n");
				epicsMutexUnlock(sp->lock);
				return pvStatERROR;
			}
		}
		dbch->dbName = epicsStrDup(pvName);
		if (!dbch->dbName)
		{
			errlogSevPrintf(errlogFatal, "pvAssign: epicsStrDup failed\n");
			free(dbch);
			epicsMutexUnlock(sp->lock);
			return pvStatERROR;
		}
		ch->dbch = dbch;

		status = pvVarCreate(
			sp->pvSys,		/* PV system context */
			dbch->dbName,		/* DB channel name */
			seq_conn_handler,	/* connection handler routine */
			seq_event_handler,	/* event handler routine */
			ch,			/* user ptr is CHAN structure */
			&dbch->pvid);		/* ptr to pvid */
		if (status != pvStatOK)
		{
			errlogSevPrintf(errlogFatal, "pvAssign(var %s, pv %s): pvVarCreate() failure: "
				"%s\n", ch->varName, dbch->dbName, pvVarGetMess(dbch->pvid));
			free(ch->dbch->dbName);
			free(ch->dbch);
		}
		else
		{
			sp->assignCount++;
		}
	}

	epicsMutexUnlock(sp->lock);

	return status;
}

static pvStat seq_pvSingleMonitor(SS_ID ss, CH_ID ch, boolean turn_on, const char *what)
{
	PROG	*sp = ss->prog;
	DBCHAN	*dbch = ch->dbch;
	pvStat	status = pvStatOK;
	unsigned nss;

	if (!dbch && !optTest(sp, OPT_SAFE))
	{
		errlogSevPrintf(errlogMajor,
			"%s(%s): user error (not assigned to a PV)\n",
			what, ch->varName
		);
		return pvStatERROR;
	}
	/* for compatibility: set monitored flag for each state set */
	for (nss = 0; nss < sp->numSS; nss++)
	{
		SSCB *ss = sp->ss + nss;
		ss->monitored[chNum(ch)] = turn_on;
	}
	if (dbch)	/* named channel */
	{
		dbch->monitored = turn_on;
		status = seq_camonitor(ch, turn_on);
		if (status != pvStatOK)
		{
			pv_call_failure(dbch, metaPtr(ch,ss), status);
		}
	}
	return status;
}

/*
 * Start monitoring a channel.
 */
epicsShareFunc pvStat seq_pvMonitor(SS_ID ss, CH_ID ch)
{
	return seq_pvSingleMonitor(ss, ch, TRUE, "pvMonitor");
}

/*
 * Array variant of seq_pvMonitor.
 */
epicsShareFunc pvStat seq_pvArrayMonitor(SS_ID ss, CH_ID *chs, unsigned length)
{
	unsigned n;

	for (n=0; n<length; n++)
	{
		pvStat status = seq_pvSingleMonitor(ss, chs[n], TRUE, "pvArrayMonitor");
		if (status != pvStatOK)
			return status;
	}
	return pvStatOK;
}

/*
 * Stop monitoring a channel.
 */
epicsShareFunc pvStat seq_pvStopMonitor(SS_ID ss, CH_ID ch)
{
	return seq_pvSingleMonitor(ss, ch, FALSE, "pvStopMonitor");
}

/*
 * Array variant of seq_pvStopMonitor.
 */
epicsShareFunc pvStat seq_pvArrayStopMonitor(SS_ID ss, CH_ID *chs, unsigned length)
{
	unsigned n;

	for (n=0; n<length; n++)
	{
		pvStat status = seq_pvSingleMonitor(ss, chs[n], FALSE, "pvArrayStopMonitor");
		if (status != pvStatOK)
			return status;
	}
	return pvStatOK;
}

/*
 * Return total number of channels.
 */
epicsShareFunc unsigned seq_pvChannelCount(SS_ID ss)
{
	return ss->prog->numChans;
}

/*
 * Return number of channels connected.
 */
epicsShareFunc unsigned seq_pvConnectCount(SS_ID ss)
{
	return ss->prog->connectCount;
}

/*
 * Return number of channels assigned.
 */
epicsShareFunc unsigned seq_pvAssignCount(SS_ID ss)
{
	return ss->prog->assignCount;
}

/* Flush outstanding PV requests */
epicsShareFunc void seq_pvFlush(SS_ID ss)
{
	pvSysFlush(ss->prog->pvSys);
}

/*
 * Return whether channel is connected.
 */
epicsShareFunc boolean seq_pvConnected(SS_ID ss, CH_ID ch)
{
	if (ss->prog->options & OPT_SAFE)
		return !(ch->dbch) || ch->dbch->connected;
	else
		return ch->dbch && ch->dbch->connected;
}

/*
 * Return whether elements of a channel array are connected.
 */
epicsShareFunc boolean seq_pvArrayConnected(SS_ID ss, CH_ID *chs, unsigned length)
{
	unsigned n;

	for (n=0; n<length; n++)
	{
		if (!seq_pvConnected(ss, chs[n]))
			return FALSE;
	}
	return TRUE;
}

/*
 * Return whether channel is assigned.
 */
epicsShareFunc boolean seq_pvAssigned(SS_ID ss, CH_ID ch)
{
	return ch->dbch != NULL;
}

/*
 * Return number elements in an array, which is the lesser of
 * the array size and the element count returned by the PV layer.
 */
epicsShareFunc unsigned seq_pvCount(SS_ID ss, CH_ID ch)
{
	return ch->dbch ? ch->dbch->dbCount : ch->count;
}

/*
 * Return a channel name of an assigned variable.
 */
epicsShareFunc char *seq_pvName(SS_ID ss, CH_ID ch)
{
	return ch->dbch ? ch->dbch->dbName : NULL;
}

/*
 * Return channel alarm status.
 */
epicsShareFunc pvStat seq_pvStatus(SS_ID ss, CH_ID ch)
{
	PVMETA	*meta = metaPtr(ch,ss);
	return ch->dbch ? meta->status : pvStatOK;
}

/*
 * Return channel alarm severity.
 */
epicsShareFunc pvSevr seq_pvSeverity(SS_ID ss, CH_ID ch)
{
	PVMETA	*meta = metaPtr(ch,ss);
	return ch->dbch ? meta->severity : pvSevrOK;
}

/*
 * Return channel error message.
 */
epicsShareFunc const char *seq_pvMessage(SS_ID ss, CH_ID ch)
{
	PVMETA	*meta = metaPtr(ch,ss);
	return ch->dbch ? meta->message : "";
}

/*
 * Return channel time stamp.
 */
epicsShareFunc epicsTimeStamp seq_pvTimeStamp(SS_ID ss, CH_ID ch)
{
	PVMETA	*meta = metaPtr(ch,ss);
	if (ch->dbch)
	{
		return meta->timeStamp;
	}
	else
	{
		epicsTimeStamp ts;
		epicsTimeGetCurrent(&ts);
		return ts;
	}
}

struct getq_cp_arg {
	CHAN	*ch;
	void	*var;
	PVMETA	*meta;
};

static void *getq_cp(void *dest, const void *value, size_t elemSize)
{
	struct getq_cp_arg *arg = (struct getq_cp_arg *)dest;
	CHAN	*ch = arg->ch;
	PVMETA	*meta = arg->meta;
	void	*var = arg->var;
	pvType	type = ch->type->getType;
	size_t	count = ch->count;

	if (ch->dbch)
	{
		assert(pv_is_time_type(type));
		/* Copy status, severity and time stamp */
		meta->status = pv_status(value,type);
		meta->severity = pv_severity(value,type);
		meta->timeStamp = pv_stamp(value,type);
		count = ch->dbch->dbCount;
	}
	return memcpy(var, pv_value_ptr(value,type), ch->type->size * count);
}

/*
 * Get value from a queued PV.
 */
epicsShareFunc boolean seq_pvGetQ(SS_ID ss, CH_ID ch)
{
	PROG	*sp = ss->prog;
	void	*var = valPtr(ch,ss);
	evflag	ev_flag = ch->syncedTo;
	PVMETA	*meta = metaPtr(ch,ss);
	boolean	was_empty;
	struct getq_cp_arg arg = {ch, var, meta};

	if (!ch->queue)
	{
		errlogSevPrintf(errlogMajor,
			"pvGetQ(%s): user error (not queued)\n",
			ch->varName
		);
		return FALSE;
	}

	was_empty = seqQueueGetF(ch->queue, getq_cp, &arg);

	if (ev_flag)
	{
		epicsMutexMustLock(sp->lock);
		/* If queue is now empty, clear the event flag */
		if (seqQueueIsEmpty(ch->queue))
		{
			bitClear(sp->events, ev_flag - sp->eventFlags);
		}
		epicsMutexUnlock(sp->lock);
	}

	return (!was_empty);
}

/*
 * Flush elements on syncQ queue and clear event flag.
 */
epicsShareFunc void seq_pvFlushQ(SS_ID ss, CH_ID ch)
{
	PROG	*sp = ss->prog;
	evflag	ev_flag = ch->syncedTo;
	QUEUE	queue = ch->queue;

	DEBUG("pvFlushQ: pv name=%s, count=%d\n",
		ch->dbch ? ch->dbch->dbName : "<anomymous>", seqQueueUsed(queue));
	seqQueueFlush(queue);

	epicsMutexMustLock(sp->lock);
	/* Clear event flag */
	bitClear(sp->events, ev_flag - sp->eventFlags);
	epicsMutexUnlock(sp->lock);
}

/*
 * Test whether a given delay has expired.
 *
 * As a side-effect, adjust the state set's wakeupTime if our delay
 * is shorter than previously tested ones.
 */
epicsShareFunc boolean seq_delay(SS_ID ss, double delay)
{
	boolean	expired;
	double	now, timeExpired;

	pvTimeGetCurrentDouble(&now);
	timeExpired = ss->timeEntered + delay;
	expired = timeExpired <= now;
	if (!expired && timeExpired < ss->wakeupTime)
		ss->wakeupTime = timeExpired;

	DEBUG("delay(%s/%s,%.10f): entered=%.10f, diff=%.10f, %s\n", ss->ssName,
		ss->states[ss->currentState].stateName, delay, ss->timeEntered,
		timeExpired - now, expired ? "expired": "unexpired");
	return expired;
}

/*
 * Return the value of an option (e.g. "a").
 * FALSE means "-" and TRUE means "+".
 */
epicsShareFunc boolean seq_optGet(SS_ID ss, const char *opt)
{
	PROG	*sp = ss->prog;

	assert(opt);
	switch (opt[0])
	{
	case 'a': return optTest(sp, OPT_ASYNC);
	case 'c': return optTest(sp, OPT_CONN);
	case 'd': return optTest(sp, OPT_DEBUG);
	case 'e': return optTest(sp, OPT_NEWEF);
	case 'r': return optTest(sp, OPT_REENT);
	case 's': return optTest(sp, OPT_SAFE);
	default:  return FALSE;
	}
}

/* 
 * Given macro name, return pointer to its value.
 */
epicsShareFunc char *seq_macValueGet(SS_ID ss, const char *name)
{
	return seqMacValGet(ss->prog, name);
}

/* 
 * Immediately terminate all state sets and jump to global exit block.
 */
epicsShareFunc void seq_exit(SS_ID ss)
{
	PROG *sp = ss->prog;
	/* Ask all state set threads to exit */
	sp->die = TRUE;
	/* Take care that we die even if waiting for initial connect */
	epicsEventSignal(sp->ready);
	/* Wakeup all state sets unconditionally */
	ss_wakeup(sp, 0);
}
