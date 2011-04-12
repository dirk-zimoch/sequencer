/*	Copyright, 2010, Helmholtz-Zentrum Berlin f. Materialien
		und Energie GmbH, Germany (HZB)
		(see file Copyright.HZB included in this distribution)
*/
/* Device support to permit database access to sequencer internals
 *
 * This is experimental only. Note the following:
 *
 * 1. uses INST_IO (an unstructured string)
 *
 * 2. string is of form <seqName>.<xxx>, where <xxx> may exhibit further
 *    structure, e.g. <seqName>.nStatesets, <seqName>.<ssName>.nStates
 *
 *    <seqName>:<ssName>
 *    <seqName>:<ssName>.threadId
 *    <seqName>:<ssName>.threadIdHex
 *    <seqName>:<ssName>.timeElapsed
 *    <seqName>:<ssName>.nStates
 *    <seqName>:<ssName>.prevState
 *    <seqName>:<ssName>.nextState
 *    <seqName>:<ssName>.currentState
 *    <seqName>.nStateSets
 *    <seqName>.nAssgin
 *    <seqName>.nConnect
 *    <seqName>.nChans
 *    <seqName>.nQueues
 *    <seqName>.queues
 *    <seqName>.threadPriority
 *    <seqName>.varSize
 *
 * Orginal Auth: William Lupton, W. M. Keck Observatory
 * Modified by   Kukhee Kim, SLAC
 */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "alarm.h"
#include "dbDefs.h"
#include "recSup.h"
#include "devSup.h"
#include "link.h"
#include "dbScan.h"
#include "stringinRecord.h"
#include "epicsEvent.h"
#include "epicsExport.h"

#include "pv.h"
#include "seqCom.h"
#include "seqPvt.h"

typedef struct {
    long	number;
    DEVSUPFUN	report;
    DEVSUPFUN	init;
    DEVSUPFUN	init_record;
    DEVSUPFUN   get_ioint_info;
    DEVSUPFUN	read_or_write;
    DEVSUPFUN	special_linconv;
} DSET;

/* stringin */
LOCAL long siInit( struct stringinRecord *rec );
LOCAL long siRead( struct stringinRecord *rec );
LOCAL long siGetIoInitInfo(int cmd, struct stringinRecord *rec, IOSCANPVT *ppvt);
LOCAL DSET  devSiSeq = { 5, NULL, NULL, siInit, siGetIoInitInfo, siRead, NULL };
epicsExportAddress(dset,devSiSeq);

LOCAL void devSeqScanThreadSpawn(void);
LOCAL void devSeqScanThread(void);

LOCAL epicsThreadOnceId devSeqScanThreadOnceFlag = EPICS_THREAD_ONCE_INIT;
LOCAL char*             devSeqScanThreadName     = "devSeqScan";
LOCAL ELLLIST           devSeqScanList;

LOCAL  char *nullStr    = "";

/* Commands for Second field */
LOCAL  char *nStateSets     = "nStateSets";
LOCAL  char *nAssign        = "nAssign";
LOCAL  char *nConnect       = "nConnect";
LOCAL  char *nChans         = "nChans";
LOCAL  char *nQueues        = "nQueues";
LOCAL  char *queues        = "queues";
LOCAL  char *threadPriority = "threadPriority";
LOCAL  char *varSize        = "varSize";

/* Commands for Third field */
LOCAL  char *threadId       = "threadId";
LOCAL  char *threadIdHex    = "threadIdHex";
LOCAL  char *timeElapsed    = "timeElapsed";
LOCAL  char *nStates        = "nStates";
LOCAL  char *firstState     = "firstState";
LOCAL  char *prevState      = "prevState";
LOCAL  char *nextState      = "nextState";
LOCAL  char *currentState   = "currentState";

LOCAL  char *notFoundMsg    = "Not Found";
LOCAL  char *syntaxErrMsg   = "Syntax Error in INP field";

typedef enum{
    notUpdated =0,
    updated,
    notFound
} updateFlagMenu;

typedef enum {
    seqShownStateSets,
    seqShownAssign,
    seqShownConnect,
    seqShownChans,
    seqShownQueues,
    seqShowpQueues,
    seqShowthreadPriority,
    seqShowvarSize,
    seqShowthreadId,
    seqShowthreadIdHex,
    seqShowtimeElapsed,
    seqShownStates,
    seqShowfirstState,
    seqShowprevState,
    seqShownextState,
    seqShowcurrentState,
    seqShowsyntaxErr
} seqShowVarType;

typedef union {
    long          numSS;
    long          assignCount;
    long          connectCount;
    long          numChans;
    int           numQueues;
    QUEUE         *queues;
    unsigned int  threadPriority;
    long          varSize;

    epicsThreadId threadId;
    double        timeElapsed;
    long          numStates;
    const char    *firstStateName;
    const char    *prevStateName;
    const char    *nextStateName;
    const char    *currentStateName;

    char          *syntaxErrMsg;
} seqShowVar;

typedef struct {
    ELLNODE               devScanNode;
    IOSCANPVT             ioScanPvt;
    seqShowVarType        type;
    SPROG                 *sp;
    SSCB                  *ss;
    STATE                 *st;
    char                  progName[80];
    char                  stateSetName[80];
    char                  updateFlag;
    epicsMutexId          mutexId;
    seqShowVar            var;
} seqShowScanPvt;

#define UPDATE_SEQSHOW_VAR(UPDATEFLAG, SOURCE, TARGET) \
        if((UPDATEFLAG) || ((TARGET) != (SOURCE))) {\
            (TARGET) = (SOURCE); \
            (UPDATEFLAG) = updated; \
        }

LOCAL seqShowScanPvt* seqShowScanPvtInit(struct link* link)
{
    seqShowScanPvt   *pvtPt;
    char             inpStr[strlen(link->value.instio.string)+1];
    char             *inpArg[3];
    int              argN  = 0;
    int              i     = 0;

    pvtPt = new(seqShowScanPvt);
    if(!pvtPt) return NULL;

    pvtPt->updateFlag = 1;
    pvtPt->sp        = NULL;
    pvtPt->mutexId    = epicsMutexCreate();
    scanIoInit(&pvtPt->ioScanPvt);

    strcpy(inpStr,link->value.instio.string);

    /* Find deliminater in INP string
       and replace null characeter to point end of string.
       And assign to sperated strings */
    while(argN<3) {
        if(inpStr[i] == '\0') break;
        inpArg[argN++] = inpStr+i;
        while(i<80) {
            char *tempChar = inpStr + (i++);
            if((*tempChar < '0' || *tempChar > '9') &&
               (*tempChar < 'a' || *tempChar > 'z') &&
               (*tempChar < 'A' || *tempChar > 'Z') ) {
                    if(*tempChar == '\0') i-=1;
                    *tempChar = '\0';
                    break;
            }
        }
    }
    for(i=argN;i<3;i++) inpArg[i] = nullStr;

    strcpy(pvtPt->progName,inpArg[0]);
    strcpy(pvtPt->stateSetName,nullStr);
    if(!strcmp(inpArg[1],nStateSets))          pvtPt->type = seqShownStateSets;
    else if(!strcmp(inpArg[1],nAssign))        pvtPt->type = seqShownAssign;
    else if(!strcmp(inpArg[1],nConnect))       pvtPt->type = seqShownConnect;
    else if(!strcmp(inpArg[1],nChans))         pvtPt->type = seqShownChans;
    else if(!strcmp(inpArg[1],nQueues))        pvtPt->type = seqShownQueues;
    else if(!strcmp(inpArg[1],queues))         pvtPt->type = seqShowpQueues;
    else if(!strcmp(inpArg[1],threadPriority)) pvtPt->type = seqShowthreadPriority;
    else if(!strcmp(inpArg[1],varSize))        pvtPt->type = seqShowvarSize;
    else {
        strcpy(pvtPt->stateSetName,inpArg[1]);
        if(!strcmp(inpArg[2],threadId))          pvtPt->type = seqShowthreadId;
        else if(!strcmp(inpArg[2],threadIdHex))  pvtPt->type = seqShowthreadIdHex;
        else if(!strcmp(inpArg[2],timeElapsed))  pvtPt->type = seqShowtimeElapsed;
        else if(!strcmp(inpArg[2],nStates))      pvtPt->type = seqShownStates;
        else if(!strcmp(inpArg[2],firstState))   pvtPt->type = seqShowfirstState;
        else if(!strcmp(inpArg[2],prevState))    pvtPt->type = seqShowprevState;
        else if(!strcmp(inpArg[2],nextState))    pvtPt->type = seqShownextState;
        else if(!strcmp(inpArg[2],nullStr) || !strcmp(inpArg[2],currentState))      
                                                 pvtPt->type = seqShowcurrentState;
        else                                     pvtPt->type = seqShowsyntaxErr;
    } 
    return pvtPt;
}

LOCAL void devSeqScanThreadSpawn(void) 
{
    epicsUInt32 devSeqScanStack;

    /* Init Linked List */
    ellInit(&devSeqScanList);

    /* Spawn the Scan Task */
    devSeqScanStack = epicsThreadGetStackSize(epicsThreadStackMedium);
    epicsThreadCreate(devSeqScanThreadName, THREAD_PRIORITY, devSeqScanStack,
                      (EPICSTHREADFUNC)devSeqScanThread,NULL);
}

LOCAL void devSeqScanThread(void)
{
    ELLLIST          *pdevSeqScanList = &devSeqScanList;
    seqShowScanPvt   *pvtPt;
    seqShowVar       *varPt;
    int              i;
    double           timeNow;


    while(!pdevSeqScanList->count) {
        epicsThreadSleep(0.5);
    }

    while(TRUE) {
        pvtPt = (seqShowScanPvt*) ellFirst(pdevSeqScanList);
        do {
            pvtPt->sp = seqFindProgByName(pvtPt->progName, 0);

            if(!pvtPt->sp) continue;
            varPt = &(pvtPt->var);

            epicsMutexLock(pvtPt->mutexId);
            switch(pvtPt->type){
                case seqShownStateSets:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->numSS, varPt->numSS)
                    break;
                case seqShownAssign:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->assignCount, varPt->assignCount)
                    break;
                case seqShownConnect:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->connectCount, varPt->connectCount)
                    break;
                case seqShownChans:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->numChans, varPt->numChans)
                    break;
                case seqShownQueues:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->numQueues, varPt->numQueues)
                    break;
                case seqShowpQueues:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->queues, varPt->queues)
                    break;
                case seqShowthreadPriority:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->threadPriority, varPt->threadPriority)
                    break;
                case seqShowvarSize:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->sp->varSize, varPt->varSize)
                    break;
                 default:
                    for(i=0, pvtPt->ss = pvtPt->sp->ss; i < pvtPt->sp->numSS; i++, (pvtPt->ss)++) {
                        if(!strcmp(pvtPt->ss->ssName, pvtPt->stateSetName)) break;
                    }
                    if(i >= pvtPt->sp->numSS) { 
                        pvtPt->updateFlag = notFound;
                        epicsMutexUnlock(pvtPt->mutexId);
                        scanIoRequest(pvtPt->ioScanPvt);
                        continue;
                    }
                    break;
        }
        switch(pvtPt->type){
                case seqShowthreadId:
                case seqShowthreadIdHex:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->ss->threadId, varPt->threadId)
                    break;
                case seqShowtimeElapsed:
                    pvTimeGetCurrentDouble(&timeNow);
                    varPt->timeElapsed = timeNow - pvtPt->ss->timeEntered;
                    pvtPt->updateFlag = 1; 
                    break;
                case seqShownStates:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->ss->numStates, varPt->numStates)
                    break;
                case seqShowfirstState:
                    pvtPt->st = pvtPt->ss->states;
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->st->stateName, varPt->firstStateName)
                    break;
                case seqShowprevState:
                    pvtPt->st = pvtPt->ss->states + pvtPt->ss->prevState;
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, 
                                       pvtPt->ss->prevState >= 0 ? pvtPt->st->stateName: nullStr, 
                                       varPt->prevStateName)
                    break;
                case seqShownextState:
                    pvtPt->st = pvtPt->ss->states + pvtPt->ss->nextState;
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, 
                                       pvtPt->ss->nextState >= 0 ? pvtPt->st->stateName: nullStr, 
                                       varPt->nextStateName)
                    break;
                case seqShowcurrentState:
                    pvtPt->st = pvtPt->ss->states + pvtPt->ss->currentState;
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, pvtPt->st->stateName, varPt->currentStateName)
                    break;
                case seqShowsyntaxErr:
                    UPDATE_SEQSHOW_VAR(pvtPt->updateFlag, syntaxErrMsg, varPt->syntaxErrMsg);
                    break;
                default:
                    break;
            }
            epicsMutexUnlock(pvtPt->mutexId);

            if(pvtPt->updateFlag) { 
                pvtPt->updateFlag = notUpdated;
                scanIoRequest(pvtPt->ioScanPvt);
            }

        } while( (pvtPt = (seqShowScanPvt*) ellNext(&pvtPt->devScanNode)) );
        epicsThreadSleep(0.1);
    }
}


LOCAL long siInit( struct stringinRecord *rec )
{
    struct link      *link   = &rec->inp;

    /* check that link is of type INST_IO */
    if ( link->type != INST_IO ) {
	return -1;
    }

    rec->dpvt = (void *) seqShowScanPvtInit(link);
    if(!rec->dpvt) return -1;

    epicsThreadOnce(&devSeqScanThreadOnceFlag, (void(*)(void *)) devSeqScanThreadSpawn, (void *) NULL);
   
    ellAdd(&devSeqScanList, &(((seqShowScanPvt *) rec->dpvt)->devScanNode));
   
    return 0;
}

LOCAL long siRead( struct stringinRecord *rec )
{
    seqShowScanPvt    *pvtPt = (seqShowScanPvt *)rec->dpvt;
    seqShowVar        *varPt;

    if(!pvtPt || pvtPt->updateFlag == notFound || !pvtPt->sp ) { 
        strcpy(rec->val, notFoundMsg); 
        return 0; 
    }
    varPt = &(pvtPt->var);
   
    epicsMutexLock(pvtPt->mutexId);
    switch(pvtPt->type){
        case seqShownStateSets:     sprintf(rec->val, "%ld", varPt->numSS);                      break;
        case seqShownAssign:        sprintf(rec->val, "%ld", varPt->assignCount);                break;
        case seqShownConnect:       sprintf(rec->val, "%ld", varPt->connectCount);               break;
        case seqShownChans:         sprintf(rec->val, "%ld", varPt->numChans);                   break;
        case seqShownQueues:        sprintf(rec->val, "%d", varPt->numQueues);                   break;
        case seqShowpQueues:        sprintf(rec->val, "%p", varPt->queues);                     break;
        case seqShowthreadPriority: sprintf(rec->val, "%d", varPt->threadPriority);              break;
        case seqShowvarSize:        sprintf(rec->val, "%ld", varPt->varSize);                    break;
        case seqShowthreadId:       sprintf(rec->val, "%lu", (unsigned long) varPt->threadId);   break;
        case seqShowthreadIdHex:    sprintf(rec->val, "0x%lx", (unsigned long) varPt->threadId); break;
        case seqShowtimeElapsed:    sprintf(rec->val, "%.3f", varPt->timeElapsed);               break;
        case seqShownStates:        sprintf(rec->val, "%ld", varPt->numStates);                  break;
        case seqShowfirstState:     strcpy(rec->val, varPt->firstStateName);                    break;
        case seqShowprevState:      strcpy(rec->val, varPt->prevStateName);                     break;
        case seqShownextState:      strcpy(rec->val, varPt->nextStateName);                     break;
        case seqShowcurrentState:   strcpy(rec->val, varPt->currentStateName);                  break;
        case seqShowsyntaxErr:      strcpy(rec->val, varPt->syntaxErrMsg);                      break;
    }
    epicsMutexUnlock(pvtPt->mutexId);

    return 0;
}

LOCAL long siGetIoInitInfo(int cmd, struct stringinRecord *rec, IOSCANPVT *ppvt)
{
    seqShowScanPvt  *pvtPt = (seqShowScanPvt *)rec->dpvt;

    *ppvt = pvtPt->ioScanPvt;

    return 0;
}
