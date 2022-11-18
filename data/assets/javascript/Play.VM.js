// ----------------------------------------
// the slideshow VM


// all timings are in seconds

// ----------------------------------------
// stati

const stCreated    = "created";     // job ready to run
const stReadypage  = "pageready";   // Json page for img loaded
const stReadymedia = "mediaready";  // img loaded
const stRendered   = "rendered";    // DOM element created
const stVisible    = "visible";     // DOM element visible
const stShown      = "shown";       // end of showing img
const stHidden     = "hidden";      // DOM element no longer visible
const stFinished   = "finished";    // job terminated
const stAborted    = "aborted";     // job terminated by abortion

const statiWords = [
    stCreated,
    stReadypage,
    stReadymedia,
    stRendered,
    stVisible,
    stShown,
    stHidden,
    stFinished,
    stAborted,
];

// set of stati

function emptySt() {
    return new Set();
};
function allSt() {
    return new Set(statiWords);
}
function addSt(st, stats) {
    return stats.add(st);
};
function memberSt(st, stats) {
    return stats.has(st);
}

// ----------------------------------------
// opcodes

const opInit      = "init";
const opType      = "type";
const opFrame     = "frame";
const opText      = "text";
const opPath      = "path";
const opLoadpage  = "loadpage";
const opLoadmedia = "loadmedia";
const opRender    = "render";
const opFadeout   = "fadeout";
const opFadein    = "fadein";
const opMove      = "move";
const opPlace     = "place";    // move without animation
const opStatus    = "status";
const opDelay     = "delay";
const opWait      = "wait";
const opWaitclick = "waitclick";
const opFinish    = "finish";

const opCodes = [
    opInit,
    opType,
    opFrame,
    opText,
    opPath,
    opLoadpage,
    opLoadmedia,
    opRender,
    opPlace,
    opMove,
    opFadeout,
    opFadein,
    opDelay,
    opWait,
    opWaitclick,
    opStatus,
    opFinish,
];

const tyText = "text";
const tyImg  = "img";

const tyWords = [
    tyText,
    tyImg,
];

// ----------------------------------------
// transitions

const trCut       = 'cut';
const trFadein    = 'fadein';
const trFadeout   = 'fadeout';
const trCrossfade = 'crossfade';

const trWords     = [ trCut, trFadein, trFadeout, trCrossfade ];
const trDefault   = trCut;

// ----------------------------------------
// text align

const alnLeft     = 'left';
const alnCenter   = 'center';
const alnRight    = 'right';

const alnWords    = [ alnLeft, alnCenter, alnRight ];
const alnDefault  = alnLeft;

// ----------------------------------------

// build a whole progam with jobs and nested
// subprograms

function mkVMProg(localBlocks, code) {
    return { code:   code,         // list of simple instructions
             blocks: localBlocks,  // list of local blocks
           };
};

function mkVMProg0(code) {
    return mkVMProg([], code);
};

function mkVMMain(...jobs) {
    return mkVMProg([...jobs], [mkInit('main'), mkFinish()]);
}

function getVMProg(jno) {
    let xs  = jnoToIntList(jno);
    let res = vmProg;                     // acces global vmProg variable
    while ( xs.length > 0 ) {
        const ix = xs.shift();
        res = res.blocks[ix - 1];
    }
    return res;
}

function getVMCode(jno)   {return getVMProg(jno).code;}
function getVMBlocks(jno) {return getVMProg(jno).blocks;}

function getLastInstr(predicate) {

    function get1(aj) {
        const jno = aj.jno;
        const jpc = aj.jpc;
        const c   = getVMCode(jno);

        for (let i = jpc; i >= 0; i--) {
            const instr = c[i];
            if ( predicate(instr) ) {
                trc(1, `getLastInstr: jpc: ${i} instr=${PP.instr(instr)}`);
                return instr;
            }
        }
        trc(1, `getLastInstr: no instr found`);
        return null;
    }
    return get1;
}

const getTextInstr   = getLastInstr((i) => {return i.op === opText;});
const getTypeInstr   = getLastInstr((i) => {return i.op === opType;});
const getLastGSInstr = getLastInstr((i) => {return isDefined(i.gs);});

function getLastGS() {
    const i = getLastGSInstr(vmActiveJob);
    return i ? i.gs : defaultGS();
}

function getLastTextInstr() {
    return getTextInstr(vmActiveJob);
}

function getLastType() {
    return getTypeInstr(vmActiveJob).type;
}

function getGeoSpecs(jno) {
    const code = getVMCode(jno);
    let   gss  = [];
    for (i of code) {
        if ( isDefined(i.gs) ) {
            gss.push(i.gs);
        }
    }
    return gss;
}

// ----------------------------------------
//
// code edit ops

// replace the code part of a job with a new instr sequence
// without destroying the object reference of the code array

function replaceVMCode(jno, code) {
    const c = getVMCode(jno);
    c.splice(0, c.length, ...code);
}

// reload the code for given jno and all sub jobs
// reset local jpc and add jobs to ready queue
// works on global vmProg variable

function reloadVMCode(jno0) {

    function readyJobs(prog, jno) {
        trc(1,`readyJobs: ${jno} ${prog.code.length} ${prog.blocks.length}`);
        addJob(jno, prog.code);
        addReady(jno);

        const subprogs = prog.blocks;
        for (let i = 0; i < subprogs.length; i++) {
            readyJobs(subprogs[i],
                      mkJno(jno, i + 1),    // job # run from 1
                     );
        }
    }

    // set VM state to interrupted
    const irupt = vmInterrupted;
    vmInterrupted = true;

    const prog0 = getVMProg(jno0);
    readyJobs(prog0, jno0);

    // restore VM interrupted flag
    vmInterrupted = irupt;
}

// ----------------------------------------
//
// build instructions

function mkInit(name) {
    return { op:   opInit,
             name: name,
           };
}

function mkFinish() {
    return { op: opFinish };
};

function mkType(type) {
    return { op:   opType,
             type: type,
           };
}

function mkFrame(gs) {
    return { op: opFrame,
             gs: gs,
           };
}

function mkPath(path) {
    return { op:   opPath,
             path: path,
           };
}

function mkText(align, text) {
    return { op:    opText,
             align: align,
             text:  text,
           };
}

function mkLoadpage() {
    return { op:  opLoadpage };
}

function mkLoadmedia() {
    return { op: opLoadmedia };
}

function mkRender(gs) {
    return { op: opRender,
             gs: gs
           };
}

function mkFadein(dur, trans, job) {
    return { op:    opFadein,
             dur:   dur,
             trans: trans,
             job: job || 'prev',
           };
};

function mkFadeout(dur, trans) {
    return { op:    opFadeout,
             dur:   dur,
             trans: trans,
           };
};

function mkStatus(st) {
    return { op: opStatus,
             st: st,
           };
};

function mkMove(dur, gs) {
    return { op:  opMove,
             dur: dur,
             gs:  gs
           };
};

function mkPlace(gs) {
    return { op: opPlace,
             gs: gs
           };
};

function mkDelay(dur) {
    return { op:  opDelay,
             dur: dur
           };
}

function mkWaitclick() {
    return { op: opWaitclick
           };
}

function mkWait(st, job) {
    return { op:     opWait,
             job:    job,
             status: st,
           };
};

function mkSet(name, value) {return {op: 'set', name:  name, value: value};}
function mkErrmsg(msg)      {return {op: 'errmsg', msg: msg};}
function mkAbort()          {return {op: 'abort'} ;}
function mkTerminate()      {return {op: 'terminate'};}
function mkExit()           {return {op: 'exit'};}
function mkFadeinCut()      {return {op: 'fadeincut'};}
function mkFadeoutCut()     {return {op: 'fadeincut'};}
function mkFadeinAnim(dur)  {return {op: 'fadeinanim',  dur: dur};}
function mkFadeoutAnim(dur) {return {op: 'fadeoutanim', dur: dur};}

// type Job = [Instr]
// type Jobno = Int
// type activeJob = { jno: n, jpc: n, }

function noop() {}

// ----------------------------------------
//
// basic compile macros

function cInit(name) {
    return [
        mkInit(name),
    ];
}

function cTerm() {
    return [
        mkFinish(),
    ];
}

function cLoadImg(imgPath, frameGS, gs, jnoWait) {
    return [
        mkType('img'),
        mkPath(imgPath),
        mkFrame(frameGS || defaultFrameGS()),
        mkLoadpage(),
        mkWait(stReadymedia, jnoWait || 'prev'), // wait for prev job loading media
        mkLoadmedia(),
        mkRender(gs),                 // 1. geo is initial geo
    ];
}

function cLoadText(frameGS, text, gs, align) {
    const gs1 = {...gs, alg: 'fix'};
    return [
        mkType('text'),
        mkWait(stRendered, 'par'),
        mkFrame(frameGS || defaultFrameGS()),
        mkText(align || 'center' , text || "???"),
        mkRender(gs1),
        mkWait(stVisible, 'par'),
    ];
}

function cView(dur) {
    let is = [];
    if ( isPositive(dur) ) {
        is.push(mkDelay(dur));
    }
    if ( dur === 'click' ) {
        is.push(mkWaitclick());
    }
    return is;
}

function cMove(dur, gs) {
    if ( isPositive(dur) ) {
        return [
            mkMove(dur, gs),
        ];
    }
    else {
        return [
            mkPlace(gs),
        ];
    }
}

function cFadein(dur, fade0, waitJob) {
    const fade =
          isPositive(dur)
          ? fade0
          : trCut;

    switch ( fade ) {
    case 'fadein':
        return [
            ...cWait(stHidden, waitJob),   // wait prev image to be hidden
            mkFadein(dur, trFadein),
        ];
    case 'crossfade':
        return [
            ...cWait(stShown, waitJob),   // wait prev image starts fading out
            mkFadein(dur, trFadein),
        ];
    case trCut:
    default:
        return [
            ...cWait(stShown, waitJob),
            mkFadein(0, trCut),
        ];
    }
}

function cWait(st, waitJob) {
    if ( waitJob === 'nowait' )
        return [];
    const prevJob = waitJob || 'prev';
    return [
        mkWait(st, waitJob || 'prev')
    ];
}

function cFadeout(dur, fade) {
    return [mkFadeout(dur, fade)];
}

// --------------------
//
// structured compile macros

// frame a job with setup and view into a complete job

function cJob(name, cSetup, cView) {
    return [
        ...cInit(name),
        ...cSetup,
        ...cView,
        ...cTerm()
    ];
}

// --------------------
//
// setup macros

// text slide with align/offset spec and text
//
// example: cLoadText({dir: 'NW', shift: V2(0.1,0.2)}, '<h1>Hello</h1>')
//   places the text 'Hello' redered as a 'H1' elem
//   with 10% of stage width as padding to the left
//   and 20% of stage height as padding at the top of the stage

function cLoadText1(text, gs) {
    gs = gs || defaultGS();
    const gs1 = {...gs, alg: 'fix'};
    return cLoadText(defaultFrameGS(), text, gs1);
}

function cLoadImgStd(imgPath, gs) {
    return cLoadImg(imgPath, defaultFrameGS(),gs);
}

// --------------------
//
// view macros

// a slide view: fadein, fadeout and view steps

function cViewStd0(fadeinDur, fadeinTr, fadeoutDur, fadeoutTr, cView, waitJob) {
    return [
        ...cFadein(fadeinDur, fadeinTr, waitJob),
        ...cView,
        ...cFadeout(fadeoutDur, fadeoutTr)
    ];
}

// a standard slide view: fadein, view, fadeout

function cViewStd(fadeinDur, fadeinTr, dur, fadeoutDur, fadeoutTr, waitJob) {
    return cViewStd0(fadeinDur, fadeinTr,
                     fadeoutDur, fadeoutTr,
                     cView(dur), waitJob,
                    );
}

// a standard view with crossfade in/out

function cViewCrossfade(dur, fadeDur) {
    return cViewStd(fadeDur, trCrossfade, dur, fadeDur, trCrossfade);
}

// ----------------------------------------
// virtual machine code interpretation

// create active job control object
//
// jno:     hierachical job number
// jpc:     instruction counter pointing into code array
// jstatus: set of status values representing the progress of the job
//          used for synchronizing jobs

function mkActiveJob(jno) {
    return { jno     : jno,
             jpc     : 0,
             jstatus : new Set(),
             jmicros : [],
    };
}

function abortActiveJob(aj) {
    // jpc points behind last instr
    // and stack of micros is empty
    // --> nextMicroInstr will return Nothing

    aj.jpc     = jobsCode.get(aj.jno).length;
    aj.jmicros = [];
}

function nextMicroInstr(aj) {
    if ( isEmptyList(aj.jmicros) ) {
        // stack of micro instructions is empty
        // expand current instr into a sequence of
        // micro instructions and push them
        // onto .jmicros stack

        const instr = jobsCode.get(aj.jno)[aj.jpc++];
        if ( ! instr ) {
            // no more instruction
            trc(1,`(${aj.jno}, ${aj.jpc}): job finished`);
            return Nothing;
        }
        trc(1, `(${aj.jno}, ${aj.jpc}): ${instr.op}`);

        expandToMicros(instr, aj.jmicros);
    }
    // .jmicros is not empty
    return aj.jmicros.pop();
}

// dummy expandToMicros
function expandToMicros(instr, micros) {
    micros.push(instr);
}

function addStatus(aj, st) {
    aj.jstatus.add(st);
}

function hasStatus(aj, st) {
    return aj.jstatus.has(st)
        || aj.jstatus.has(stFinished);

    // stFinished is the super status
    // finished jobs don't block any other jobs
}

function mkWaitJob(jno, jstatus) {
    return { jno     : jno,
             jstatus : jstatus
           };
};

// ----------------------------------------
//
// Jno ops

const jno0 ='';

function mkJno(jno, i) {
    return jno + '.' + i;
}

function jnoToIntList(jno) {
    let l = jno.split('.');
    l.shift();
    return map(toNum)(l);
}

function jnoFromIntList(l) {
    const m = map((i) => {return "." + i;})(l);
    return intercalate('', m);
}

function jnoPrev(jno) {
    let l = jnoToIntList(jno);
    let i = l.pop();
    if ( i > 1 ) {
        l.push(i - 1);
    }
    return jnoFromIntList(l);
}

function jnoPar(jno) {
    let l = jnoToIntList(jno);
    l.pop();
    return jnoFromIntList(l);
}

function jnoChildren(jno) {

}

function jnoPrevN(jno, n) {
    if ( isNumber(n) ) {
        for (i = 0; i < n; i++) {
            jno = jnoPrev(jno);
        }
        return jno;
    }
    // if ( isString(n) ) {
        trc(1, `jnoPrevN: job names (${n}) not yet implemented`);
        return jno0;
    // }

}

function jnoToId(jno) {
    return jno.replace(/[.]/g,'-');
}

function jnoFromJobName(jno, n) {
    if ( n === 'prev' ) {
        return jnoPrev(jno);
    }
    if ( n === 'par' ) {
        return jnoPar(jno);
    }
    if ( isNegative(n) ) {
        return jnoPrevN(jno, 0 - n);
    }
    if ( isString(n) ) {
        for (const kv of jobsData) {
            if ( kv[1] === n ) {
                return kv[0];
            }
        }
    }
    return jno0;
}

function isTopLevelJno(jno) {return jnoToIntList(jno).length <= 1;}
function isRootJno    (jno) {return jno === jno0;}

// ----------------------------------------

function initVMCode(prog0) {
    // vmInterrupted = true;    // addReady does not start jobs immediatly
    vmProg = prog0;             // store job hierachy in global VM variable
    reloadVMCode(jno0);         // flatten nested jobs and init jobs
}

function restartVM() {
    vmInterrupted = false;
    run();
}

// machine state: global

var vmInterrupted; // :: Bool
var vmRunning;     // :: Bool
var vmStepCnt;     // :: Int
var vmActiveJob;   // :: (Jno, Jpc, Set Status)

// the hierachically structured job numbers
// type Jno    = [Nat]

// the hierachically organized set of jobs
// type VMCode = ([VMCode], [Instr])


var vmProg;        // :: VMCode
var jobsCode;      // :: Map Jno [Instr]        // instruction cache
var jobsData;      // :: Map Jno JobLocalData   // local job data
var jobsAll;       // :: Map Jno ActiveJob      // the control vars of a job

// job sync map: every job has an associated table
// with the job stati to be reached to wakeup other jobs
var jobsWaiting;   // :: Map Jno (Map Status (Set Jno))

// the jobs waiting for IO, e.g. clicks to continue
var jobsInput;     // :: Queue Jno

// the jobs ready to be executed
var jobsReady;     // :: Queue Jno

// the jobs running currently in a Javascript asyn functions
// and will be terminated and put back into the ready queue by a callback
var jobsRunning;   // :: Set Jno

// initialize al VM variables
function resetVM() {
    vmInterrupted = true;
    vmRunning     = false;
    vmStepCnt     = 0;
    vmActiveJob   = null;
    jobsCode      = new Map();  // Map Jno [Instr]
    jobsData      = new Map();  // Map Jno JobData
    jobsAll       = new Map();  // Map Jno ActiveJob
    jobsWaiting   = new Map();  // Map Jno (Map Status (Set Jno))
    jobsInput     = [];         // Queue Jno
    jobsReady     = [];         // Queue Jno
    jobsRunning   = new Set();  // Set Jno
}

resetVM();

// --------------------
// jobs:

function addJob(jno, jcode) {
    jobsCode.set(jno, jcode);
    jobsData.set(jno, {});
    jobsAll.set(jno, mkActiveJob(jno));
}

function remJob(jno) {
    jobsCode.delete(jno);
    jobsData.delete(jno);
    jobsAll.delete(jno);
};

function getCode(jno) {
    return jobsCode.get(jno);
}

function replaceCode(jno, code) {  // the array in jobsCode is modified
    if ( jobsCode.has(jno) ) {     // not replaced by a a new array
        let c = jobsCode.get(jno);
        c.splice(0, c.length, ...code);
    }
    else {
        jobsCode.set(jno, code);
    }
}

function getData(jno) {
    return jobsData.get(jno);
}

function noMoreJobs() {
    return ! readyJobs() && ! runningJobs() && ! waitingForInputJobs();
}

function restartJob(jno, jcode) {
    removeFrame(jno);
    replaceVMCode(jno, jcode);
    reloadVMCode(jno);
    restartVM();
}

function restartProg(jno, jprog) {

    // whole VM code is changed, new init
    if ( jno === jno0 ) {
        resetVM();
        initVMCode(jprog);
        restartVM();
    }
                                  // a single job is replaced
    const p  = getVMProg(jno);    // get the program block
    p.code   = jprog.code;        // update code
    p.blocks = jprog.blocks;      // replace local jobs

    removeFrame(jno);             // remove DOM stuff for job and subjobs
    reloadVMCode(jno);            // reload and reinit job and subjobs
    restartVM();                  // push the red button
}
// --------------------
// jobsRunning:

function addAsyncRunning(jno) {
    trc(1, `addAsyncRunning: add to async running jobs: ${jno}`);
    jobsRunning.add(jno);
}
function remAsyncRunning(jno) {
    jobsRunning.delete(jno);
}
function runningJobs() {
    return jobsRunning.size > 0;
}

// --------------------
// jobsReady:

function addReady(jno) {
    // in restart after code edit
    // jno maybe already in queue
    if ( ! jobsReady.includes(jno) ) {
        jobsReady.push(jno);           // add at end of job queue
        trc(1, `addReady: ${jno}`);
    }
    run();                             // new ready job, (re)start VM
}

function nextReady() {
    const jno = jobsReady.shift();      // get and rem 1. elem of job queue
    return jobsAll.get(jno);
}

function readyJobs() {
    return jobsReady.length > 0;
}

// --------------------
// jobsWaiting
//
// for lookup jobsWaiting the WaitJob objects must be serialized

function addWaiting(jno, waitFor) {
    const jno1    = waitFor.jno;
    const status1 = waitFor.jstatus;

    const stmap   = jobsWaiting.get(jno1) || new Map();
    const jnoset  = stmap.get(status1)  || new Set();
    const stmap1  = stmap.set(status1, jnoset.add(jno));
    jobsWaiting.set(jno1, stmap1);
    trc(1, `addWaiting: job ${jno} waits for (${jno1},${status1})`);
}

function wakeupWaiting(waitFor) {
    const jno1    = waitFor.jno;
    const status1 = waitFor.jstatus;
    // trc(1,`wakeupWaiting: (${jno1},${status1})`);

    const stmap   = jobsWaiting.get(jno1);    // lookup waiting jobs for jno1
    if ( stmap ) {
        const jnos = stmap.get(status1);      // lookup waiting jobs for status1
        if ( jnos ) {                         // jobs found
            trc(1,`wakeupWaiting: (${jno1},${status1})`);
            stmap.delete(status1);            // cleanup map of waiting jobs
            if ( stmap.size === 0) {
                jobsWaiting.delete(jno1);
            }
            jnos.forEach((jno) => {           // wakeup all jobs
                trc(1,`wakup: job ${jno}`);   // waiting for jno1 reaching
                addReady(jno);                // status1
            });
        }
    }
}

function wakeupAllWaiting(jno1) {
    trc(1,`wakeupAllWaiting: ${jno1}`);

    const stmap = jobsWaiting.get(jno1);
    if ( stmap ) {
        let sts = [];
        for (const s of stmap.keys()) {
            sts.push(s);
        }
        for (const s1 of sts) {
            wakeupWaiting(mkWaitJob(jno1, s1));
        }
    }
}

function waitingJobs() {
    return jobsWaiting.size > 0;
}

// --------------------

function addInputJob(jno) {
    trc(1,`addInputJob: job ${jno} inserted into waiting for click queue`);
    jobsInput.push(jno);
}

function wakeupInputJobs() {
    jobsInput.forEach((jno) => {
        trc(1,`wakeupInputJobs: job ${jno} moved to ready jobs`);
        addReady(jno);
    });
    jobsInput = [];
}

function waitingForInputJobs() {
    return jobsInput.length > 0;
}

// --------------------

// resume from interrupted state

function resumeFromInput() {
    vmInterrupted = false;
    wakeupInputJobs();
}

function run() {
    if ( vmRunning || vmInterrupted ) {
        // trc(1, "run: VM already running");
        return;
    }
    else {
        trc(1, "run: (re)start VM");
        run1();
    }
}

function run1() {
    vmRunning = true;
    do {
        // trc(1, `${vmStepCnt}. step started`);
        res = step1();
        if ( res === 'step') {
            // trc(1, `${vmStepCnt}. step finished`);
        }
        vmStepCnt++;
    }
    while ( res === 'step' );

    vmRunning = false;
    trc(1, `${vmStepCnt - 1}. step: res = ${res}`);
}

function step1() {
    if ( readyJobs() ) {
        let aj = nextReady();
        let mi = nextMicroInstr(aj);

        // trc(1, `(${aj.jno}, ${aj.jpc}): mi: ${JSON.stringify(mi)}`);
        if ( ! isNothing(mi) ) {
            execInstr(mi, aj);
        }                        // else job is finished
        return 'step';
    }
    else if ( runningJobs() ) {
        return 'running';
    }
    else if ( waitingForInputJobs() ){
        return 'waiting for input';
    }
    else if ( waitingJobs() ){
        return 'blocked';
    }
    else {
        return 'terminated';
    }
}

function execMicroInstr(mi, aj) {
    const jno = aj.jno;
    const op  = mi.op;
    const ms  = aj.jmicros;

    trc(1, `(${jno}, ${aj.jpc}): ${JSON.stringify(mi)} ${JSON.stringify(ms)}}`);

    switch ( op ) {

    // --------------------
    // micro instructions
    case 'set':
        getData(jno)[mi.name] = mi.value;
        break;

    case 'errmsg':
        statusBar.show(mi.msg);
        break;

    case opStatus:
        setStatus(aj, mi.st);
        break;

    case 'fadeincut':
        fadeinCut(aj);
        break;

    case 'fadeinanim':
        fadeinAnim(aj, mi.dur);
        break;

    case 'fadeoutcut':
        fadeinCut(aj);
        break;

    case 'fadeoutanim':
        fadeoutAnim(aj, mi.dur);
        break;

    case 'exit':
        abortActiveJob(aj);
        break;

    // --------------------
    // macro instructions

    case 'terminate':
        ms.push(
            mkExit(),
            mkStatus(stFinished),
        );
        break;

    case 'abort':
        ms.push(
            mkTerminate(),
            mkStatus(stAborted),
            mkErrmsg(`job ${jno} is aborted`),
        );
        break;

    case opFadein:
        ms.push(mkStatus(stVisible));
        if (mi.trans === trCut || ! isPositive(mi.dur)) {
            ms.push(mkFadeinCut(),
                    mkWait(stShown, mi.job),
                   );
        }
        else if (mi.trans === trFadein) {
            ms.push(mkFadeinAnim(mi.dur),
                    mkWait(stHidden, mi.job),
                   );
        }
        else if (mi.trans === trCrossfade) {
            ms.push(mkFadeinAnim(mi.dur),
                    mkWait(stShown, mi.job),
                   );
        }
        break;

    case opFadeout:
        ms.push(mkStatus(stHidden));
        if (mi.trans === trCut || ! isPositive(mi.dur)) {
            ms.push(mkFadeoutCut());
        }
        else if (mi.trans === trFadeout || mi.trans === trCrossfade) {
            ms.push(mkFadeoutAnim(mi.dur));
        }
        ms.push(mkStatus(stShown));
        break;

    case opFinish:
        const nb = getVMBlocks(jno).length;
        // cleanup
        ms.push(mkTerminate());

        // wait for children to be finished
        for (i = nb; i > 0; i--) {
           ms.push(mkWait(stFinished, mkJno(jno, i)));
        }
        // set status, so no job will wait for any status to be reached
        ms.push(mkStatus(stFinished));
        break;

    case opFrame:
        ms.push(mkSet('frameGS', mi.gs));
        break;

    case opInit:
        ms.push(
            mkStatus(stCreated),
            mkSet('name', mi.name),
        );
        // local jobs wait until parent job is created
        if (! isTopLevelJno(jno)) {
            ms.push(mkWait(stCreated, 'par'));
        }
        break;

    case opPath:
        ms.push(mkSet('path', mi.path));
        break;

    case opText:
        ms.push(
            mkSet('lastTextInstr', mi),
            mkStatus(stReadypage),
            mkStatus(stReadymedia),
        );
        break;

    case opType:
        ms.push(mkSet('type', mi.type));
        break;

    default:
        trc(1,`execMicroInstr: op=${op} not yet implemented !!!`);
        break;
    }
    addReady(jno);
}

function execInstr(instr, activeJob) {
    const jno = activeJob.jno;
    const op  = instr.op;

    if ( [ opInit,
           'set',
           'errmsg',
           opStatus,
           'terminate',
           'fadeincut',
           'fadeinanim',
           'fadeoutcut',
           'fadeoutanim',

           'abort',
           opFadein,
           opFadeout,
           opFinish,
           opFrame,
           opPath,
           opText,
           opType,
         ].includes(op)
       ) {
        execMicroInstr(instr, activeJob);
        return;
    }
    /*
    if ( op === opInit ) {
        getData(jno).name = instr.name;
        setStatus(activeJob, stCreated);

        addReady(jno);
        return;
    }
    */
    if ( op === opLoadpage ) {
        trc(1,`execInstr: op=${op}`);

        loadPage(activeJob, jobsData.get(jno));
        return;
    }

    if ( op === opLoadmedia ) {
        trc(1,`execInstr: op=${op}`);

        loadMedia(activeJob, jobsData.get(jno));
        return;
    }

    // render frame and media element
    if ( op === opRender ) {
        getData(jno).lastGSInstr = instr;

        render(activeJob, jobsData.get(jno), instr.gs);
        setStatus(activeJob, stRendered);
        addReady(jno);
        return;
    }
    /*
    // fadein
    if ( op === opFadein ) {
        if ( instr.trans === trCut) {    // sync exec
            fadeinCut(activeJob);
            addReady(jno);
        }
        else {
            fadeinAnim(activeJob, instr.dur);  // async exec
        }
        return;
    }

    // fadeout
    if ( op === opFadeout ) {
        const dur = instr.dur;
        let trans = instr.trans;

        if ( ! isPositive(dur) ) {
            // no fadeout animation
            trans = trCut;
        }
        else {
            if ( trans === trCrossfade ) {
                // animation for corssfade and fadeout are the same
                trans = trFadeout;
            }
        }

        setStatus(activeJob, stShown);
        // slide has been shown, next jobs waiting
        // for this job to be shown are restarted

        if ( trans === trCut) {                 // sync job
            fadeoutCut(activeJob);
            addReady(jno);
        }
        else {
            fadeoutAnim(activeJob, instr.dur);  // async job
        }
        return;
    }
    */

    // animated move and/or scaling of media, async exec
    if ( op === opMove ) {
        getData(jno).lastGSInstr = instr;

        move(activeJob, jobsData.get(jno), instr.dur, instr.gs);
        return;
    }

    // move and/or scale without animation
    if ( op === opPlace ) {
        getData(jno).lastGSInstr = instr;

        place(activeJob, jobsData.get(jno), instr.gs);
        addReady(jno);
        return;
    }

    /*
    if ( op === opType ) {
        getData(jno).type  = instr.type;
        addReady(jno);
        return;
    }

    if ( op === opFrame ) {
        getData(jno).frameGS = instr.gs;

        addReady(jno);
        return;
    }

    if ( op === opText ) {
        // remember last text instr for text rendering and editing the text
        getData(jno).lastTextInstr = instr;

        setStatus(activeJob, stReadypage);
        setStatus(activeJob, stReadymedia);
        addReady(jno);
        return;
    }

    if ( op === opPath ) {
        getData(jno).path = instr.path;
        addReady(jno);
        return;
    }

    if ( op === opStatus ) {
        setStatus(activeJob, instr.st);
        addReady(jno);
        return;
    }
    */

    // sleep a given time
    if ( op === opDelay ) {
        if ( instr.dur ) {
            execDelay(instr, activeJob); // async
            return;
        }
        addReady(jno);
        return;
    }

    // put job into queue of jobs waiting for click
    if ( op === opWaitclick ) {

        // store job no, instr cnt and status for inspection
        vmActiveJob   = activeJob;
        vmInterrupted = true;
        vmRunning     = false;
        addInputJob(jno);
        return;
    }

    // syncing with previous slides
    if ( op === opWait ) {
        const jno1 = jnoFromJobName(jno, instr.job);
        const st1  = instr.status;

        trc(1, `wait: ${jno} requires (${jno1}, ${st1})`);

        const aj1  = jobsAll.get(jno1);
        if ( aj1                            // job j1 exists
             &&
             ! hasStatus(aj1, st1)          // job j1 already has reached st1
           ) {
                // job jno1 is too slow
                // put job into wait queue
                // and setup syncronizing

                trc(1, `wait: add to waiting jobs: jno=${jno1}, status=${st1}`);
                addWaiting(jno, mkWaitJob(jno1, st1));
                return;
        }

        // job not blocked, instr becomes a noop
        trc(1, `wait: job ${jno1} already has status ${st1}, continue`);
        addReady(jno);
        return;
    }

    /*
    // cleanup
    if ( op === opFinish ) {
        setStatus(activeJob, stFinished);
        terminateJob(activeJob);
        return;
    }
    */

    // default instr end
    // for instructions executed syncronized

    trc(1,`execInstr: op=${op} not yet implemented !!!`);
    addReady(jno);
}

// --------------------

function execDelay(instr, aj) {
    const jno = aj.jno;

    // set termination action
    function endDelay () {
        trc(1, `finish delay instr: (${jno}, ${aj.jpc})`);
        termAsyncInstr(jno);
    };

    addAsyncRunning(jno);
    setTimeout(endDelay, instr.dur * 1000); // sec -> msec
}

// --------------------

// add status to status set and wakeup waiting for status

function setStatus(activeJob, st) {
    const jno = activeJob.jno;

    addStatus(activeJob, st);
    trc(1, `setStatus: job=${jno} add ${st}`);

    // wakeup jobs waiting for this job to reach status
    if ( st === stFinished ) {
        wakeupAllWaiting(jno);
    }
    else {
        wakeupWaiting(mkWaitJob(jno, st));
    }
}

// --------------------

function abortJob(activeJob) {
    trc(1, `abortJob: job abborted: ${activeJob.jno}`);
    addStatus(activeJob, stAborted);
    terminateJob(activeJob);
}

function terminateJob(activeJob) {
    const jno = activeJob.jno;

    trc(1, `terminateJob: job terminated, delete job: ${jno}`);

    // // just for debugging
    // removeFrame(jno);
    // remJob(jno);

    addStatus(activeJob, stFinished);
    wakeupAllWaiting(jno);
    addReady(jno);

}

// --------------------

function termAsyncInstr(jno) {
    const aj = jobsAll.get(jno);
    trc(1, `termAsyncInstr: instr terminated: (${jno}, ${aj.jpc})`);

    // move job from async set to ready queue
    remAsyncRunning(jno);
    addReady(jno);
}

// --------------------

function loadPage(activeJob, jobData) {
    const jno = activeJob.jno;
    const req = { rType:    'json',
                  geo:      'org',
                  rPathPos: sPathToPathPos(jobData.path),
                };
    const url = reqToUrl(req);
    trc(1,`loadPage: ${url}`);

    function processRes(page) {
        remAsyncRunning(jno);

        const ty = getPageType(page);
        switch ( ty ) {
        case 'img':
            const frameGeo      = stageGeo();
            const imgGeo        = readGeo(page.oirGeo[0]);
            const gss           = getGeoSpecs(jno);
            const mxGeo         = maxGeo(frameGeo, imgGeo, gss);

            jobData.imgMetaData = getPageMeta(page);
            jobData.imgGeo      = imgGeo;
            jobData.imgMaxGeo   = mxGeo;
            jobData.imgReqGeo   = bestFitToGeo(mxGeo, imgGeo);

            break;
        default:
            trc(1,`loadPage: unsupported page type ${ty}`);
            abortJob(activeJob);
            return;
        }

        setStatus(activeJob, stReadypage);
        addReady(jno);
    }
    function processErr(errno, url, msg) {
        remAsyncRunning(jno);
        const txt = showErrText(errno, url, msg);
        statusBar.show(txt);
        abortJob(activeJob);
    }

    addAsyncRunning(jno);
    getJsonPage(url, processRes, processErr, noop);
}

// --------------------

function loadMedia(activeJob, jobData) {
    const jno = activeJob.jno;
    const ty  = jobData.type;

    switch ( ty ) {
    case 'img':
        jobData.imgCache = new Image();           // image cache
        const req = { rType:    'img',
                      rPathPos: sPathToPathPos(jobData.path),
                    };
        const url = imgReqToUrl(req, jobData.imgReqGeo);
        jobData.imgUrl = url;

        trc(1, `loadMedia: ${url}`);

        addAsyncRunning(jno);
        jobData.imgCache.onload = function () {
            const img = jobData.imgCache;
            jobData.imgResGeo = V2(img.naturalWidth, img.naturalHeight);

            remAsyncRunning(jno);               // remove job from async jobs
            setStatus(activeJob, stReadymedia);
            addReady(jno);         // next step(s)
        };
        jobData.imgCache.src = url;     // triggers loading of image into cache
        return;

    case 'text':                        // no request required for type 'text'
        setStatus(activeJob, stReadymedia);
        addReady(jno);
        return;

    default:
        trc(1,`loadMedia: unsupported type ${ty}`);
        abortJob(activeJob);
        return;
    }
}

// --------------------

function newFrame(id, go, css) {
    const s1 = cssAbsGeo(go);
    const s2 = {...s1, ...css};
    return newElem('div', id, s2, 'frame');
}

function mkFrameId(jno) {
    return `frame${jnoToId(jno)}`;
}

function imgGeoToCSS(jobData, gs) {
    const frameGeo = jobData.frameGO.geo;
    const imgGeo   = jobData.imgGeo;
    const go       = placeMedia(frameGeo, imgGeo)(gs);
    jobData.gs     = gs;           // save current geo spec
    jobData.go     = go;           // save current abs geo/off
    return cssAbsGeo(go);
}

// --------------------

function alignText(activeJob, jobData, aln) {
    const fid  = mkFrameId(activeJob.jno);
    const iid  = mkImgId(fid);
    const iid2 = mkImgId(iid);
    trc(1,`alignText: ${activeJob.jno} aln=${aln}`);
    setTextAlignCSS(iid2, aln);
}

function place(activeJob, jobData, gs) {
    const fid  = mkFrameId(activeJob.jno);
    const iid  = mkImgId(fid);
    const iid2 = mkImgId(iid);
    const ms   = imgGeoToCSS(jobData, gs);

    trc(1,`place: ${activeJob.jno} gs=${PP.gs(gs)} type=${jobData.type}`);
    setCSS(iid, ms);
    if ( jobData.type === 'text' ) {
        setTextScaleCSS(iid2, gs.scale);
    }
}

let cssAnimCnt = 1;

function move(activeJob, jobData, dur, gs) {
    const jno   = activeJob.jno;
    const fid   = mkFrameId(jno);
    const cssId = mkCssId(jno, cssAnimCnt++);
    const imgId = mkImgId(fid);

    const ms0   = cssAbsGeo(jobData.go);    // transition start
    const ms1   = imgGeoToCSS(jobData, gs); // transition end

    const cssKeyFrames = `
@keyframes kf-${cssId} {
    0% {${moveCSS(ms0)}}
  100% {${moveCSS(ms1)}}
}
`;
    const cssMoveClass = `
img.${cssId}, div.${cssId} {
  animation-name:            kf-${cssId};
  animation-duration:        ${dur}s;
  animation-delay:           0s;
  animation-direction:       normal;
  animation-iteration-count: 1;
  animation-timing-function: ease-in-out;
  animation-fill-mode:       both;
}
`;
    // install CSS animation code
    const c = newCssNode(cssId);
    c.appendChild(newText(cssKeyFrames + cssMoveClass));

    const i = getElem(imgId);

    function animEnd(ev) {
        trc(1, `animEnd: ${ev.animationName} animation has finished`);

        ev.stopPropagation();
        i.classList.remove(cssId);
        i.removeEventListener("animationend", animEnd);

        setCSS(imgId, ms1);
        // // for debugging
        // c.remove();
        termAsyncInstr(jno);
    }

    trc2(`move: ${jno} dur=${dur}`, gs);

    i.addEventListener('animationend', animEnd);
    i.classList.add(cssId);
    addAsyncRunning(jno);
}

function moveCSS(ms) {
    return `width: ${ms.width}; height: ${ms.height}; left: ${ms.left}; top: ${ms.top}`;
}

function mkCssId(jno, cssCnt) {
    return `css-job${jnoToId(jno)}-${cssCnt}`;
}

function newCssNode(cssId) {
    let e;
    if ( hasElem(cssId) ) {
        e = getElem(cssId);
        clearCont(e);
    }
    else {
        e = newElem('style', cssId);
        e.type = 'text/css';
    }

    getElem('head').appendChild(e);
    return e;
}

// --------------------
// build a new frame containing a media element

function parFrameGeoId(jno) {
    const jnoParent = jnoPar(jno);
    if ( jnoParent === jno0 ) {
        return { sg:  stageGeo(),
                 sid: stageId,
               };
    }
    else {
        return { sg:  jobsData.get(jnoParent).frameGO.geo,
                 sid: mkFrameId(jnoParent),
               } ;
    }
}

function render(activeJob, jobData, gs) {
    const jno = activeJob.jno;
    const ty  = jobData.type;
    const pfg = parFrameGeoId(jno);
    const sg  = pfg.sg;
    const sid = pfg.sid;
    const fid = mkFrameId(jno);

    switch ( ty ) {
    case 'text':
        renderText(jobData, fid, sg, sid, gs);
        break;

    case 'img':
        renderImg( jobData, fid, sg, sid, gs);
        break;

    default:
        abortJob(activeJob);
        // throw `render: unsupported media type: ${ty}`;
    }
}

function renderFrame(jobData, frameId, stageGeo, frameCss) {
    const frameGO   = placeFrame(stageGeo, jobData.frameGS);
    jobData.frameGO = frameGO;  // save frame geo/off

    return newFrame(frameId, frameGO, frameCss);
}

function renderImg(jobData, frameId, stageGeo, parentId, gs) {
    const frameCss = { opacity: 0,
                       visibility: 'hidden',
                       display:    'block',
                       overflow:   'hidden',
                     };
    const frame    = renderFrame(jobData, frameId, stageGeo, frameCss);
    const ms       = imgGeoToCSS(jobData, gs);
    const me       = newImgElem(frameId, ms, 'img');
    me.src         = jobData.imgUrl;

    frame.appendChild(me);
    getElem(parentId).appendChild(frame);
}

function renderText(jobData, frameId, stageGeo, parentId, gs) {
    const frameCss  = { opacity: 0,
                        visibility: 'hidden',
                        display:    'block',
                        overflow:   'hidden',
                      };
    const frame     = renderFrame(jobData, frameId, stageGeo, frameCss);
    const frameGeo  = jobData.frameGO.geo;

    // compute geomety of 'div' element containing the text
    // media geo is rel to frame geo
    // make media geo absolute

    const go   = placeFrame(frameGeo, gs);
    jobData.go = go;        // save text element geo/off in job data

    const ms1 = cssAbsGeo(go);
    const ms2 = { height:             'auto',
                  width:              null,
                  'background-color': 'red',
                  // 'background-color': 'transparent',
                };
    const ms  = {...ms1, ...ms2};

    // build a 'div' element containing the text
    // and insert it and the frame into DOM

    const me     = newBlogElem(frameId, ms, 'text');
    const textId = mkImgId(frameId);

    const me3    = newBlogElem(textId, {width: '100%'}, 'text-body' );
    setTextAlignCSS(me3, jobData.lastTextInstr.align);
    setTextScaleCSS(me3, gs.scale);
    me3.innerHTML = jobData.lastTextInstr.text;
    me.appendChild(me3);

    frame.appendChild(me);
    getElem(parentId).appendChild(frame);

    // compute real size of text
    // recompute geo/off for text
    // and overwrite preliminary geo/off

    const rect    = me.getBoundingClientRect();

    // round up geo, else unwanted linebreaks are added during rendering
    const textGeo = ceilV2(V2(rect.width, rect.height));
    jobData.imgGeo = textGeo;
    const go4     = placeMedia(frameGeo, textGeo)(gs);
    const ms4     = cssAbsGeo(go4);
    setCSS(me, ms4);

}

// --------------------
// transitions

function fadeCut(jno, visibility, opacity) {
    setCSS(mkFrameId(jno),
           { opacity:    opacity,
             visibility: visibility,
           }
          );
}

function fadeinCut (activeJob) {
    fadeCut(activeJob.jno, 'visible', 1.0);
    setStatus(activeJob, stVisible);
}
function fadeoutCut(activeJob) {
    fadeCut(activeJob.jno, 'hidden',  0.0);
    setStatus(activeJob, stHidden);
}

function fadeAnim(frameId, activeJob, dur, fade) { // fadein/fadeout
    const jno     = activeJob.jno;
    const e       = getElem(frameId);
    const cls     = `${fade}-image`;
    const opacity = (fade === 'fadein' ) ? 0.0 : 1.0;

    function handleFadeEnd(ev) {
        trc(1,`handleFadeEnd: ${frameId}, ${dur}, ${fade}`);
        ev.stopPropagation();
        e.classList.remove(cls);
        e.removeEventListener('animationend', handleFadeEnd);
        if ( fade === 'fadeout' ) {
            fadeoutCut(activeJob);
        } else {
            fadeinCut(activeJob);
        }
        termAsyncInstr(jno);
    }

    fadeCut(jno, 'visible', opacity);
    e.addEventListener('animationend', handleFadeEnd);
    e.classList.add(cls);

    addAsyncRunning(jno);   // add job to async jobs
    // start animation
    setAnimDur(e, dur);
}

function fadeinAnim (activeJob, dur) {
    fadeAnim(mkFrameId(activeJob.jno), activeJob, dur, 'fadein');
}

function fadeoutAnim(activeJob, dur) {
    fadeAnim(mkFrameId(activeJob.jno), activeJob, dur, 'fadeout');
}

// ----------------------------------------
// cleanup

function removeFrame(jno) {
    const frameId  = mkFrameId(jno);
    const e = getElem(frameId);

    e && e.remove();
}

// ----------------------------------------
//
// code selection and modification

// list of geo specs in a job

function instrGS(code) {
    let gss = [];
    for (i of code) {
        if ( i.gs !== undefined ) {
            gss.push(i.gs);
        }
    }
    return gss;
}

function activeJobData() {
    const aj  = vmActiveJob;
    const jno = aj.jno;
    return jobsData.get(jno);
}

// this is a destructive op
// the new scale value is inserted into the existing GS object

function algGS(alg) {
    function go(gs) {
        gs.alg = alg;
        return gs;
    }
    return go;
}
function dirGS(dir) {
    function go(gs) {
        gs.dir = dir;
        return gs;
    }
    return go;
}
function scaleGS(sc) {
    function go(gs) {
        gs.scale = mulV2(gs.scale, sc);
        return gs;
    }
    return go;
}
function shiftGS(sh) {
    function go(gs) {
        gs.shift = addV2(gs.shift, sh);
        return gs;
    }
    return go;
}

const shiftGS0 = (gs) => { gs.shift = V2(0); return gs; };
const scaleGS1 = (gs) => { gs.scale = V2(1); return gs; };
const resetGS  = (gs) => { gs.alg   = 'fitInto';
                           gs.scale = V2(1);
                           gs.dir   = 'center';
                           gs.shift = V2(0);
                           return gs;
                         };

function replaceImg(gs) {
    const aj  = vmActiveJob;
    const jd  = activeJobData();
    place(aj, jd, gs);
}

function editGS(f) {
    const gs  = getLastGS();
    const gs1 = f(gs);
    replaceImg(gs);
}

// --------------------

function alnText(aln) {
    function go(i) {
        i.align = aln;
        return i;
    }
    return go;
}

function realignText(aln) {
    const aj = vmActiveJob;
    const jd = activeJobData();
    alignText(aj, jd, aln);
}

function editTextAlign(f) {
    const i  = getLastTextInstr();
    const i1 = f(i);
    realignText(i1.align);
}

function editTextInstr() {
    const i  = getLastTextInstr();  // last text instr

    // callback when edit is finished
    //
    // store new text into last text instr
    // and rerender the text frame and contents

    function restoreCont(text) {
        const aj  = vmActiveJob;
        const jno = aj.jno;
        const jd  = activeJobData();
        const gs  = jd.lastGSInstr.gs;

        i.text = text;

        removeFrame(jno);     // remove old frame
        render(aj, jd, gs);   // and rebuild it with new text content
        fadeCut(jno, 'visible', 1.0);

    }
    // get text content of last text instr
    // display it in the text edit panel
    // and set callback after edit
    editTextPanel.edit(i.text, restoreCont);
}

function editCode() {return editCode1(vmActiveJob.jno);}
function editProg() {return editProg1(vmActiveJob.jno);}

function editCode1(jno) {
    const ppc = PP.code(getVMCode(jno));

    // callback
    function restoreCode(ppc1) {
        trc(1, `restoreCode: parser result:\n${ppc1}`);
        const res = PVM.parseCode(ppc1);
        if ( isLeft(res) ) {
            editTextPanel.edit(res.left, restoreCode);
        }
        else {
            const newCode = res.right;
            trc(1, PP.code(newCode));
            trc(1, `restart job ${jno} with new code`);
            restartJob(jno, newCode);
        }
    }

    // open edit code panel
    editTextPanel.edit(ppc, restoreCode);
}

function editProg1(jno) {
    const ppc = PP.prog(getVMProg(jno));

    // callback
    function restoreProg(ppc1) {
        trc(1, `restoreProg: parser result:\n${ppc1}`);
        const res = PVM.parseProg(ppc1);
        if ( isLeft(res) ) {
            editTextPanel.edit(res.left, restoreProg);
        }
        else {
            const newProg = res.right;
            trc(1, PP.prog(newProg));
            trc(1, `restart job ${jno} and subjobs with new code`);
            restartProg(jno, newProg);
        }
    }

    // open edit prog panel
    editTextPanel.edit(ppc, restoreProg);
}

// ----------------------------------------
//
// geo spec constructor

function GS(alg, scale, dir, shift) {
    return {
        alg:   alg,
        scale: scale,
        dir:   dir,
        shift: shift,
    };
}

// sharing the default object with others is
// prevented by turning the default geo spec into a function
//
// geo spec modifications come up during code editing
// when images are scaled and shifted

const defaultFrameGS = () => {
    return GS('fitInto', V2(1.0,1.0), 'center', V2(0,0));
};

const defaultGS = defaultFrameGS;

const leftHalfGeo = () => {
    return GS('fitInto', V2(0.5,1.0), 'W', V2(0,0));
};

const rightHalfGeo = () => {
    return GS('fitInto', V2(0.5,1.0), 'E', V2(0,0));
};

// ----------------------------------------
