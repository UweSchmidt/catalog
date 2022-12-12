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
const stMoved      = "moved";       // img has been redrawn (move, place)
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
    stMoved,
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
//
// code and prog constructors

// build a whole progam with jobs and nested subprograms

function VMProg(localBlocks, code) {
    return { code:   code,         // list of simple instructions
             blocks: localBlocks,  // list of local blocks
           };
};


function VMProg0(code) {
    return VMProg([], code);
};

function VMMain(...jobs) {
    return VMProg(
        [...jobs],
        [ mkInit('main'),
          mkType('container'),
          mkFrame(defaultFrameGS),
          mkRender(defaultGS),
          mkFinish()
        ]
    );
}

// ----------------------------------------
//
// constructors for VM instructions

function mkInit(name)  {return {op: opInit, name: name};}
function mkFinish()    {return {op: opFinish };};
function mkType(type)  {return {op: opType, type: type};}
function mkFrame(gs)   {return {op: opFrame, gs: gs};}
function mkPath(path)  {return {op: opPath, path: path};}
function mkText(align, text) {return {op: opText, align: align, text: text};}
function mkLoadpage()  {return {op: opLoadpage };}
function mkLoadmedia() {return {op: opLoadmedia };}
function mkRender(gs)  {return {op: opRender, gs: gs};}

function mkFadein(dur, trans, job) {
    return {op: opFadein, dur: dur, trans: trans, job: job || 'prev'};
}
function mkFadeout(dur, trans) {return {op: opFadeout, dur: dur, trans: trans};}
function mkStatus(st)        {return {op: opStatus, status: st};};
function mkMove(dur, gs)     {return {op: opMove, dur: dur, gs: gs};};
function mkPlace(gs)         {return {op: opPlace, gs: gs};};
function mkDelay(dur)        {return {op:  opDelay, dur: dur};}
function mkWaitclick()       {return {op: opWaitclick};}

// job values: 'par', 'prev', 'children' or -1, -2, ... (rel. jnos)
function mkWait(status, job) {return {op: opWait, job: job, status: status};};
function mkSet(name, value)  {return {op: 'set', name:  name, value: value};}
function mkErrmsg(msg)       {return {op: 'errmsg', msg: msg};}
function mkAbort()           {return {op: 'abort'} ;}
function mkTerminate()       {return {op: 'terminate'};}
function mkExit()            {return {op: 'exit'};}
function mkFadeinCut()       {return {op: 'fadeincut'};}
function mkFadeoutCut()      {return {op: 'fadeincut'};}
function mkFadeinAnim(dur)   {return {op: 'fadeinanim',  dur: dur};}
function mkFadeoutAnim(dur)  {return {op: 'fadeoutanim', dur: dur};}
function mkRender1(gs)       {return {op: 'render1', gs: gs};}
function mkMove1(dur, gs)    {return {op: 'move1', dur: dur, gs: gs};}
function mkPlace1(gs)        {return {op: 'place1', gs: gs};}
function mkLoadpage1()       {return {op: 'loadpage1'};}
function mkLoadpage1CB()     {return {op: 'loadpageCallback'};}
function mkProcesspage()     {return {op: 'processpage'};}
function mkLoadimage()       {return {op: 'loadimage'};}
function mkWaitOnClick()       {return {op: 'waitonclick'};}
function mkWait1(status,jno) {return {op: 'wait1', jno: jno, status: status};};

// --------------------

function noop() {}

// ----------------------------------------
// virtual machine code interpretation

// create active job control object
//
// jno:     hierachical job number
// jpc:     instruction counter pointing into jcode array
// jstatus: set of status values representing the progress of the job
//          used for synchronizing jobs
// jmicros: stack of micro instructions to be executed
// jcode:   array of instructions to be executed
// jdata:   job local data store
// jterm:   handler for premature termination of async instructions,
//          delay and animation instructions can be terminated
//          by input commands and set this field,
//          http requests (json and image requests) can finish in background
//          without conflicts with stopped or advanced slideshows

function VMJob(jno) {
    return { jno     : jno,
             jpc     : 0,
             jstatus : new Set(),
             jmicros : [],
             jcode   : [],
             jdata   : {},
             jterm   : null,
    };
}
// --------------------
// status ops

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

const jno0 ='.0';   // not '', howto see an empty string?

function mkJno(jno, i) {
    if ( jno === jno0 ) {jno = '';}
    return jno + '.' + i;
}

function jnoToIntList(jno) {
    if ( jno === jno0 ) {jno = '';}
    let l = jno.split('.');
    l.shift();
    return map(toNum)(l);
}

function jnoFromIntList(l) {
    const m = map((i) => {return "." + i;})(l);
    return (m.length === 0)
        ? jno0
        : intercalate('', m);
}

function jnoPrev(jno) {
    if ( isRootJno(jno) ) {
        return Nothing;
    }
    let l = jnoToIntList(jno);
    let i = l.pop();
    if ( i > 1 ) {
        l.push(i - 1);
    }
    return jnoFromIntList(l);
}

function jnoPar(jno) {
    if ( isRootJno(jno) ) {
        return Nothing;
    }
    let l = jnoToIntList(jno);
    l.pop();
    return jnoFromIntList(l);
}

function jnoPrevN(jno, n) {
    for (i = 0; i < n; i++) {
        jno = jnoPrev(jno);
        if (isNothing(jno)) break;
    }
    return jno;
}

function jnoToId(jno) {
    return jno.replace(/[.]/g,'-');
}

function jnoToFrameId(jno) {
    return `frame${jnoToId(jno)}`;
}

function jnosFromJobName(jno, n) {
    const res = [];

    if ( n === 'prev' ) {
        const r = jnoPrev(jno);
        if (isJust(r)) res.push(r);

    }
    else if ( n === 'par' ) {
        const r = jnoPar(jno);
        if (isJust(r)) res.push(r);
    }
    else if ( n === 'children' ) {    // access into cur VM code needed
        const p = vm.getProg(jno);
        for (let j = 1; j <= p.blocks.length; j++) {
            res.push(mkJno(jno, j));
        }
    }
    else if ( isNegative(n) ) {
        const r = jnoPrevN(jno, 0 - n);
        if (isJust(r)) res.push(r);
    }
    if ( isString(n) ) {
        for (const aj of vm.jobsAll.values()) {
            if ( aj.jdata.name === n ) {  // acces into VM data needed
                res.push(aj.jno);
            }
        }
    }
    return res;
}

function isTopLevelJno(jno) {return jnoToIntList(jno).length <= 1;}
function isRootJno    (jno) {return jno === jno0;}

// ----------------------------------------
// the VM constructor
//
// all data of the VM is defined
// as fields in this object
//
// this enables to build various vm instances

function VM() {
    const vm = {
        prog          : null,

        // prog management
        reset:      reset,
        initCode:   initCode,
        loadProg :  loadProg,
        reloadCode: reloadCode,

        // code access
        getProg:   getProg,
        getCode:   getCode,
        getBlocks: getBlocks,

        // start, stop and restart
        start:     start,
        stop:      stop,
        restartJob:  restartJob,
        restartProg: restartProg,

        // core ops
        run:         run,
        run1: run1,
        step1: step1,
        execMicroInstr: execMicroInstr,
        sleep: sleep,
        setStatus: setStatus,
        abortJob: abortJob,
        terminateJob: terminateJob,
        finishAsyncInstr: finishAsyncInstr,

        // ready jobs
        addReady:  addReady,
        nextReady: nextReady,
        readyJobs: readyJobs,

        // async jobs
        addAsyncRunning:  addAsyncRunning,
        remAsyncRunning:  remAsyncRunning,
        termAsyncRunning: termAsyncRunning,
        runningJobs:      runningJobs,

        // job management
        addJob:     addJob,
        remJob:     remJob,
        noMoreJobs: noMoreJobs,

        // syncing jobs
        addWaiting: addWaiting,
        getWaiting: getWaiting,
        getAllWaiting: getAllWaiting,
        wakeupWaiting: wakeupWaiting,
        wakeupAllWaiting: wakeupAllWaiting,
        waitingJobs: waitingJobs,

        // input jobs
        // addInputJob:         addInputJob,
        // wakeupInputJobs:     wakeupInputJobs,
        // waitingForInputJobs: waitingForInputJobs,
        // resumeFromInput:     resumeFromInput,
    };

    // reset all fields except the program
    // this enables restarting of the VM

    function reset() {
        // vm.prog          = null;
        vm.interrupted   = true;
        vm.running       = false;
        vm.stepCnt       = 0;
        vm.curJob        = null;
        vm.jobsAll       = new Map();  // Map Jno VMJob
        vm.jobsWaiting   = new Map();  // Map Jno (Map Status (Set Jno))
        // vm.jobsInput     = [];         // Queue Jno
        vm.jobsReady     = [];         // Queue Jno
        vm.jobsRunning   = new Set();  // Set Jno
    }
    // initialize the data fields of the VM
    reset();

    // initialize program code
    function loadProg(prog) {vm.prog = prog;}


    // --------------------
    // load program

    function initCode(prog0) {
        loadProg(prog0);          // store job hierachy in global VM variable
        reloadCode(jno0);         // flatten nested jobs and init jobs
    }

    function reloadCode(jno0) {   // reload whole program e.g. after edit
        const ready = [];

        function readyJobs(prog, jno) {
            trc(1,`readyJobs: ${jno} ${prog.code.length} ${prog.blocks.length}`);
            addJob(jno, prog.code);
            ready.push(jno);

            const subprogs = prog.blocks;
            for (let i = 0; i < subprogs.length; i++) {
                readyJobs(subprogs[i],
                          mkJno(jno, i + 1),    // job # run from 1
                         );
            }
        }

        const prog0 = vm.getProg(jno0);
        readyJobs(prog0, jno0);
        addReady(...ready);      // init queue of ready jobs ans run VM
    }

    // --------------------
    // the core: run and its components

    function start() {
        vm.interrupted = false;
        run();
    }

    function stop() {
        vm.interrupted = true;
        vm.termAsyncRunning();
    }

    function run() {
        if ( vm.running || vm.interrupted ) {
            // trc(1, "run: VM already running");
            return;
        }
        else {
            trc(1, "run: (re)start VM");
            run1();
        }
    }

    function run1() {
        vm.running = true;
        do {
            trc(1, `${vm.stepCnt}. step started`);
            res = step1();
            if ( res === 'step') {
                trc(1, `${vm.stepCnt}. step finished`);
            }
            vm.stepCnt++;
        }
        while ( res === 'step' );

        vm.running = false;
        trc(1, `${vm.stepCnt - 1}. step: res = ${res}`);
    }

    function step1() {
        if ( readyJobs() ) {
            let aj = nextReady();
            let mi = nextMicroInstr(aj);

            trc(1, `(${aj.jno}, ${aj.jpc}): mi: ${JSON.stringify(mi)}`);
            if ( ! isNothing(mi) ) {
                execMicroInstr(mi, aj);
            }                        // else job is finished
            return 'step';
        }
        else if ( runningJobs() ) {
            return 'running';
        }
        /* TODO
        else if ( waitingForInputJobs() ){
            return 'waiting for input';
        }
        */
        else if ( waitingJobs() ){
            return 'blocked';
        }
        else {
            return 'terminated';
        }
    }

    function abortCurJob(aj) {
        // jpc points behind last instr
        // and stack of micros is empty
        // --> nextMicroInstr will return Nothing

        aj.jpc     = aj.jcode.length;
        aj.jmicros = [];
    }

    function nextMicroInstr(aj) {
        if ( isEmptyList(aj.jmicros) ) {
            // stack of micro instructions is empty
            // expand current instr into a sequence of
            // micro instructions and push them
            // onto .jmicros stack

            const instr = aj.jcode[aj.jpc++];
            if ( ! instr ) {
                // no more instruction
                trc(1,`(${aj.jno}, ${aj.jpc}): job finished`);
                return Nothing;
            }
            trc(1, `(${aj.jno}, ${aj.jpc}): ${instr.op}`);

            aj.jmicros.push(instr);
        }
        // .jmicros is not empty
        return aj.jmicros.pop();
    }

    function execMicroInstr(mi, aj) {
        const jno = aj.jno;
        const op  = mi.op;
        const ms  = aj.jmicros;

        trc(1, `(${jno}, ${aj.jpc}): ${JSON.stringify(mi)} ${JSON.stringify(ms)}}`);

        switch ( op ) {

            // --------------------
            // micro instructions

        case 'errmsg':
            trc(1, mi.msg);
            statusBar.show(mi.msg);
            break;

        case 'exit':
            abortCurJob(aj);
            break;

        case 'fadeoutcut':
            fadeinCut(aj);
            break;

        case 'place1':
            place1(aj, mi.gs);
            break;

        case 'processpage':
            processPage(aj);
            break;

        case 'render1':
            render1(aj, mi.gs);
            break;

        case 'set':
            aj.jdata[mi.name] = mi.value;
            break;

        case opStatus:
            setStatus(aj, mi.status);
            break;

            // --------------------
            // basic async operations (don't exec addReady)

        case opDelay:
            sleep(aj, mi.dur);
            return;

        case 'fadeincut':
            fadeinCut(aj);
            return;

        case 'fadeinanim':
            fadeinAnim(aj, mi.dur);
            return;

        case 'fadeoutanim':
            fadeoutAnim(aj, mi.dur);
            return;

        case 'loadpage1':
            loadPage1(aj);
            return;

        case 'loadimage':
            loadImage(aj);
            return;

        case 'move1':
            move1(aj, mi.dur, mi.gs);
            return;

        case 'wait1':
            const jno1 = mi.jno;
            const st1  = mi.status;

            // trc(1, `wait: ${jno} requires (${jno1}, ${st1})`);

            const aj1  = vm.jobsAll.get(jno1);
            if ( ! hasStatus(aj1, st1) ) {
                // job jno1 is too slow
                // put job into wait queue
                // and setup syncronizing

                trc(1, `wait: ${jno} waits for job ${jno1} reaching status=${st1}`);
                addWaiting(jno, mkWaitJob(jno1, st1));
                return;                                 // async
            }

            // job j1 already has reached st1
            // job not blocked, instr becomes a noop
            trc(1, `wait: job ${jno1} already has status ${st1}, continue`);
            break;                                     // not async

        case 'waitonclick':
            // semantics: wait an unlimited time
            // wakeup by termAsyncRunning
            sleep(aj, 'unlimited');
            return;

            /*  TODO cleanup
            vm.curJob      = aj;      // store active job
            vm.interrupted = true;    // interrupt VM
            vm.running     = false;
            addInputJob(jno);         // mark job as waiting for input
            return;
            */

            // ----------------------------------------
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
            const syncJob = mi.job;

            // if job to be synced with is set to 'nowait'
            // no syncronisation is done,
            // so fadein can start immediately

            function mkFadeWait(st) {
                if ( syncJob !== 'nowait' ) {
                    ms.push(mkWait(st, syncJob));
                }
            }
            ms.push(mkStatus(stVisible));

            if (mi.trans === trCut || ! isPositive(mi.dur)) {
                ms.push(mkFadeinCut());
                mkFadeWait(stShown);
            }
            else if (mi.trans === trFadein) {
                ms.push(mkFadeinAnim(mi.dur));
                mkFadeWait(stHidden);
            }
            else if (mi.trans === trCrossfade) {
                ms.push(mkFadeinAnim(mi.dur));
                mkFadeWait(stShown);
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
            // const nb = getVMBlocks(jno).length;
            // cleanup
            ms.push(mkTerminate());

            // wait for children to be finished
            ms.push(mkWait(stFinished, 'children'));

            // for (i = nb; i > 0; i--) {
            //    ms.push(mkWait(stFinished, mkJno(jno, i)));
            // }
            // set status, so no job will wait for any status to be reached
            ms.push(mkStatus(stFinished));
            break;

        case opFrame:
            ms.push(mkSet('frameGS', mi.gs));
            break;

        case opInit:
            ms.push(
                mkStatus(stCreated),
                mkSet('name', mi.name || jno),
            );
            // local jobs wait until parent job is created
            if (! isTopLevelJno(jno)) {
                ms.push(mkWait(stCreated, 'par'));
            }
            break;

        case opLoadmedia:
            const ty = aj.jdata.type;
            switch ( ty ) {
            case 'img':
                ms.push(
                    mkStatus(stReadymedia),
                    mkLoadimage(),
                );
                break;

            case 'text':
                ms.push(
                    mkStatus(stReadymedia),
                );
                break;

            default:
                ms.push(
                    mkAbort(),
                    mkErrmsg(`loadMedia: wrong type ${ty}`),
                );
                break;
            }
            break;

        case opLoadpage:
            ms.push(
                mkLoadpage1CB(),
                mkLoadpage1(),   // async (HTTP request)
            );                   // aj.jdata.page gets the result page
            break;               // aj.jdata.callbackError the errmsg

        case 'loadpageCallback':
            if ( aj.jdata.page ) {
                ms.push(
                    mkStatus(stReadypage),
                    mkProcesspage(),
                );
            }
            else {
                ms.push(
                    mkAbort(),
                    mkErrmsg(aj.jdata.callbackError),
                );
            }
            break;

        case opMove:
            ms.push(
                mkStatus(stMoved),
                mkMove1(mi.dur, mi.gs),
                mkSet('lastGSInstr', mi),
            );
            break;

        case opPath:
            ms.push(mkSet('path', mi.path));
            break;

        case opPlace:
            ms.push(
                mkStatus(stMoved),
                mkPlace1(mi.gs),
                mkSet('lastGSInstr', mi),
            );
            break;

        case opRender:
            ms.push(
                mkStatus(stRendered),
                mkRender1(mi.gs)
            );
            // all jobs, except '.0', must wait for parent job
            // to create a frame (DOM div element) as the parent
            // element for this frame
            if ( ! isRootJno(jno) ) {
                ms.push(mkWait(stRendered, 'par'));
            }
            ms.push(mkSet('lastGSInstr', mi));
            break;

        case opText:
            ms.push(
                mkStatus(stReadymedia),
                mkStatus(stReadypage),
                mkSet('lastTextInstr', mi),
            );
            break;

        case opType:
            ms.push(mkSet('type', mi.type));
            break;

        case opWait:
            // symbolic jnos --> real jnos
            const jnos = jnosFromJobName(jno, mi.job);
            for (const jno1 of jnos) {
                ms.push(mkWait1(mi.status, jno1));
            }
            break;

        case opWaitclick:
            ms.push(
                mkWaitOnClick(),
                mkErrmsg('wait for user input'),
            );
            break;

        default:
            trc(1,`execMicroInstr: op=${op} not yet implemented !!!`);
            break;
        }
        addReady(jno);
    }

    // --------------------

    function sleep(aj, dur) {
        const jno = aj.jno;
        let timeout = null;

        function wakeup () {
            trc(1, `sleep: wakeup (${jno}, ${aj.jpc})`);

            timeout && clearTimeout(timeout);
            finishAsyncInstr(aj);
        };

        // wait a given time
        // or wait unlimited
        if ( isPositive(dur) ) {
            timeout = setTimeout(wakeup, dur * 1000);  // sec -> msec
        }
        aj.jterm = wakeup;

        addAsyncRunning(jno);
    }

    // --------------------

    // add status to status set and wakeup waiting for status

    function setStatus(aj, st) {
        const jno = aj.jno;

        addStatus(aj, st);
        trc(1, `setStatus: job=${jno} add ${st}`);

        // wakeup jobs waiting for this job to reach status
        if ( st === stFinished ) {
            wakeupAllWaiting(jno);
        }
        else {
            wakeupWaiting(mkWaitJob(jno, st));
        }
    }

    function abortJob(aj) {
        trc(1, `abortJob: job abborted: ${aj.jno}`);
        addStatus(aj, stAborted);
        terminateJob(aj);
    }

    function terminateJob(aj) {
        const jno = aj.jno;

        trc(1, `terminateJob: job terminated, delete job: ${jno}`);

        // // just for debugging
        // removeFrame(jno);
        // remJob(jno);

        addStatus(aj, stFinished);
        wakeupAllWaiting(jno);
        addReady(jno);
    }

    // --------------------

    function finishAsyncInstr(aj) {
        const jno = aj.jno;

        trc(1, `finishAsyncInstr: instr terminated: (${jno}, ${aj.jpc})`);

        // remove handler for premature termination
        // of async instruction (timeout or animation)
        // These maybe set before starting async instr
        aj.jterm = null;

        // move job from async set to ready queue
        remAsyncRunning(jno);
        addReady(jno);
    }

    // --------------------
    // jobs:

    function addJob(jno, jcode) {
        const aj = VMJob(jno);        // create new job
        aj.jcode = jcode;             // and set jcode array
        vm.jobsAll.set(jno, aj);      // insert job into job table
    }

    function remJob(jno) {
        vm.jobsAll.delete(jno);
    };

    function noMoreJobs() {
        return ! readyJobs()
            && ! runningJobs();
            // TODO && ! waitingForInputJobs();
    }

    // --------------------
    // set of jobs running currently asynchonized in the JS vm

    function addAsyncRunning(jno) {
        trc(1, `addAsyncRunning: add to async running jobs: ${jno}`);
        vm.jobsRunning.add(jno);
    }

    function remAsyncRunning(jno) {
        vm.jobsRunning.delete(jno);
    }

    // terminate all those async running jobs,
    // which have set a termination handler in .jterm field

    function termAsyncRunning() {
        let jbs = [];
        vm.jobsRunning.forEach((jno) => {
            const aj1 = vm.jobsAll.get(jno);
            if ( aj1.jterm ) {
                jbs.push(aj1);
            }
        });
        if ( jbs.length > 0 ) {
            // interrupt VM and
            // run all .jterm finalizer actions
            // then restart VM
            const intr = vm.interrupted;
            vm.interrupted = true;     // call of run() in addReady becomes noop

            for (const aj of jbs) {
                trc(1, `termAsyncRunning: job ${aj.jno} synchonized`);
                aj.jterm();
            }

            vm.interrupted = intr;
            run();
        }
    }

    function runningJobs() {
        return vm.jobsRunning.size > 0;
    }

    // --------------------
    // queue of ready jobs

    function addReady(...jnos) {
        // in restart after code edit
        // jno maybe already in queue

        for (const jno of jnos) {
            if ( ! vm.jobsReady.includes(jno) ) {
                vm.jobsReady.push(jno);           // add at end of job queue
                trc(1, `vm.addReady: ${jno}`);
            }
        }
        run();                           // new ready jobs, (re)start VM
    }

    function nextReady() {
        const jno = vm.jobsReady.shift();   // get and rem 1. elem of job queue
        return vm.jobsAll.get(jno);
    }

    function readyJobs() {
        return vm.jobsReady.length > 0;
    }

    // --------------------
    // jobsWaiting

    function addWaiting(jno, waitFor) {
        const jno1    = waitFor.jno;
        const status1 = waitFor.jstatus;

        const stmap   = vm.jobsWaiting.get(jno1) || new Map();
        const jnoset  = stmap.get(status1)  || new Set();
        const stmap1  = stmap.set(status1, jnoset.add(jno));
        vm.jobsWaiting.set(jno1, stmap1);
        trc(1, `addWaiting: job ${jno} waits for (${jno1},${status1})`);
    }

    function getWaiting(waitFor) {
        const jno1    = waitFor.jno;
        const status1 = waitFor.jstatus;
        const res     = [];

        const stmap   = vm.jobsWaiting.get(jno1);  // lookup waiting jobs for jno1
        if ( stmap ) {
            const jnos = stmap.get(status1);    // lookup waiting jobs for status1
            if ( jnos ) {                       // jobs found
                // trc(1,`wakeupWaiting: (${jno1},${status1})`);
                stmap.delete(status1);          // cleanup map of waiting jobs
                if ( stmap.size === 0) {
                    vm.jobsWaiting.delete(jno1);
                }
                jnos.forEach((jno) => {            // collect all jobs
                    // trc(1,`wakup: job ${jno}`); // waiting for jno1 reaching
                    res.push(jno);                 // status1
                });
            }
        }
        return res;
    }

    function wakeupWaiting(waitFor) {
        // all jobs are added at once before execution continues
        const res = getWaiting(waitFor);
        if ( res.length > 0 ) {
            trc2(`wakeupWaiting: (${waitFor.jno},${waitFor.jstatus}):`, res);
            addReady(...res);
        }
    }

    function getAllWaiting(jno1) {
        const res   = [];
        const stmap = vm.jobsWaiting.get(jno1);
        if ( stmap ) {
            let sts = [];
            for (const s of stmap.keys()) {
                sts.push(s);
            }
            for (const s1 of sts) {
                res.push(...getWaiting(mkWaitJob(jno1, s1)));
            }
        }
        return res;
    }

    function wakeupAllWaiting(jno) {
        // all jobs are added at once before execution continues
        const res = getAllWaiting(jno);
        if ( res.length > 0 ) {
            trc2(`wakeupAllWaiting: (${jno},*):`, res);
            addReady(...res);
        }
    }

    function waitingJobs() {
        return vm.jobsWaiting.size > 0;
    }

    // --------------------
    /*
    function addInputJob(jno) {
        trc(1,`addInputJob: job ${jno} inserted into waiting for click queue`);
        vm.jobsInput.push(jno);
    }

    function wakeupInputJobs() {
        const jobs = [...vm.jobsInput];
        vm.jobsInput.splice(0, jobs.length); // don't build a new array object

        trc(1, `wakeupInput: ${JSON.stringify(jobs)} ${JSON.stringify(vm.jobsInput)}`);
        addReady(...jobs); // only change the elements
    }

    function waitingForInputJobs() {
        return vm.jobsInput.length > 0;
    }

    // resume from interrupted state
    function resumeFromInput() {
        vm.interrupted = false;
        wakeupInputJobs();
    }
    */
    // --------------------
    // code access (for code edit)

    function getProg(jno) {
        let xs  = jnoToIntList(jno);
        let res = vm.prog;
        while ( xs.length > 0 ) {
            const ix = xs.shift();
            res = res.blocks[ix - 1];
        }
        return res;
    }

    function getCode  (jno) {return getProg(jno).code;}
    function getBlocks(jno) {return getProg(jno).blocks;}

    function getLastInstr(predicate) {

        function get1(aj) {
            const jno = aj.jno;
            const jpc = aj.jpc;
            const c   = getCode(jno);

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

    vm.getTextInstr   = getLastInstr((i) => {return i.op === opText;});
    vm.getTypeInstr   = getLastInstr((i) => {return i.op === opType;});
    vm.getLastGSInstr = getLastInstr((i) => {return isDefined(i.gs);});

    function getLastGS() {
        const i = vm.getLastGSInstr(vm.curJob);
        return i ? i.gs : defaultGS();
    }

    function getLastTextInstr() {return vm.getTextInstr(vm.curJob);}
    function getLastType()      {return vm.getTypeInstr(vm.curJob).type;}

    vm.getLastGS        = getLastGS;
    vm.getLastTextInstr = getLastTextInstr;
    vm.getLastType      = getLastType;

    function getGeoSpecs(jno) {
        const code = getCode(jno);
        let   gss  = [];
        for (const i of code) {
            if ( isDefined(i.gs) ) {
                gss.push(i.gs);
            }
        }
        trc2(`vm.getGeoSpecs: ${jno}`, gss);
        return gss;
    }
    vm.getGeoSpecs = getGeoSpecs;

    // --------------------
    //  restart job or block of jobs

    // replace the code part of a job with a new instr sequence
    // without destroying the object reference of the code array

    function replaceCode1(jno, code) {
        const c = getCode(jno);
        c.splice(0, c.length, ...code);
    }

    function restartJob(jno, jcode) {
        removeFrame(jno);
        replaceCode1(jno, jcode);
        reloadCode(jno);
        start();
    }

    function restartProg(jno, jprog) {

        // whole VM code is changed, new init
        if ( jno === jno0 ) {
            reset();
            initCode(jprog);
            start();
        }
        const p  = getProg(jno);      // get the program block
        p.code   = jprog.code;        // update code
        p.blocks = jprog.blocks;      // replace local jobs

        removeFrame(jno);             // remove DOM stuff for job and subjobs
        reloadCode(jno);              // reload and reinit job and subjobs
        start();                      // push the red button
    }

    // --------------------

    return vm;   // the completely initialized VM
}

// build the VM object

let vm = VM();   // this object contains the VM

// ----------------------------------------
//
// server requests

function loadPage1(aj) {
    const jdata = aj.jdata;
    const jno   = aj.jno;
    const req   = { rType:    'json',
                    geo:      'org',
                    rPathPos: sPathToPathPos(jdata.path),
                  };
    const url = reqToUrl(req);
    trc(1,`loadPage1: ${url}`);

    function processRes(page) {
        jdata.page = page;
        vm.finishAsyncInstr(aj);
    }

    function processErr(errno, url, msg) {
        const txt = showErrText(errno, url, msg);
        jdata.callbackError = txt;
        vm.finishAsyncInstr(aj);
    }

    vm.addAsyncRunning(jno);
    getJsonPage(url, processRes, processErr, noop);
}

function processPage(aj) {
    const jno   = aj.jno;
    const jdata = aj.jdata;
    const page  = jdata.page;
    const ty    = getPageType(page);

    switch ( ty ) {
    case 'img':
        const frameGeo      = parFrameGeoId(jno).sg;
        const imgGeo        = readGeo(page.oirGeo[0]);
        const gss           = vm.getGeoSpecs(jno);
        const mxGeo         = maxGeo(frameGeo, imgGeo, gss);

        jdata.imgMetaData = getPageMeta(page);
        jdata.imgGeo      = imgGeo;
        jdata.imgMaxGeo   = mxGeo;
        jdata.imgReqGeo   = bestFitToGeo(mxGeo, imgGeo);
        break;

    default:
        trc(1,`processPage: unsupported page type ${ty}`);
        // abortJob(aj);
        return;
    }
}

// --------------------

function loadImage(aj) {
    const jno   = aj.jno;
    const jdata = aj.jdata;
    const req   = { rType:    'img',
                    rPathPos: sPathToPathPos(jdata.path),
                  };
    const url   = imgReqToUrl(req, jdata.imgReqGeo);

    jdata.imgUrl   = url;
    jdata.imgCache = new Image();           // image cache

    trc(1, `loadImage: ${url}`);

    // install event handler on image cache
    jdata.imgCache.onload = function () {
        const img       = jdata.imgCache;
        jdata.imgResGeo = V2(img.naturalWidth, img.naturalHeight);
        vm.finishAsyncInstr(aj);
    };

    vm.addAsyncRunning(jno);
    jdata.imgCache.src = url;     // triggers loading of image into cache
    return;
}

// ----------------------------------------
//
// DOM manipulation

function newFrame(id, go, css) {
    const s1 = cssAbsGeo(go);
    const s2 = {...s1, ...css};
    return newElem('div', id, s2, 'frame');
}

function imgGeoToCSS(jdata, gs) {
    const frameGeo = jdata.frameGO.geo;
    const imgGeo   = jdata.imgGeo;
    const go       = placeMedia(frameGeo, imgGeo)(gs);
    jdata.gs       = gs;           // save current geo spec
    jdata.go       = go;           // save current abs geo/off
    return cssAbsGeo(go);
}

// --------------------

function alignText(aj, aln) {
    const fid  = jnoToFrameId(aj.jno);
    const iid  = mkImgId(fid);
    const iid2 = mkImgId(iid);
    trc(1,`alignText: ${aj.jno} aln=${aln}`);
    setTextAlignCSS(iid2, aln);
}

function place1(aj, gs) {
    const jdata = aj.jdata;
    const fid   = jnoToFrameId(aj.jno);
    const iid   = mkImgId(fid);
    const iid2  = mkImgId(iid);
    const ms    = imgGeoToCSS(jdata, gs);

    trc(1,`place: ${aj.jno} gs=${PP.gs(gs)} type=${jdata.type}`);
    setCSS(iid, ms);
    if ( jdata.type === 'text' ) {
        setTextScaleCSS(iid2, gs.scale);
    }
}

let cssAnimCnt = 1;

function move1(aj, dur, gs) {
    const jno   = aj.jno;
    const jdata = aj.jdata;
    const fid   = jnoToFrameId(jno);
    const cssId = mkCssId(jno, cssAnimCnt++);
    const cssNm = `kf-${cssId}`;
    const imgId = mkImgId(fid);

    const ms0   = cssAbsGeo(jdata.go);    // transition start
    const ms1   = imgGeoToCSS(jdata, gs); // transition end

    const cssKeyFrames = `
@keyframes kf-${cssId} {
    0% {${moveCSS(ms0)}}
  100% {${moveCSS(ms1)}}
}
`;
    const cssMoveClass = `
img.${cssId}, div.${cssId} {
  animation-name:            ${cssNm};
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
        trc(1, `animEnd: ${cssNm} animation has finished`);

        // ev only set when animationend is fired (not premature term)
        ev && ev.stopPropagation();
        i.classList.remove(cssId);
        i.removeEventListener("animationend", animEnd);

        setCSS(imgId, ms1);
        // // for debugging
        // c.remove();
        vm.finishAsyncInstr(aj);
    }

    trc2(`move: ${jno} dur=${dur}`, gs);

    // set handler for premature termination of async instr
    aj.jterm = animEnd;

    // set handler for regular termination of async instr
    i.addEventListener('animationend', animEnd);
    i.classList.add(cssId);
    vm.addAsyncRunning(jno);
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
    if ( isRootJno(jno) ) {
        return { sg:  stageGeo(),
                 sid: stageId,
               };
    }
    else {
        const jnoParent = jnoPar(jno);
        return { sg:  vm.jobsAll.get(jnoParent).jdata.frameGO.geo,
                 sid: jnoToFrameId(jnoParent),
               } ;
    }
}

function render1(aj, gs) {
    const jno   = aj.jno;
    const jdata = aj.jdata;
    const ty    = jdata.type;
    const pfg   = parFrameGeoId(jno);
    const sg    = pfg.sg;
    const sid   = pfg.sid;
    const fid   = jnoToFrameId(jno);

    switch ( ty ) {
    case 'text':
        renderText(jdata, fid, sg, sid, gs);
        break;

    case 'img':
        renderImg(jdata, fid, sg, sid, gs);
        break;

    case 'container':
        renderContainer(jdata, fid, sg, sid);
        break;

    default:
        // abortJob(aj);
        throw `render: unsupported media type: ${ty}`;
    }
}

const visibleCSS = {
    opacity:    1,
    visibility: 'visible',
    display:    'block',
    overflow:   'hidden',
};

const hiddenCSS = {
    opacity:    0,
    visibility: 'hidden',
    display:    'block',
    overflow:   'hidden',
};

function renderFrame(jdata, frameId, stageGeo, frameCss) {
    const frameGO   = placeFrame(stageGeo, jdata.frameGS);
    jdata.frameGO = frameGO;  // save frame geo/off

    return newFrame(frameId, frameGO, frameCss);
}

function renderContainer(jdata, frameId, stageGeo, parentId) {
    const frame = renderFrame(jdata, frameId, stageGeo, visibleCSS);
    getElem(parentId).appendChild(frame);
}

function renderImg(jdata, frameId, stageGeo, parentId, gs) {
    const frame    = renderFrame(jdata, frameId, stageGeo, hiddenCSS);
    const ms       = imgGeoToCSS(jdata, gs);
    const me       = newImgElem(frameId, ms, 'img');
    me.src         = jdata.imgUrl;

    frame.appendChild(me);
    getElem(parentId).appendChild(frame);
}

function renderText(jdata, frameId, stageGeo, parentId, gs) {
    const frame     = renderFrame(jdata, frameId, stageGeo, hiddenCSS);
    const frameGeo  = jdata.frameGO.geo;

    // compute geomety of 'div' element containing the text
    // media geo is rel to frame geo
    // make media geo absolute

    const go   = placeFrame(frameGeo, gs);
    jdata.go = go;        // save text element geo/off in job data

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
    setTextAlignCSS(me3, jdata.lastTextInstr.align);
    setTextScaleCSS(me3, gs.scale);
    me3.innerHTML = jdata.lastTextInstr.text;
    me.appendChild(me3);

    frame.appendChild(me);
    getElem(parentId).appendChild(frame);

    // compute real size of text
    // recompute geo/off for text
    // and overwrite preliminary geo/off

    const rect    = me.getBoundingClientRect();

    // round up geo, else unwanted linebreaks are added during rendering
    const textGeo = ceilV2(V2(rect.width, rect.height));
    jdata.imgGeo  = textGeo;
    const go4     = placeMedia(frameGeo, textGeo)(gs);
    const ms4     = cssAbsGeo(go4);
    setCSS(me, ms4);

}

// --------------------
// transitions

function fadeCut(jno, visibility, opacity) {
    setCSS(jnoToFrameId(jno),
           { opacity:    opacity,
             visibility: visibility,
           }
          );
}

function fadeinCut (aj) {
    fadeCut(aj.jno, 'visible', 1.0);
}
function fadeoutCut(aj) {
    fadeCut(aj.jno, 'hidden',  0.0);
}

function fadeAnim(frameId, aj, dur, fade) { // fadein/fadeout
    const jno     = aj.jno;
    const e       = getElem(frameId);
    const cls     = `${fade}-image`;
    const opacity = (fade === 'fadein' ) ? 0.0 : 1.0;

    function handleFadeEnd(ev) {
        trc(1,`handleFadeEnd: ${frameId}, ${dur}, ${fade}`);

        // ev only set when animationend is fired (not premature term)
        ev && ev.stopPropagation();
        e.classList.remove(cls);
        e.removeEventListener('animationend', handleFadeEnd);
        if ( fade === 'fadeout' ) {
            fadeoutCut(aj);
        } else {
            fadeinCut(aj);
        }
        vm.finishAsyncInstr(aj);
    }

    fadeCut(jno, 'visible', opacity);

    // set handler for premature termination of async instr
    aj.jterm = handleFadeEnd;

    e.addEventListener('animationend', handleFadeEnd);
    e.classList.add(cls);

    vm.addAsyncRunning(jno);   // add job to async jobs
    // start animation
    setAnimDur(e, dur);
}

function fadeinAnim (aj, dur) {
    fadeAnim(jnoToFrameId(aj.jno), aj, dur, 'fadein');
}

function fadeoutAnim(aj, dur) {
    fadeAnim(jnoToFrameId(aj.jno), aj, dur, 'fadeout');
}

// ----------------------------------------
// cleanup

function removeFrame(jno) {
    const e = getElem(jnoToFrameId(jno));
    e && e.remove();
}

// ----------------------------------------
//
// code modification ops

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

// these are destructive op
// the existing GS objects are modified, no new GS are constructed

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

function editGS(f) {
    const gs  = vm.getLastGS();
    const gs1 = f(gs);
    place1(curJob, gs);
}

// --------------------

function alnText(aln) {
    function go(i) {
        i.align = aln;
        return i;
    }
    return go;
}

function editTextAlign(f) {
    const i  = vm.getLastTextInstr();
    const i1 = f(i);
    alignText(curJob, i1.align);
}

function editTextInstr() {
    const textInstr  = vm.getLastTextInstr();  // last text instr

    // callback when edit is finished
    //
    // store new text into last text instr
    // and rerender the text frame and contents

    function restoreCont(text) {
        const aj  = vm.curJob;
        const jno = aj.jno;
        const gs  = aj.jdata.lastGSInstr.gs;

        textInstr.text = text;        // change the text instr

        removeFrame(jno);     // remove old frame
        render1(aj, gs);      // and rebuild it with new text content
        fadeCut(jno, 'visible', 1.0);

    }
    // get text content of last text instr
    // display it in the text edit panel
    // and set callback after edit
    editTextPanel.edit(textInstr.text, restoreCont);
}

function editCode() {return editCode1(curJob.jno);}
function editProg() {return editProg1(curJob.jno);}

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
