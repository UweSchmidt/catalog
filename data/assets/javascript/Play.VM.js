// stati

const stCreated    = "created";
const stReadypage  = "readypage";
const stReadymedia = "readymedia";
const stRendered   = "rendered";
const stVisible    = "visible";
const stShown      = "shown";
const stHidden     = "hidden";
const stFinished   = "finished";
const stNothing    = "nothing";

function emptySt() {
    return new Set();
};
function addSt(st, stats) {
    return stats.add(st);
};
function memberSt(st, stats) {
    return stats.has(st);
}
// opcodes

const opInit      = "init";
const opLoadpage  = "loadpage";
const opLoadmedia = "loadmedia";
const opRender    = "render";
const opFadeout   = "fadeout";
const opFadein    = "fadein";
const opMove      = "move";
const opShown     = "shown";
const opDelay     = "delay";
const opWait      = "wait";
const opExit      = "exit";

// build instructions

function mkInit() {
    return { op: opInit };
}

function mkLoadpage(url) {
    return { op:  opLoadpage,
             url: url
           };
}

function mkLoadmedia() {
    return { op: opLoadmedia };
};

function mkRender() {
    return { op: opRender };
};

function mkFadein(transition, duration) {
    return { op:    opFadein,
             trans: transition || 'cut',
             dur:   duration   || 0,
           };
};

function mkFadeout(transition, duration) {
    return { op:    opFadeout,
             trans: transition || 'cut',
             dur:   duration   || 0,
           };
};

function mkShowEnd() {
    return { op:  opShown };
};

function mkMove() {
    return { op: opMove };
};

function mkDelay(dur) {
    return { op:  opDelay,
             dur: dur
           };
}

function mkWait(reljno, st) {
    return { op:     opWait,
             reljno: reljno,
             status: st,
           };
};

function mkExit() {
    return { op: opExit };
};

// type Job = [Instr]
// type Jobno = Int
// type activeJob = { jno: n, jpc: n, }

function noop() {}

function mkActiveJob(jno, jpc, jstatus) {
    return { jno     : jno,
             jpc     : jpc,
             jstatus : jstatus,
             jterm   : noop,        // finalizer func
             jtimeout: null
    };
}

function mkWaitJob(jno, jstatus) {
    return { jno     : jno,
             jstatus : jstatus
           };
};

// machine state: global

var vmRunning;
var vmStepCnt;
var jobsCode;
var jobsAll;
var jobsWaiting;
var jobsReady;
var jobsRunning;

function initVM() {
    vmRunning   = false;
    vmStepCnt   = 0;
    jobsCode    = new Map();  // Map Jno [Instr]
    jobsAll     = new Map();  // Map Jno ActiveJob
    jobsWaiting = new Map();  // Map (Jno, Status) (Set Jno)
    jobsReady   = [];         // Queue Jno
    jobsRunning = new Set();  // Set Jno
}

initVM();

// --------------------
// jobs:

function addJob(jno, jcode) {
    jobsCode.set(jno, jcode);
    jobsAll.set(jno, mkActiveJob(jno, 0, new Set()));
}
function remJob(jno) {
    jobsCode.delete(jno);
    jobsAll.delete(jno);
};
function getCode(jno) {
    return jobsCode.get(jno);
}
function noMoreJobs() {
    return ! readyJobs() && ! runningJobs();
}

// --------------------
// jobsRunning:

function addRunning(jno) {
    jobsRunning.add(jno);
}
function remRunning(jno) {
    jobsRunning.delete(jno);
}
function runningJobs() {
    return jobsRunning.size > 0;
}

// --------------------
// jobsReady:

function addReady(jno) {
    if (! jobsAll.get(jno).jstatus.has(stFinished)) {
        jobsReady.push(jno);           // add at end of job queue
        trc(1, `addReady: ${jno}`);

        if ( jobsReady.length === 1) {
            run();                         // new ready job, (re)start VM
        }
    }
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
    const k1 = toKeyWaitJob(waitFor);
    const s1 = jobsWaiting.get(k1) || new Set();
    jobsWaiting.set(k1, s1.add(jno));
    trc(1, `addWaiting: job ${jno} waits for ${k1}`);
}
function wakeupWaiting(waitFor) {
    const k1 = toKeyWaitJob(waitFor);
    const s1 = jobsWaiting.get(k1);
    if ( s1 ) {
        trc(1,`wakeupWaiting: ${k1}`);
        jobsWaiting.delete(k1);
        s1.forEach((jno) => {
            trc(1,`wakup: job ${jno}`);
            addReady(jno);
        });
    }
}
function waitingJobs() {
    return jobsWaiting.size > 0;
}
function toKeyWaitJob(wj) {
    return `(${wj.jno},${wj.jstatus})`;
}

// --------------------

function run() {
    if ( vmRunning ) {
        trc(1, "run: VM already running");
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
        trc(1, `${vmStepCnt}. step started`);
        res = step1();
        trc(1, `${vmStepCnt}. step finished`);
        vmStepCnt++;
    }
    while ( res === 'step' );

    vmRunning = false;
    trc(1, `${vmStepCnt - 1}. step: res = ${res}`);
}

function step1() {
    if ( readyJobs() ) {
        let aj = nextReady();
        let ci = getCode(aj.jno)[aj.jpc];

        trc(1, `(${aj.jno}, ${aj.jpc}): ${ci.op}`);

        execInstr(ci, aj);
        return 'step';
    }
    else if ( runningJobs() ) {
        return 'running';
    }
    else if ( waitingJobs() ){
        return 'blocked';
    }
    else {
        return 'terminated';
    }
}

function execInstr(instr, activeJob) {
    const jno = activeJob.jno;
    const op  = instr.op;
    var st;

    if ( op === opInit ) {
        st = stCreated;
    }
    else if ( op === opLoadpage ) {
        st = stReadypage;
    }
    else if ( op === opLoadmedia ) {
        st = stReadymedia;
    }
    else if ( op === opRender ) {
        st = stRendered;
    }
    else if ( op === opFadein ) {
        st = stVisible;
    }
    else if ( op === opShown ) {
        st = stShown;
    }
    else if ( op === opMove ) {  // TODO async
        st = stVisible;
    }
    else if ( op === opDelay ) {
        if ( instr.dur ) {
            execDelay(instr, activeJob); // async
            return;
        }
        st = stNothing;  // delay 0 -> instr is a noop
    }
    else if ( op === opFadeout ) {

        st = stHidden;
    }
    else if ( op === opWait ) {
        const jno1 = jno - instr.reljno;
        const st1  = instr.status;

        trc(1, `wait: ${jno} requires (${jno1}, ${st1})`);

        const aj1  = jobsAll.get(jno1);
        if ( aj1 ) {
            if ( ! aj1.jstatus.has(st1) ) {
                // jno1 is too slow
                // put job into wait queue
                // and setup syncronizing

                trc(1, `wait: add to waiting jobs: jno=${jno1}, status=${st1}`);
                const aj = advanceJob(activeJob);        // finish wait instr
                addWaiting(jno, mkWaitJob(jno1, st1));
                return;
            }
        }

        // job not blocked, instr becomes a noop
        trc(1, `wait: job ${jno1} already has status ${st1}, continue`);
        st = stNothing;
    }
    else if ( op === opExit ) {
        trc(1, `exit: job terminated, delete job ${activeJob.jno}`);
        st = stFinished;
    }

    // default instr end
    // for instructions executed syncronized

    advanceJobStatus(activeJob, st);
}

// --------------------

/*
// show instr: async, delay job for a given time
// but make timeout interruptable
// timeout is stored in active job obj


function execShow(instr, aj) {
    const jno = aj.jno;

    // set termination action
    aj.jterm = function () {
        trc(1, `finish show instr: (${jno}, ${aj.jpc})`);
        termInstr(jno, stShown);
    };

    // set timeout
    aj.jtimeout = setTimeout(aj.jterm, instr.dur);

    // put job into set of jobs running asyncronized
    trc(1, `execShow: add to running: ${jno}`);
    addRunning(jno);
}
*/
function execDelay(instr, aj) {
    const jno = aj.jno;

    // set termination action
    aj.jterm = function () {
        trc(1, `finish delay instr: (${jno}, ${aj.jpc})`);
        termInstr(jno, stNothing);
    };

    // set timeout
    aj.jtimeout = setTimeout(aj.jterm, instr.dur);

    // put job into set of jobs running asyncronized
    trc(1, `execDelay: add to running: ${jno}`);
    addRunning(jno);
}

// --------------------

function termInstr(jno, st) {
    trc(1, `termInstr: jno=${jno}, st=${st}`);
    const aj = jobsAll.get(jno);
    trc(1, `termInstr: finalize instr (${jno}, ${aj.jpc})`);
    if ( aj ) {
        remRunning(jno);           // remove from set of async jobs

        if ( aj.jtimeout ) {               // clear timeouts
            clearTimeout(aj.jtimeout);     // when interrupted by input events
            aj.timeout = null;
        }
        advanceJobStatus(aj, st);
    }
}

// instr exec terminated,
// advance job PC, add status to status set
// and put back the job into queue of running jobs

function advanceJob(aj) {
    const jno = aj.jno;
    const aj1 = mkActiveJob(jno, aj.jpc + 1, aj.jstatus);
    jobsAll.set(jno, aj1);
}

function advanceJobStatus(aj, st) {
    const jno = aj.jno;
    const aj1 = mkActiveJob(jno, aj.jpc + 1, aj.jstatus.add(st));
    jobsAll.set(jno, aj1);

    trc(1, `advanceJobStatus: (${jno}, ${aj1.jpc}): status: ${st}`);

    wakeupWaiting(mkWaitJob(jno, st));
    addReady(jno);
}

// --------------------

// job creation

var newJobNo = 0;

const trCut       = 'cut';
const trFadein    = 'fadein';
const trFadeout   = 'fadeout';
const trCrossfade = 'crossfade';

function mkJob1(jno, url, dur) {
    return { jno:   jno,
             jcode: [ mkInit(),
                      mkLoadpage(url),
                      mkLoadmedia(),
                      mkRender(),
                      mkFadein(),
                      mkDelay(dur),
                      mkShowEnd(),
                      mkFadeout(),
                      mkExit()
                    ]
           };
};

function mkJob(jno, jd) {
    // init code
    let cinit = [ mkInit(),
                  mkLoadpage(jd.url),
                  mkLoadmedia(),
                  mkRender(),
                ];

    // --------------------
    // fadein code

    let cfadein = [];
    if ( jd.fadeinDur === 0 ) {
        jd.fadeinTrans = trCut;
    }

    switch ( jd.fadeinTrans ) {
    case 'fadein' :
        cfadein = [ mkWait(1, stHidden),   // wait for previous job
                    mkFadein(trFadein, jd.fadeinDur)
                  ];
        break;

    case 'crossfade' :
        cfadein = [ mkWait(1, stShown),   // crossfade starts earlier
                    mkFadein(trFadein, jd.fadeinDur)
                  ];
        break;

    case trCut :
    default:
        cfadein = [ mkWait(1, stShown),
                    mkFadein(trCut, 0)
                  ];
        break;
    }

    // --------------------
    // fadeout code

    let cfadeout = [];
    if ( jd.fadeoutDur === 0 ) {
        jd.fadeoutTrans = trCut;
    }

    switch ( jd.fadeoutTrans ) {
    case 'fadeout' :
    case 'crossfade' :
        cfadeout = [ mkFadeout(trFadeout, jd.fadeoutDur) ];
        break;

    case trCut :
    default:
        cfadeout = [ mkFadeout(trCut, 0) ];
        break;
    }

    // --------------------
    // show code

    let cshow = [ mkDelay(jd.showDur),
                  mkShowEnd()
                ];

    // --------------------
    // exit code

    let cexit = [ mkExit() ];

    // --------------------

    const code = [].concat(cinit, cfadein, cshow, cfadeout, cexit);

    return { jno:   jno,
             jcode: code
           };
}

// --------------------

var j1 = mkJob(1, { url: "/emil",
                    showDur: 3000,
                    fadeinDur: 1000,
                    fadeinTrans: trFadein,
                    fadeoutDur: 2000,
                    fadeoutTrans: trCrossfade,
                  }
              );
var j2 = mkJob(2, { url: "/egon",
                    showDur: 5000,
                    fadeinDur: 2000,
                    fadeinTrans: trCrossfade,
                    fadeoutDur: 1000,
                    fadeoutTrans: trCrossfade,
                  }
              );

function ttt() {
    initVM();
    addJob(j1.jno, j1.jcode);
    addJob(j2.jno, j2.jcode);
    addReady(j1.jno);
    addReady(j2.jno);
}
