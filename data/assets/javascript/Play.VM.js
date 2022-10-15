// ----------------------------------------
// the slideshow VM


// all timings are in seconds

// ----------------------------------------
// stati

const stCreated    = "created";
const stReadypage  = "readypage";
const stReadymedia = "readymedia";
const stVisible    = "visible";
const stShown      = "shown";
const stHidden     = "hidden";
const stFinished   = "finished";
const stNothing    = "nothing";

// set of stati

function emptySt() {
    return new Set();
};
function addSt(st, stats) {
    return stats.add(st);
};
function memberSt(st, stats) {
    return stats.has(st);
}

// ----------------------------------------
// opcodes

const opInit      = "init";
const opLoadpage  = "loadpage";
const opLoadmedia = "loadmedia";
const opRender    = "render";
const opFadeout   = "fadeout";
const opFadein    = "fadein";
const opMove      = "move";
const opSetstatus    = "setstatus";
const opDelay     = "delay";
const opWait      = "wait";
const opFinish    = "finish";

// ----------------------------------------
// transitions

const trCut       = 'cut';
const trFadein    = 'fadein';
const trFadeout   = 'fadeout';
const trCrossfade = 'crossfade';

// ----------------------------------------

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
}

function mkRender(media) {
    return { op:    opRender,
             media: media,
           };
}

function mkFadein(dur, trans) {
    if ( dur === 0) {
        trans = trCut;
    }
    return { op:    opFadein,
             trans: trans,
             dur:   dur,
           };
};

function mkFadeout(dur, trans) {
    if ( dur === 0) {
        trans = trCut;
    }
    return { op:    opFadeout,
             trans: trans,
             dur:   dur,
           };
};

function mkSetstatus(st) {
    return { op:  opSetstatus,
             st: st,
           };
};

function mkMove(dur, off, scale) {
    return { op:    opMove,
             off:   off,
             scale: scale
           };
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

function mkFinish() {
    return { op: opFinish };
};

// type Job = [Instr]
// type Jobno = Int
// type activeJob = { jno: n, jpc: n, }

function noop() {}

// ----------------------------------------
// virtual machine code interpretation

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
        trc(1,`execInstr: op=${op}`);
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opLoadpage ) {
        trc(1,`execInstr: op=${op}`);

        // load page

        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opLoadmedia ) {
        trc(1,`execInstr: op=${op}`);

        // load media

        advanceReadyJob(activeJob);
        return;
    }

    // render frame and media element
    if ( op === opRender ) {
        render(jno, instr.media);
        advanceReadyJob(activeJob);
        return;
    }

    // fadein
    if ( op === opFadein ) {
        if ( instr.trans === trCut) {
            fadeinCut(jno);
        }
        else {
            fadeinAnim(jno, instr.dur);
        }
        advanceReadyJob(activeJob);
        return;
    }

    // fadeout
    if ( op === opFadeout ) {
        if ( instr.trans === trCut) {
            fadeoutCut(jno);
        }
        else {
            fadeoutAnim(jno, instr.dur);
        }
        advanceReadyJob(activeJob);
        return;
    }

    // animated move and/or scaling of media
    if ( op === opMove ) {
        trc(1,`execInstr: op=${op}`);

        // move media

        advanceReadyJob(activeJob);
        return;
    }

    // add status to status set and wakeup waiting for status
    else if ( op === opSetstatus ) {
        const st = instr.st;

        trc(1,`job ${jno}: add status ${st}`);

        // wakeup jobs waiting for this job to reach status
        activeJob.jstatus.add(st);
        wakeupWaiting(mkWaitJob(jno, st));

        advanceReadyJob(activeJob);
        return;
    }

    // sleep a given time
    if ( op === opDelay ) {
        if ( instr.dur ) {
            execDelay(instr, activeJob); // async
            return;
        }
        advanceReadyJob(activeJob);
        return;
    }

    // syncing with previous slides
    if ( op === opWait ) {
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
        advanceReadyJob(activeJob);
        return;
    }

    // cleanup
    if ( op === opFinish ) {
        trc(1, `exit: job terminated, delete job ${jno}`);

        // // just for debugging
        // removeFrame(jno);
        // remJob(jno);

        advanceReadyJob(activeJob);
        return;
    }

    // default instr end
    // for instructions executed syncronized

    trc(1,`execInstr: op=${op} not yet implemented !!!`);
    advanceReadyJob(activeJob, st);
}

// --------------------

function execDelay(instr, aj) {
    const jno = aj.jno;

    // set termination action
    aj.jterm = function () {
        trc(1, `finish delay instr: (${jno}, ${aj.jpc})`);
        termInstr(jno, stNothing);
    };

    // set timeout
    aj.jtimeout = setTimeout(aj.jterm, instr.dur * 1000); // sec -> msec

    // put job into set of jobs running asyncronized
    trc(1, `execDelay: add to running: ${jno}`);
    addRunning(jno);
}

// --------------------

function termInstr(jno) {
    trc(1, `termInstr: jno=${jno}`);
    const aj = jobsAll.get(jno);
    trc(1, `termInstr: finalize instr (${jno}, ${aj.jpc})`);
    if ( aj ) {
        remRunning(jno);                   // remove from set of async jobs

        if ( aj.jtimeout ) {               // clear timeouts
            clearTimeout(aj.jtimeout);     // when interrupted by input events
            aj.timeout = null;
        }
        advanceReadyJob(aj);
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

function advanceReadyJob(aj) {
    const jno = aj.jno;
    advanceJob(aj);
    // trc(1, `advanceReadyJob: (${jno}, ${aj.jpc + 1}): status: ${st}`);
    addReady(jno);
}

// --------------------

function newFrame(id, stageGeo, fmt) {
    let geo = stageGeo;
    let off = V2(0,0);

    switch ( fmt ) {
    case 'leftHalf' :
        geo.w = div(stageGeo.w, 2);
        break;
    case 'rightHalf' :
        geo.w = stageGeo.w - div(stageGeo.w, 2);
        off.w = div(stageGeo.w, 2);
        break;
    default:
        break;
    }

    let s = cssAbsGeo(geo, off);
        s.display = "none";

    let e = newElem('div', id, s, 'frame');
    return e;
}

function mkFrameId(jno) {
    return `frame-${jno}`;
}

// --------------------
// build a new frame containing a media element

function render(jno, media) {
    const ty = media.type;
    const sg = stageGeo();

    switch ( ty ) {
    case 'text':
        renderText(jno, media, sg, stageId);
        break;
    default:
        throw `render: unknown media type: ${ty}`;
    }
}

function renderText(jno, media, frameGeo, parentId) {
    const frameId   = mkFrameId(jno);
    const frame     = newFrame(frameId, frameGeo, V2(0,0));

    // media geo is rel to frame geo
    const g = mulV2(media.geo, frameGeo);
    const o = mulV2(media.off, frameGeo);

    const ms  = cssAbsGeo(g, o);
    ms.height = "auto";
    ms['background-color'] = "red";

    const me        = newBlogElem(frameId, ms, 'text');
    me.innerHTML = media.text;
    frame.appendChild(me);
    getElem(parentId).appendChild(frame);
}

// --------------------
// transitions

function fadeCut(frameId, display, opacity) {
    setCSS(frameId,
           { display: display,
             opacity: opacity,
           }
          );
}

function fadeinCut (jno) { fadeCut(mkFrameId(jno), 'block', 1.0); }
function fadeoutCut(jno) { fadeCut(mkFrameId(jno), 'none', null); }

function fadeAnim(frameId, dur, fade) { // fade = 'fadein' or 'fadeout'
    const e   = getElem(frameId);
    const cls = `${fade}-image`;
    const opacity = (fade === 'fadein' ) ? 0.0 : 1.0;

    function handleFadeEnd(ev) {
        trc(1,`handleFadeEnd: ${frameId}, ${dur}, ${fade}`);
        ev.stopPropagation();
        e.classList.remove(cls);
        e.removeEventListener('animationend', handleFadeEnd);
        if ( fade === 'fadeout' ) {
            fadeCut(frameId, 'none', null);
        } else {
            fadeCut(frameId, 'block', 1.0);
        }
    }

    fadeCut(frameId, 'block', opacity);
    e.classList.add(cls);
    e.addEventListener('animationend', handleFadeEnd);

    // start animation
    setAnimDur(e, dur);
}

function fadeinAnim (jno, dur) { fadeAnim(mkFrameId(jno), dur, 'fadein'); }
function fadeoutAnim(jno, dur) { fadeAnim(mkFrameId(jno), dur, 'fadeout'); }

// ----------------------------------------
// cleanup

function removeFrame(jno) {
    const frameId  = mkFrameId(jno);
    const e = getElem(frameId);
    e.remove();
}

// ----------------------------------------
//
// job creation

var newJobNo = 0;

function mkJob(jno, jd) {
    // init code
    let cinit = [ mkInit(),
                  mkSetstatus(stCreated),
                  mkLoadpage(jd.url),
                  mkSetstatus(stReadypage),
                  mkWait(1, stReadymedia),  // wait for prev job loading media
                  mkLoadmedia(),
                  mkSetstatus(stReadymedia),
                  mkRender(jd.media),
                ];

    // --------------------
    // fadein code

    let cfadein = [];

    switch ( jd.fadeinTrans ) {
    case 'fadein' :
        cfadein = [ mkWait(1, stHidden),   // wait for previous job
                    mkFadein(jd.fadeinDur, trFadein),
                    mkSetstatus(stVisible),
                  ];
        break;

    case 'crossfade' :
        cfadein = [ mkWait(1, stShown),   // crossfade starts earlier
                    mkFadein(jd.fadeinDur, trFadein),
                    mkSetstatus(stVisible),
                  ];
        break;

    case trCut :
    default:
        cfadein = [ mkWait(1, stShown),
                    mkFadein(0, trCut),
                    mkSetstatus(stVisible),
                  ];
        break;
    }

    // --------------------
    // fadeout code

    let cfadeout = [];

    switch ( jd.fadeoutTrans ) {
    case 'fadeout' :
    case 'crossfade' :
        cfadeout = [ mkFadeout(jd.fadeoutDur, trFadeout),
                     mkDelay(jd.fadeoutDur),
                     mkSetstatus(stHidden),
                   ];
        break;

    case trCut :
    default:
        cfadeout = [ mkFadeout(0, trCut),
                     mkSetstatus(stHidden),
                   ];
        break;
    }

    // --------------------
    // show code

    let cshow = [ mkDelay(jd.showDur),
                  mkSetstatus(stShown),
                ];

    // --------------------
    // exit code

    let cexit = [ mkFinish(),
                  mkSetstatus(stFinished)
                ];

    // --------------------

    const code = [].concat(cinit, cfadein, cshow, cfadeout, cexit);

    return { jno:   jno,
             jcode: code
           };
}

// --------------------
/*
var s0 = `
  load      /emil/egon/003.jpg
  fadein    1.0s
  show      2.0s
  move      2.0s +20-30 1.2
  show      1.0s
  crossfade 2.0s
  `;

var s1 = [ mkFadein(1.0, trFadein),
           mkDelay(3.0),
           mkMove(2.0, V2(0.2,0.3), 1.2),
           mkDelay(3.0),
           mkFadeout(1.0, 'crossfade'),
         ];
*/

var j1 = mkJob(1, { url: "/emil",
                    showDur: 3.000,
                    fadeinDur: 1.000,
                    fadeinTrans: trFadein,
                    fadeoutDur: 2.000,
                    fadeoutTrans: trCrossfade,
                    media: { type: "text",
                             text: "<h1>Hallo Welt</h1><p>Hier bin ich!</p>",
                             geo: V2(0.50, 0.30),    // rel. to frame Geo
                             off: V2(0.10, 0.30),
                           }
                  }
              );
var j2 = mkJob(2, { url: "/egon",
                    showDur: 5.000,
                    fadeinDur: 2.000,
                    fadeinTrans: trCrossfade,
                    fadeoutDur: 1.000,
                    fadeoutTrans: trCrossfade,
                    media: { type: "text",
                             text: "<h2>Hallo Welt</h2>",
                             geo: V2(0.70, 0.10),
                             off: V2(0.20, 0.10),
                           }
                  }
              );

function ttt() {
    initVM();
    addJob(j1.jno, j1.jcode);
    addJob(j2.jno, j2.jcode);
    addReady(j1.jno);
    addReady(j2.jno);
    setAspectRatio(V2(4,3));
}
