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
const stAborted    = "aborted";
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
const opPlace     = "place";    // move without animation
const opSetData   = "setdata";
const opSetStatus = "setstatus";
const opDelay     = "delay";
const opWait      = "wait";
const opWaitInput = "waitinput";
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

function mkLoadpage() {
    return { op:  opLoadpage };
}

function mkLoadmedia() {
    return { op: opLoadmedia };
}

function mkRender(gix) {
    return { op:  opRender,
             gix: gix,
           };
}

function mkFadein(dur, trans) {
    return { op:    opFadein,
             trans: trans,
             dur:   dur,
           };
};

function mkFadeout(dur, trans) {
    return { op:    opFadeout,
             trans: trans,
             dur:   dur,
           };
};

function mkSetStatus(st) {
    return { op:  opSetStatus,
             st: st,
           };
};

function mkSetData(key, data) {
    return { op:   opSetData,
             key:  key,
             data: data,
           };
};

function mkMove(dur, gix) {
    return { op:  opMove,
             dur: dur,
             gix: gix
           };
};

function mkPlace(gix) {
    return { op:  opPlace,
             gix: gix
           };
};

function mkDelay(dur) {
    return { op:  opDelay,
             dur: dur
           };
}

function mkWaitInput() {
    return { op: opWaitInput
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
//
// basic compile macros

function cInit(name) {
    return [
        mkInit(),
        mkSetStatus(stCreated),
        mkSetData('name', name)
    ];
}

function cTerm() {
    return [
        mkFinish(),
        mkSetStatus(stFinished)
    ];
}

function cLoadImg(imgPathPos, frameGeo, geos, gix, jnoWait) {
    return [
        mkSetData('type', 'img'),
        mkSetData('imgPathPos', imgPathPos),
        mkSetData('frameGeo',   frameGeo || defaultFrameGeo),
        mkSetData('geos',       geos     || [defaultSlideGeo]),
        mkLoadpage(),
        mkSetStatus(stReadypage),
        mkWait(jnoWait || 1, stReadymedia), // wait for prev job loading media
        mkLoadmedia(),
        mkSetStatus(stReadymedia),
        mkRender(gix || 0),                 // 1. geo is initial geo
    ];
}

function cLoadText(frameGeo, geos, text, gix) {
    return [
        mkSetData('type', 'text'),
        mkSetData('frameGeo',     frameGeo || defaultFrameGeo),
        mkSetData('geos',         geos     || [defaultSlideGeo]),
        mkSetStatus(stReadypage),
        mkSetStatus(stReadymedia),
        mkSetData('text', text || "???"),
        mkRender(gix || 0),

    ];
}

function cView(dur) {
    if ( isPositive(dur) ) {
        return [
            mkDelay(dur),
        ];
    }
    else {
        return [
            mkWaitInput(),
        ];
    }
}

function cMove(dur, gix) {
    if ( isPositive(dur) ) {
        return [
            mkMove(dur, gix),
            cView(dur)
        ];
    }
    else {
        return [
            mkPlace(gix),
        ];
    }
}

/*
function wrap(node, c1) {
    function go(...args) {
        return {node: node, succ: c1(...args)};
    }
    return go;
}

const cFadein = wrap('cFadein', cFadein1);
*/

function cFadein(dur, fade0, waitJob) {
    const pj   = waitJob || 1;
    const fade =
          isPositive(dur)
          ? fade0
          : trCut;

    switch ( fade ) {
    case 'fadein':
        return [
            mkWait(pj, stHidden),   // wait for previous job
            mkFadein(dur, trFadein),
            mkDelay(dur),
            mkSetStatus(stVisible),
        ];
    case 'crossfade':
        return [
            mkWait(pj, stShown),   // crossfade starts earlier
            mkFadein(dur, trFadein),
            mkDelay(dur),
            mkSetStatus(stVisible),
        ];
    case trCut:
    default:
        return [
            mkWait(pj, stShown),
            mkFadein(0, trCut),
            mkSetStatus(stVisible),
        ];
    }
}

function cFadeout(dur, fade0) {
    const fade =
          isPositive(dur)
          ? fade0
          : trCut;

    switch ( fade ) {
    case 'fadeout':
    case 'crossfade':
        return [
            mkFadeout(dur, trFadeout),
            mkDelay(dur),
            mkSetStatus(stHidden),
        ];
    case trCut:
    default:
        return [
            mkFadeout(0, trCut),
            mkSetStatus(stHidden),
        ];
    }
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
    gs = gs || defaultSlideGeo;
    const gs1 = {...gs, alg: 'fix'};
    return cLoadText(defaultFrameGeo, [gs1], text);
}

function cLoadImgStd(imgPath, gs) {
    return cLoadImg(imgPath, defaultFrameGeo, [gs]);
}

// --------------------
//
// view macros

// a slide view: fadein, fadeout and view steps

function cViewStd0(fadeinDur, fadeinTr, fadeoutDur, fadeoutTr, cView) {
    return [
        ...cFadein(fadeinDur, fadeinTr),
        ...cView,
        ...cFadeout(fadeoutDur, fadeoutTr)
    ];
}

// a standard slide view: fadein, view, fadeout

function cViewStd(fadeinDur, fadeinTr, dur, fadeoutDur, fadeoutTr) {
    return cViewStd0(fadeinDur, fadeinTr,
                     fadeoutDur, fadeoutTr,
                     cView(dur)
                    );
}

// a standard view with crossfade in/out

function cViewCrossfade(dur, fadeDur) {
    return cViewStd(fadeDur, trCrossfade, dur, fadeDur, trCrossfade);
}

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
var jobsData;
var jobsAll;
var jobsWaiting;
var jobsInput;
var jobsReady;
var jobsRunning;

function initVM() {
    vmRunning   = false;
    vmStepCnt   = 0;
    jobsCode    = new Map();  // Map Jno [Instr]
    jobsData    = new Map();  // Map Jno JobData
    jobsAll     = new Map();  // Map Jno ActiveJob
    jobsWaiting = new Map();  // Map Jno (Map Status (Set Jno))
    jobsInput   = [];         // Queue Jno
    jobsReady   = [];         // Queue Jno
    jobsRunning = new Set();  // Set Jno
}

initVM();

// --------------------
// jobs:

function addJob(jno, jcode) {
    jobsCode.set(jno, jcode);
    jobsData.set(jno, {});
    jobsAll.set(jno, mkActiveJob(jno, 0, new Set()));
}

function remJob(jno) {
    jobsCode.delete(jno);
    jobsData.delete(jno);
    jobsAll.delete(jno);
};

function getCode(jno) {
    return jobsCode.get(jno);
}

function getData(jno) {
    return jobsData.get(jno);
}

function noMoreJobs() {
    return ! readyJobs() && ! runningJobs() && ! waitingForInputJobs();
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
        if ( res === 'step') {
            trc(1, `${vmStepCnt}. step finished`);
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
        let ci = getCode(aj.jno)[aj.jpc];

        trc(1, `(${aj.jno}, ${aj.jpc}): ${ci.op}`);

        execInstr(ci, aj);
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
        render(activeJob, jobsData.get(jno), instr.gix);
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
        move(activeJob, jobsData.get(jno), instr.dur, instr.gix);
        advanceReadyJob(activeJob);
        return;
    }

    // move and/or scale without animation
    if ( op === opPlace ) {
        place(activeJob, jobsData.get(jno), instr.gix);
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opSetData ) {
        trc(1,`execInstr: op=${op}: key=${instr.key}`);
        const data = getData(jno);
        data[instr.key] = instr.data;

        advanceReadyJob(activeJob);
        return;
    }

    // add status to status set and wakeup waiting for status
    if ( op === opSetStatus ) {
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

    // put job into queue of jobs waiting for click
    if ( op === opWaitInput ) {
        addInputJob(jno);
        advanceJob(activeJob);
        return;
    }

    // syncing with previous slides
    if ( op === opWait ) {
        const jno1 = jno - instr.reljno;
        const st1  = instr.status;

        trc(1, `wait: ${jno} requires (${jno1}, ${st1})`);

        const aj1  = jobsAll.get(jno1);
        if ( aj1                                // job j1 exists
             &&
             ( ! (aj1.jstatus.has(st1)          // job j1 already has reached st1
                  ||
                  aj1.jstatus.has(stFinished)   // job j1 already finished
                 )
             )
           ) {
                // jno1 is too slow
                // put job into wait queue
                // and setup syncronizing

                trc(1, `wait: add to waiting jobs: jno=${jno1}, status=${st1}`);
                const aj = advanceJob(activeJob);        // finish wait instr
                addWaiting(jno, mkWaitJob(jno1, st1));
                return;
        }

        // job not blocked, instr becomes a noop
        trc(1, `wait: job ${jno1} already has status ${st1}, continue`);
        advanceReadyJob(activeJob);
        return;
    }

    // cleanup
    if ( op === opFinish ) {
        trc(1, `exit: normal job termination: ${jno}`);
        terminateJob(activeJob);
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
        termAsyncInstr(jno);
    };

    addAsyncRunning(jno);
    // set timeout
    aj.jtimeout = setTimeout(aj.jterm, instr.dur * 1000); // sec -> msec
}

// --------------------

function abortJob(activeJob) {
    trc(1, `abortJob: job abborted: ${activeJob.jno}`);
    activeJob.jstatus.add(stAborted);
    terminateJob(activeJob);
}

function terminateJob(activeJob) {
    const jno = activeJob.jno;

    trc(1, `terminateJob: job terminated, delete job: ${jno}`);

    // // just for debugging
    // removeFrame(jno);
    // remJob(jno);

    activeJob.jstatus.add(stFinished);
    wakeupAllWaiting(jno);
    advanceReadyJob(activeJob);

}

// --------------------

function termAsyncInstr(jno) {
    trc(1, `termAsyncInstr: jno=${jno}`);
    const aj = jobsAll.get(jno);
    trc(1, `termAsyncInstr: finalize instr (${jno}, ${aj.jpc})`);
    if ( aj ) {
        if ( aj.jtimeout ) {               // clear timeouts
            clearTimeout(aj.jtimeout);     // when interrupted by input events
            aj.timeout = null;
        }
        remAsyncRunning(jno);                   // remove from set of async jobs
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

function loadPage(activeJob, jobData) {
    const jno = activeJob.jno;
    const req = { rType:    'json',
                  geo:      'org',
                  rPathPos: jobData.imgPathPos,
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
            const mxGeo         = maxGeo(frameGeo, imgGeo, jobData.geos);

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
        advanceReadyJob(activeJob);
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
                      rPathPos: jobData.imgPathPos
                    };
        const url = imgReqToUrl(req, jobData.imgReqGeo);
        jobData.imgUrl = url;

        trc(1, `loadMedia: ${url}`);

        addAsyncRunning(jno);
        jobData.imgCache.onload = function () {
            const img = jobData.imgCache;
            jobData.imgResGeo = V2(img.naturalWidth, img.naturalHeight);

            remAsyncRunning(jno);            // remove job from async jobs
            advanceReadyJob(activeJob); // next step(s)
        };
        jobData.imgCache.src = url;     // triggers loading of image into cache
        return;

    case 'text':                        // no request required for type 'text'
        advanceReadyJob(activeJob);
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
    return `frame-${jno}`;
}

function imgGeoToCSS(jobData, gix) {
    const frameGeo = jobData.frameGO.geo;
    const imgGeo   = jobData.imgGeo;
    const go       = placeMedia(frameGeo, imgGeo)(jobData.geos[gix]);
    jobData.gix    = gix;          // save current geo spec index
    jobData.go     = go;           // save current abs geo/off
    return cssAbsGeo(go);
}

// --------------------

function place(activeJob, jobData, gix) {
    const fid  = mkFrameId(activeJob.jno);
    const ms   = imgGeoToCSS(jobData, gix);

    trc(1,`place: ${activeJob.jno} gix=${gix}`);
    setCSS(mkImgId(fid), ms);
}

function move(activeJob, jobData, dur, gix) {
    const fid   = mkFrameId(activeJob.jno);
    const cssId = mkCssId(activeJob.jno, gix);
    const imgId = mkImgId(fid);

    const ms0   = cssAbsGeo(jobData.go);     // transition start
    const ms1   = imgGeoToCSS(jobData, gix); // transition end

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
    }

    trc(1,`move: ${activeJob.jno} dur=${dur} gix=${gix}`);

    i.addEventListener('animationend', animEnd);
    i.classList.add(cssId);
}

function moveCSS(ms) {
    return `width: ${ms.width}; height: ${ms.height}; left: ${ms.left}; top: ${ms.top}`;
}

function mkCssId(jno, gix) {
    return `css-job-${jno}-${gix}`;
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

function render(activeJob, jobData, gix) {
    const jno = activeJob.jno;
    const ty  = jobData.type;
    const sg  = stageGeo();
    const fid = mkFrameId(jno);

    switch ( ty ) {
    case 'text':
        renderText(jobData, fid, sg, stageId, gix);
        break;

    case 'img':
        renderImg(jobData, fid, sg, stageId, gix);
        break;

    default:
        abortJob(activeJob);
        // throw `render: unsupported media type: ${ty}`;
    }
}

function renderFrame(jobData, frameId, stageGeo, frameCss) {
    const frameGO   = placeFrame(stageGeo, jobData.frameGeo);
    jobData.frameGO = frameGO;  // save frame geo/off

    return newFrame(frameId, frameGO, frameCss);
}

function renderImg(jobData, frameId, stageGeo, parentId, gix) {
    const frameCss = { opacity: 0,
                       visibility: 'hidden',
                       display:    'block',
                       overflow:   'hidden',
                     };
    const frame    = renderFrame(jobData, frameId, stageGeo, frameCss);
    const ms       = imgGeoToCSS(jobData, gix);
    const me       = newImgElem(frameId, ms, 'img');
    me.src         = jobData.imgUrl;

    frame.appendChild(me);
    getElem(parentId).appendChild(frame);
}

function renderText(jobData, frameId, stageGeo, parentId, gix) {
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

    const gs   = jobData.geos[gix];
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
    me.innerHTML = jobData.text;
    frame.appendChild(me);
    getElem(parentId).appendChild(frame);

    // compute real size of text
    // recompute geo/off for text
    // and overwrite preliminary geo/off

    const rect    = me.getBoundingClientRect();

    // round up geo, else unwanted linebreaks are added during rendering
    const textGeo = ceilV2(V2(rect.width, rect.height));
    const go3     = placeMedia(frameGeo, textGeo)(gs);
    const ms3     = cssAbsGeo(go3);
    setCSS(me, ms3);
}

// --------------------
// transitions

function fadeCut(frameId, visibility, opacity) {
    setCSS(frameId,
           { // display: display,
             opacity: opacity,
             visibility: visibility,
           }
          );
}

function fadeinCut (jno) { fadeCut(mkFrameId(jno), 'visible', 1.0); }
function fadeoutCut(jno) { fadeCut(mkFrameId(jno), 'hidden',  0.0); }

function fadeAnim(frameId, dur, fade) { // fade = 'fadein' or 'fadeout'
    const e       = getElem(frameId);
    const cls     = `${fade}-image`;
    const opacity = (fade === 'fadein' ) ? 0.0 : 1.0;

    function handleFadeEnd(ev) {
        trc(1,`handleFadeEnd: ${frameId}, ${dur}, ${fade}`);
        ev.stopPropagation();
        e.classList.remove(cls);
        e.removeEventListener('animationend', handleFadeEnd);
        if ( fade === 'fadeout' ) {
            fadeCut(frameId, 'hidden', 0.0);
        } else {
            fadeCut(frameId, 'visible', 1.0);
        }
    }

    fadeCut(frameId, 'visible', opacity);
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

    e && e.remove();
}

// ----------------------------------------
//
// job creation

var newJobNo = 0;

const defaultFrameGeo = {    // full frame
    alg:   'fitInto',
    scale: V2(1.0,1.0),
    dir:   'center',
    shift: V2(0,0),
}
;
const leftHalfGeo = {
    alg:   'fitInto',
    scale: V2(0.5,1.0),
    dir:   'W',
    shift: V2(0,0),
};

const rightHalfGeo = {
    alg:   'fitInto',
    scale: V2(0.5,1.0),
    dir:   'E',
    shift: V2(0,0),
};

const defaultSlideGeo = {
    alg:   'fitInto',  // default
    scale: 1.0,        // default
    dir:   'center',   // default
    shift:  V2(0,0),   // default
};

// --------------------

function mkJob(jno,code) {
    return {jno: jno, jcode: code};
}

var j1 =
    cJob('HalloWelt',
         cLoadText1("<h1>Hallo Welt</h1><p>Hier bin ich!</p>",
                    {dir: 'NW', shift: V2(0.10,0.10)},
                   ),
         cViewStd(1.0, trFadein, 3.0, 2.0, trCrossfade)
        );

var j2 =
    cJob('Hallo',
         cLoadText(rightHalfGeo,
                   [{alg: 'fix', dir: 'SE', shift: V2(-0.20,-0.20)}],
                   "<h2>Hallo Welt</h2>"),
         cViewStd0(0.5, trCrossfade,
                   0.5, trCrossfade,
                   [...cView(3.0),
                    // more view steps
                   ]
                  )
        );

var j3 =
    cJob('Ente1',
         cLoadImg(['/archive/collections/albums/EinPaarBilder',1],
                  defaultFrameGeo,
                  [ defaultSlideGeo,
                    {alg: 'fill',    scale: 2.0, dir: 'center',},
                    {alg: 'fitInto', scale: 0.5, dir: 'center',},
                    {alg: 'fix',},                  // real img size
                    {alg: 'fitInto', scale: 0.5, dir: 'W',},
                    defaultSlideGeo,
                  ]
                 ),
         cViewStd0(2.0, trCrossfade,
                   2.0, trCrossfade,
                   [...cView(2.0),
                    ...cMove(3.0,1),
                    ...cView(3.0),
                    ...cMove(5.0,2),
                    ...cView(3.0),
                    ...cMove(2.0,3),
                    ...cView(3.0),
                    ...cMove(1.0,4),
                    ...cView(3.0),
                    ...cMove(1.0,5),
                    ...cView(3.0),
                   ]
                  )
        );

var j4 =
    cJob("Ente2",
         cLoadImgStd(['/archive/collections/albums/EinPaarBilder',2],
                     {alg: 'fill', scale: 1.1, dir: 'center'}
                    ),
         cViewCrossfade(5.0,1.0)
        );

var j6 =
    cJob("arizona",
         cLoadImg(['/archive/collections/clipboard',0],
                  defaultFrameGeo,
                  [{alg: 'sameHeight', scale: 1, dir: 'W'},
                   {alg: 'sameHeight', scale: 1, dir: 'E'},
                   {alg: 'sameWidth',  scale: 1, dir: 'center'},
                 ]
                 ),
         cViewStd0(1.5, trCrossfade,
                   1.5, trCrossfade,
                   [...cView(2.0),
                    ...cMove(10.0,1),
                    ...cView(-1),
                    ...cMove(4.0,2),
                    ...cView(3.0),
                   ]
                  )
        );

var j5 =
    cJob('Ende',
         cLoadText1(`<h1>The End</h1>
                     <div>This is the end, my friend.</div>`
                   ),
         cViewStd(1.0, trFadein, 'click', 5.0, trFadeout)
        );

var jobList = [
//    j1,
//    j2,
//    j3,
//    j6,
//    j4,
    j5,
];

function restartJobs(js) {
    for (let i = 0; i < js.length; ++i) {
        // remove comments in terminateJob to cleanup on the fly
        removeFrame(i+1);    // cleanup DOM when restartd
        remJob(i+1);         // cleanup job data when restarted
        addJob(i+1, js[i]);
    }
    for (let i = 0; i < js.length; ++i) {
        addReady(i+1);
    }
}

function ttt() {
    setAspectRatio(V2(4,3));
    initVM();
    restartJobs(jobList);
}

function click() {
    wakeupInputJobs();
}

// ----------------------------------------
