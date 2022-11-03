// ----------------------------------------
// the slideshow VM


// all timings are in seconds

// ----------------------------------------
// stati

const stCreated    = "created";
const stReadypage  = "pageready";
const stReadymedia = "mediaready";
const stVisible    = "visible";
const stShown      = "shown";
const stHidden     = "hidden";
const stFinished   = "finished";
const stAborted    = "aborted";

const statiWords = [
    stCreated,
    stReadypage,
    stReadymedia,
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
const opWaitInput = "waitinput";
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
    opWaitInput,
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

// build instructions

function mkInit(name) {
    return { op:   opInit,
             name: name,
           };
}

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

function mkText(text) {
    return { op:   opText,
             text: text,
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

function mkWaitInput() {
    return { op: opWaitInput
           };
}

function mkWait(st, job) {
    if ( isNumber(job) ) {
        return { op:     opWait,
                 reljno: job,
                 status: st,
               };
    }
    return { op:     opWait,
             name:   job,
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
        mkFrame(frameGS || defaultFrameGS),
        mkLoadpage(),
        mkWait(stReadymedia, jnoWait || -1), // wait for prev job loading media
        mkLoadmedia(),
        mkRender(gs),                 // 1. geo is initial geo
    ];
}

function cLoadText(frameGS, text, gs) {
    return [
        mkType('text'),
        mkFrame(frameGS || defaultFrameGS),
        mkText(text || "???"),
        mkRender(gs),
    ];
}

function cView(dur) {
    let is = [];
    if ( isPositive(dur) ) {
        is.push(mkDelay(dur));
    }
    if ( dur === 'click' ) {
        is.push(mkWaitInput());
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
    const prevJob = waitJob || -1;
    const fade =
          isPositive(dur)
          ? fade0
          : trCut;

    switch ( fade ) {
    case 'fadein':
        return [
            mkWait(stHidden, prevJob),   // wait prev image to be hidden
            mkFadein(dur, trFadein),
        ];
    case 'crossfade':
        return [
            mkWait(stShown, prevJob),   // wait prev image starts fading out
            mkFadein(dur, trFadein),
        ];
    case trCut:
    default:
        return [
            mkWait(stShown, prevJob),
            mkFadein(0, trCut),
        ];
    }
}

function cFadeout(dur, fade0) {
    const fade =
          isPositive(dur)
          ? fade0
          : trCut;

    switch ( fade ) {
    case 'crossfade':
    case 'fadeout':
        return [
            mkStatus(stShown),
            mkFadeout(dur, trFadeout),
        ];
    case trCut:
    default:
        return [
            mkStatus(stShown),
            mkFadeout(0, trCut),
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
    gs = gs || defaultGS;
    const gs1 = {...gs, alg: 'fix'};
    return cLoadText(defaultFrameGS, text, gs1);
}

function cLoadImgStd(imgPath, gs) {
    return cLoadImg(imgPath, defaultFrameGS,gs);
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
        getData(jno).name = instr.name;
        setStatus(activeJob, stCreated);

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
        render(activeJob, jobsData.get(jno), instr.gs || instr.gix);
        advanceReadyJob(activeJob);
        return;
    }

    // fadein
    if ( op === opFadein ) {
        if ( instr.trans === trCut) {    // sync exec
            fadeinCut(activeJob);
            advanceReadyJob(activeJob);
        }
        else {
            fadeinAnim(activeJob, instr.dur);  // async exec
        }
        return;
    }

    // fadeout
    if ( op === opFadeout ) {
        if ( instr.trans === trCut) {
            fadeoutCut(activeJob);
            advanceReadyJob(activeJob);
        }
        else {
            fadeoutAnim(activeJob, instr.dur);
        }
        return;
    }

    // animated move and/or scaling of media, async exec
    if ( op === opMove ) {
        move(activeJob, jobsData.get(jno), instr.dur, instr.gs);
        return;
    }

    // move and/or scale without animation
    if ( op === opPlace ) {
        place(activeJob, jobsData.get(jno), instr.gs || instr.gix);
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opType ) {
        const data = getData(jno);
        data.type  = instr.type;
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opFrame ) {
        const data = getData(jno);
        data.frameGS = instr.gs;
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opText ) {
        getData(jno).text = instr.text;

        setStatus(activeJob, stReadypage);
        setStatus(activeJob, stReadymedia);
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opPath ) {
        const data = getData(jno);
        data.path = instr.path;
        advanceReadyJob(activeJob);
        return;
    }

    if ( op === opStatus ) {
        setStatus(activeJob, instr.st);
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
        let jno1 = jno - 1;
        if ( isNumber(instr.reljno) ) {
            jno1 = jno + instr.reljno;    // reljno is usually -1 (prev job)
        } else {
            const jname = instr.name;
            trc(1, `wait: job names not yet implemented`);
        }
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
        setStatus(activeJob, stFinished);
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

// add status to status set and wakeup waiting for status

function setStatus(activeJob, st) {
    activeJob.jstatus.add(st);
    trc2('setStatus: job', activeJob);

    // wakeup jobs waiting for this job to reach status
    wakeupWaiting(mkWaitJob(activeJob.jno, st));
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
        remAsyncRunning(jno);              // remove from set of async jobs
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
            const gss           = instrGS(jobsCode.get(jno));
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
            advanceReadyJob(activeJob);         // next step(s)
        };
        jobData.imgCache.src = url;     // triggers loading of image into cache
        return;

    case 'text':                        // no request required for type 'text'
        setStatus(activeJob, stReadymedia);
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

function imgGeoToCSS(jobData, gs) {
    const frameGeo = jobData.frameGO.geo;
    const imgGeo   = jobData.imgGeo;
    const go       = placeMedia(frameGeo, imgGeo)(gs);
    jobData.gs     = gs;           // save current geo spec
    jobData.go     = go;           // save current abs geo/off
    return cssAbsGeo(go);
}

// --------------------

function place(activeJob, jobData, gs) {
    const ms   = imgGeoToCSS(jobData, gs);

    trc(1,`place: ${activeJob.jno} gs=${pp.gs(gs)}`);
    setCSS(mkImgId(fid), ms);
}

let cssAnimCnt = 1;

function move(activeJob, jobData, dur, gs) {
    const jno   = activeJob.jno;
    const fid   = mkFrameId(jno);
    const cssId = mkCssId(jno, cssAnimCnt++);
    const imgId = mkImgId(fid);

    const ms0   = cssAbsGeo(jobData.go);     // transition start
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
    return `css-job-${jno}-${cssCnt}`;
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

function render(activeJob, jobData, gs) {
    const jno = activeJob.jno;
    const ty  = jobData.type;
    const sg  = stageGeo();
    const fid = mkFrameId(jno);

    switch ( ty ) {
    case 'text':
        renderText(jobData, fid, sg, stageId, gs);
        break;

    case 'img':
        renderImg(jobData, fid, sg, stageId, gs);
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

function fadeinCut (activeJob) {
    fadeCut(mkFrameId(activeJob.jno), 'visible', 1.0);
    setStatus(activeJob, stVisible);
}
function fadeoutCut(activeJob) {
    fadeCut(mkFrameId(activeJob.jno), 'hidden',  0.0);
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
            fadeCut(frameId, 'hidden', 0.0);
            setStatus(activeJob, stHidden);
        } else {
            fadeCut(frameId, 'visible', 1.0);
            setStatus(activeJob, stVisible);
        }
        termAsyncInstr(jno);
    }

    fadeCut(frameId, 'visible', opacity);
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

// ----------------------------------------
//
// job creation

var newJobNo = 0;

// geo spec constructor
function GS(alg, scale, dir, shift) {
    return {
        alg:   alg,
        scale: scale,
        dir:   dir,
        shift: shift,
    };
}

const defaultFrameGS = {    // full frame
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

const defaultGS = {
    alg:   'fitInto',  // default
    scale:  V2(1.0),   // default
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
                    GS('fix', V2(1), 'NW', V2(0.10,0.10)),
                   ),
         cViewStd(1.0, trFadein, 3.0, 2.0, trCrossfade)
        );

var j2 =
    cJob('Hallo',
         cLoadText(rightHalfGeo,
                   "<h2>Hallo Welt</h2>",
                   GS('fix', V2(1), 'SE', V2(-0.20,-0.20)),
                  ),
         cViewStd0(0.5, trCrossfade,
                   0.5, trCrossfade,
                   [...cView(3.0),
                    // more view steps
                   ]
                  )
        );

var j3 =
    cJob('Ente1',
         cLoadImg('/albums/EinPaarBilder/pic-00001',
                  defaultFrameGS,
                  defaultGS,
                 ),
         cViewStd0(2.0, trCrossfade,
                   2.0, trCrossfade,
                   [...cView(2.0),
                    ...cMove(3.0,GS('fill',V2(2),'ceter',V2())),
                    ...cView(3.0),
                    ...cMove(5.0,GS('fitInto',V2(0.5),'center',V2())),
                    ...cView(3.0),
                    ...cMove(2.0,GS('fix',V2(1),'center',V2())),
                    ...cView(3.0),
                    ...cMove(1.0,GS('fitInto',V2(0.5),'W',V2())),
                    ...cView(3.0),
                    ...cMove(1.0,defaultGS),
                    ...cView(3.0),
                   ]
                  )
        );

var j4 =
    cJob("Ente2",
         cLoadImgStd('/albums/EinPaarBilder/pic-0002',
                     GS('fill', V2(1.1), 'center', V2()),
                    ),
         cViewCrossfade(5.0,1.0)
        );

var j6 =
    cJob("arizona",
         cLoadImg('/clipboard/pic-0000',
                  defaultFrameGS,
                  {alg: 'sameHeight', scale: V2(1), dir: 'W', shift: V2()},
                 ),
         cViewStd0(1.5, trCrossfade,
                   1.5, trCrossfade,
                   [...cView(2.0),
                    ...cMove(10.0,
                             {alg: 'sameHeight', scale: V2(1),
                              dir: 'E', shift: V2()}),
                    ...cView('click'),
                    ...cMove(4.0,
                             {alg: 'sameWidth',  scale: V2(1),
                              dir: 'center', shift: V2()}),
                    ...cView(3.0),
                   ]
                  )
        );

var j5 =
    cJob('Ende',
         cLoadText1(`<h1 class='text-center'>The End</h1>
                     <div>This is the end, my friend.</div>`,
                    defaultGS,
                   ),
         cViewStd(1.0, trFadein, 'click', 5.0, trFadeout)
        );

var jobList = [
    j3, j1, j2,
    j6,
    j4,
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

function ppSimple(x) { return "" + x; }
function ppDur(x) { return "" + x + "s"; }

function ppPath(l) {
    return l[0] + (l[1] ? ppSimple(l[1]) : "");
}

function ppPathPrefix(p) { return rePathPx + p; }

function ppGS(gs) {
    return filter((x) => {return x !== "";})([
        pp.text(gs.alg),
        pp.scale(gs.scale),
        (gs.dir === dirDefault) ? "" : pp.text(gs.dir),
        (eqV2(gs.shift, V2(0,0)) ? "" : pp.off(gs.shift))
    ]).join(" ");
}

function ppScale(sc) {
    if ( eqV2(sc, V2(1,1)) )
        return "";
    if ( sc.x === sc.y)
        return pp.num(sc.x);
    return pp.geo(sc);
}

function ppInstr(i) {
    const op = i.op;
    let  res = [fillR(10, op)];

    switch ( op ) {
    case opLoadpage:
    case opLoadmedia:
    case opWaitInput:
    case opFinish:
        break;

    case opInit:
        res.push(i.name);
        break;

    case opType:
        res.push(i.type);
        break;

    case opFrame:
        res.push(pp.gs(i.gs));
        break;

    case opText:
        res.push(JSON.stringify(i.text));
        break;

    case opPath:
        res.push(i.path);
        break;

    case opRender:
    case opPlace:
        res.push(pp.gs(i.gs));
        break;

    case opFadein:
    case opFadeout:
        res.push(i.trans, pp.dur(i.dur));
        break;

    case opStatus:
        res.push(i.st);
        break;

    case opMove:
        res.push(pp.dur(i.dur));
        res.push(pp.gs(i.gs));
        break;

    case opDelay:
        res.push(pp.dur(i.dur));
        break;

    case opWait:
        res.push(i.status);
        if ( isNumber(i.reljno) ) {
            if ( i.reljno !== -1 ) {
                res.push(pp.num(i.reljno));
            }
        } else {
            res.push(i.name);
        }
        break;

    default:
        res.push('unknown op');
    }
    return nl(unwords(res));
}

function showCode(is) {
    return (map(ppInstr)(is)).join("");
}

const pp = {
    text:     ppSimple,
    num:      ppSimple,
    dur:      ppDur,
    dir:      ppSimple,
    clickdur: ppSimple,
    geo:      showGeo,
    off:      showOff,
    go:       showGO,
    gs:       ppGS,
    scale:    ppScale,
    path:     ppPath,
    pathpx:   ppPathPrefix,
    instr:    ppInstr,
    code:     showCode,
};

// ----------------------------------------

function initParserState(inp) {
    let s0 = {
        inp : inp,
        ix  : 0,
        cc  : 0,
        lc  : 0,
        backtrack: false,
    };

    // save the variable parts of the parser state
    function saveState() {
        return [s0.ix, s0.cc, s0.lc, s0.backtrack];
    }

    // restore saved parser state
    // for backtracking in alt and followedBy parsers
    function resetState(cs) {
        s0.ix = cs[0];
        s0.cc = cs[1];
        s0.lc = cs[2];
        s0.backtrack = cs[3];
    }

    function resetBacktrack(cs) {
        s0.backtrack = cs[3];
    }
    // prepare error message with source line and position
    function showErr(msg) {
        const ls  = lines(s0.inp);
        const lc1 = s0.lc;
        const lc0 = Math.max(0, lc1 - 3);
        let   ls1 = take(lc1 - lc0 + 1, drop(lc0, ls));
        const l2  = replicate(s0.cc, " ") + "^^^^^";
        const l3  = `line ${s0.lc + 1}: ${msg}`;
        ls1.push(l2,l3,"");
        trc(1, unlines(ls1));
    }

    function rest() {
        return s0.inp.slice(s0.ix);
    }

    s0.save   = saveState;
    s0.reset  = resetState;
    s0.resetBacktrack = resetBacktrack;
    s0.errmsg = showErr;
    s0.rest   = rest;
    return s0;
}

// object used as module

function parse(p1, inp) {
    const s0 = initParserState(inp);
    trc(1,JSON.stringify(s0));

    const res = p1(s0);
    trc(1,JSON.stringify(res));

    return res;
}

function parse1(p1, inp) {
    const res = parse(p1, inp);

    if ( res.err ) {
        const msg = res.state.errmsg(res.err);
        trc(1, msg);
        return "";
    }
    else {
        return res.res;
    }
}

function succ(res, state) { return {res: res, state: state}; };
function fail(err, state) { return {err: err, state: state}; };
//                                            ^^^^^^^^^^^^
//                                            for better error reporting

// (a -> Bool) -> Parser a
function satisfy(pred) {
    return (state) => {
        const c = state.inp[state.ix];
        if ( c ) {
            if ( pred(c) ) {
                state.ix++;
                if ( c === "\n" ) {
                    state.lc += 1;
                    state.cc  = 0;
                }
                else {
                    state.cc += 1;
                }
                return succ(c,state);
            }
            else {
                return fail(`unexpected char '${c}'`, state);
            }
        }
        else {
            return fail("end of input", state);
        }
    };
}

// Parser a
const item = satisfy(cnst(true));

// Char -> Parser String
function char(c) {
    return satisfy((c1) => { return (c === c1)});
}

// Set Char -> Parser Char
function oneOf(s) {
    return satisfy((c1) => {
        return s.indexOf(c1) >= 0;
    });
}

// Set Char -> Parser Char
function noneOf(s) {
    return satisfy((c1) => {
        return s.indexOf(c1) < 0;
    });
}

// a -> Parser a
function unit(res) {
    return (state) => { return succ(res, state); };
}

// mark state to not backtrack in an alt parser
// but fail instead
//
// Parser ()
function pCut(state) {
    state.backtrack = false;
    return succ(Void, state);
}

// String -> Parser a
function failure(msg) {
    return (state) => { return fail(msg, state); };
}

function withErr(p, msg) {
    return (state) => {
        const res = p(state);
        if ( res.err ) {
            res.err = msg;
        }
        return res;
    };
}

// Parser ()
function eof(state) {
    trc(1,`eof: ${state.inp} ${state.ix}`);
    const c = state.inp[state.ix];
    if ( ! c ) {
        return succ(Void, state);
    }
    else {
        const rest = state.inp.slice(state.ix, state.ix + 40);
        return fail(`end of input expected, but seen: '${rest}...'`, state);
    }
}

// Parser a -> Parser ()
const del = (p) => {
    return fmap(cnst(Void), p);      // discard result, e.g. for separators
};

// Parser String -> Parser ()        // Void = ()
function followedBy(p) {
    function go(state) {
        const s1 = state.save();
        const r1 = p(state);
        state.reset(s1);
        if ( r1.err ) {
            return fail("followed context not matched", state);
        }
        else {
            return succ(Void, state);
        }
     }
    return go;
}

// Parser String -> Parser ()
function notFollowedBy(p) {
    return alt(seqT(followedBy(p),
                    failure("wrong context followed")
                   ),
               unit(Void)
              );
}

// (a -> b) -> Parser a -> Parser b
function fmap(f, p) {
        return (st0) => {
            const r1 = p(st0);
            // trc2('fmap: r1=', r1);
            if ( r1.err ) {
                return r1;
            }
            else {
                r1.res = f(r1.res);
                // trc2("fmap: rs=", r1);
                return r1;
            }
        };
    }

// generalisation of <$> and <*>
// f <$> Parser a <*> Parser b <*> ...
//
// ((a, b, ...) -> r) -> Parser a -> Parser b -> ... -> Parser r
function app(f, ...ps) {
    return fmap((xs) => { return f(...xs); },
                seq(...ps)
               );
}

// Parser a -> Parser b -> Parser a                // <*  from Applicative
function cxR(p, cx) {
    return app(id, p, del(cx));                  // discard right context
}

// Parser b -> Parser a -> Parser a                // *>  from Applicative
function cxL(cx, p) {
    return app(id, del(cx), p);                  // discard left context
}

// Parser b -> Parser a -> Parser c -> Parser a    // *>  <* from Applicative
function cx(cx1, p, cx2) {
    return app(id, del(cx1), p, del(cx2));     // discard context
}

// Parser a -> (a -> Parser b) -> Parser b    // >>=
function bind(p1, f) {
        return (state) => {
            const r1 = p1(state);
            if ( r1.err ) {
                return r1;
            }
            else {
                const p2 = f(r1.res);
                return p2(r1.state);
            }
        };
    }

// [Parser a]                           -> Parser [a]
// (Parser a, Parser b, ...)            -> Parser (a, b, ...)
// (Parser a, Parser (), Parser b, ...) -> Parser (a, b, ...)

function seq(...ps) {
    function go(state) {
        let   st = state;
        let   xs = [];
        const l  = ps.length;
        if ( l === 0 ) {
            return succ(xs, state);
        }
        for (let i = 0; i < ps.length; i++) {
            const rs = ps[i](st);
            if ( rs.err )
                return rs;
            if ( rs.res !== Void ) {
                xs.push(rs.res);
            }
            trc(1,`seq: ${JSON.stringify(rs.res)} ${JSON.stringify(xs)}`);
            st = rs.state;
        }
        return succ(xs.length === 0 ? Void : xs, st);
    }
    return go;
}

// Parser a -> Parser a -> Parser a
function alt(p1, p2) {
        return (state) => {
            const s1 = state.save();

            // allow backtracking, as long as not cut parser is reached
            state.backtrack = true;
            // trc2("alt: state=", state);

            const r1 = p1(state);
            trc2("alt: r1=", r1);

            if ( r1.err ) {                     // p1 failed
                if ( r1.state.backtrack ) {     // backtracking allowed
                    state.reset(s1);            // reset state and
                    const r2 = p2(state);       // run p2
                    // trc2("alt: r2=", r2);
                    return r2;
                }
                else {                          // no backtracking
                    // r1.state.resetBacktrack(s1);// restore backtracking flag
                    trc(1, "alt: no backtracking");
                    return r1;                  // and report error
                }
            }
            else {                              // p1 succeded
                r1.state.resetBacktrack(s1);    // restore backtracking flag
                return r1;                      // and return result
            }
        };
    }

// (Parser a, ...) -> Parser a
function alts(...ps) {
    let res = failure("no alternatve matched");
    for (let i = ps.length; i > 0; i--) {
        res = alt(ps[i-1], res);
    }
    return res;
}

// a -> Parser a -> Parser a
function opt(defVal, p) {
    return alt(p, unit(defVal));
}

// many and some parsers always return a list
// if parser p is of type 'Parser ()', the result
// will be always the empty list

// Parser a  -> Parser [a]
// Parser () -> Parser []  (res: empty list)

function many(p) {
    function go(state) {
        let   st = state;
        let   xs = [];                       // result accumulator

        while ( true ) {
            const s1 = st.save();
            st.backtrack = true;
            let rs = p(st);

            if ( rs.err ) {
                if ( rs.state.backtrack ) {  // terminate loop
                    st.reset(s1);            // reset state
                    const res = succ(xs, st);
                    trc2('many: res:', res);
                    return res;              // and return xs
                }
                else {                       // error when running p
                    trc2("many: failed:", rs);
                    return rs;               // propagate error
                }
            }
                                             // one more item parsed
            if ( rs.res !== Void ) {
                xs.push(rs.res);             // update xs
            }
            st = rs.state;                   // update st
        }
    }
    return go;
}

// Parser a  -> Parser [a]
// Parser () -> Parser []  (res: empty list)
function some(p) {
    function cons1(...args) {
        // trc(1,`cons1: ${JSON.stringify(args)}`);
        if ( args.length === 1 ) {
            return id(...args);           // return args[0];
        }
        if ( args.length === 2 ) {
            return cons(...args);
        }
        throw "some: cons1: wrong args";
    }
    return app(cons1, p, many(p));
}

// Parser [String] -> Parser String
function joinT(p) {
    return fmap((xs)  => { return concatS(...xs); }, p);
}

// Parser String -> Parser String
const manyT = (p)     => { return joinT(many(p)); };
const someT = (p)     => { return joinT(some(p)); };

// (Parser String, ...) -> ParserString
const seqT  = (...ps) => { return joinT(seq(...ps)); };

// String -> Parser String
function word(w) {
    const ps = map((c) => {return char(c);})(w);
    return seqT(...ps);
}

// CAUTION: if a word w1 is prefix of a word w2
// w1 must be put into the list behind w2
// else tokenising does not word properly
// right: ['fixandfoxi, fix']
// wrong: ['fix', 'fixandfoxi']
// see 'wordToken'

function words(ws) {
    const ps = map(word)(ws);
    return alts(...ps);
}

// for not mess up with quoting

function charOrd(n) {
    return char(fromOrd(n));
}

function oneOfOrd(...ns) {
    return oneOf(fromOrd(...ns));
};

function noneOfOrd(...ns) {
    return noneOf(fromOrd(...ns));
};

function wordOrd(...ns) {
    return word(fromOrd(...ns));
}

// --------------------
//
// whitespace and comment separators


const dquote     = charOrd(34);
const squote     = charOrd(39);
const bslash     = charOrd(92);

const ws         = oneOf(' \t\n');
const blank      = oneOf(' \t');
const newline    = char('\n');

const lineCmtJS  = lineCmtP('//');
const lineCmtHS  = lineCmtP('--');

const multiCmtJS = failure("multiCmtJS parser not yet implemented");
const multiCmtHS = failure("multiCmtHS parser not yet implemented");

const noCmt      = failure("no comment");

let   lineCmt    = lineCmtJS;
let   multiCmt   = multiCmtJS;

// whitespace parsing for line oriented parsers
function lineSep1(cmt) {
    return alts(some(del(alts(blank, cmt))),
                followedBy(newline),
                eof
               );
};

function textSep1(lcmt, tcmt) {
    return alts(some(del(alts(ws,
                              newline,
                              lcmt,
                              tcmt
                             )
                        )
                    ),
                eof
               );
};

// String -> Parser ()
function lineCmtP(w) {
    return seq(word(w), manyT(noneOf('\n')));
};

const lineSep   = lineSep1(noCmt);
const lineSepJS = lineSep1(lineCmtJS);
const lineSepHS = lineSep1(lineCmtHS);

const textSep   = textSep1(noCmt, noCmt);
const textSepJS = textSep1(lineCmtJS, multiCmtJS);


// --------------------
//
// token parsers

// Parser String

// whitespace parsers

const ws0        = manyT(ws);
const ws1        = someT(ws);

const someBlanks = someT(blank);
const manyBlanks = manyT(blank);

// token parsers discard trailing whitespace
// and stop backtracking, due to successfully parse a token
//
// tokenBL discard whitespace only on current line
// tokenWS discards whitespace and newlines

// Parser a -> Parser a

function tokenL  (p) { return cxR(p, lineSep);   };  // space and tabs
function tokenLJS(p) { return cxR(p, lineSepJS); };  // space, tabs, //...
function tokenT  (p) { return cxR(p, textSep);   };    // space, tab, nl
function tokenC  (p) { return cxR(p, pCut);      };

// if token parser succeds,
// backtracking is disabled

let token = (p) => { return tokenC(tokenL(p)); };

// the seq of words does not matter with this parser
// even if a word w1 is prefix of a word w2, and
// w2 occurs behind w1 the parser works

function wordTokens(ws) {
    const ps = map(wordToken)(ws);
    return alts(...ps);
}

// String -> Parser ()
function wordToken(w) {
    return token(word(w));
}

function wordToken0(w) {
    return del(wordToken(w));
}


// regex parsers

const ident      = seqT(satisfy(isAlpha), manyT(satisfy(isAlphaNum)));
const digit      = oneOf("0123456789");
const manyDigits = manyT(digit);
const someDigits = someT(digit);
const sign       = oneOf('-+');
const optSign    = opt("", sign);
const intS       = seqT(sign, someDigits);
const intOS      = seqT(optSign, someDigits);
const fractN     = seqT(someDigits,
                        opt("", seqT(char('.'), someDigits))
                       );
const fractS     = seqT(sign, fractN);
const fract      = seqT(optSign, fractN);

const textLit    = seqT(dquote,
                        pCut,
                        manyT(alt(noneOfOrd(34, 10),
                                  seqT(bslash, item)
                                 )
                             ),
                        pCut,
                        dquote,
                       );
const pathLit    = someT(seqT(char('/'),
                              pCut,
                              someT(alt(satisfy(isAlphaNum),
                                        oneOf('+-._')
                                       )
                                   )
                             )
                        );

// --------------------
//
// geo and offset Parser String

const geoS       = seq(fractN,              // 12.3x4.5
                       del(char('x')),
                       fractN
                      );


// --------------------
//
// object parsers

// whitespace parsing maybe configured here
// a token parser parses a token, converts the string(s)
// into values
// removes trailing whitespace
// whitespace and comment parsing can be configured
// by the token parser variable

// geo parser
const pNat    = fmap(toNum, someDigits);
const pInt    = fmap(toNum, intOS);
const pIntS   = fmap(toNum, intS);
const pFractN = fmap(toNum, fractN);
const pGeo    = app(toV2, fractN, del(char('x')), fractN);  // 12.3x4.5
const pOff    = app(toV2, fractS, fractS);                  // +1.5-2.0

const geoSy   = token(pGeo);
const offSy   = token(pOff);
const scaleSy = opt(V2(1,1), alt(geoSy, fmap(V2, token(pFractN))));
const shiftSy = opt(V2(0,0), offSy);
const goSy    = token(app(GO, pGeo, opt(V2(0,0),pOff)));

// duration parser
const durSy   = fmap(toNum, token(cxR(fractN, char('s'))));

// keyword and name parser
const identSy   = withErr(token(ident),
                          "identifier expected"
                         );

const algSy     = opt(resizeDefault, wordTokens(resizeWords));
const dirSy     = opt(dirDefault,    wordTokens(dirWords));
const transSy   = opt(trDefault,     wordTokens(trWords));
const statusSy  = withErr(wordTokens(statiWords),
                          "status expected"
                         );
const typeSy    = withErr(wordTokens(tyWords),
                          "slide type expected"
                         );
const textSy    = withErr(fmap((xs) => {return JSON.parse(xs);},
                               token(textLit)
                              ),
                          "text literal expected"
                         );
const pathSy    = withErr(token(pathLit),
                          "path expected"
                         );
const jobSy     = withErr(alt(opt(-1, pIntS),
                              identSy
                             ),
                          "rel. job num or job name expected"
                         );

// compound parsers
const geoSpec        = app(GS, algSy, scaleSy, dirSy, shiftSy);

const instrInit      = app(mkInit,  wordToken0(opInit),  identSy);
const instrType      = app(mkType,  wordToken0(opType),  typeSy);
const instrFrame     = app(mkFrame, wordToken0(opFrame), geoSpec);
const instrText      = app(mkText,  wordToken0(opText),  textSy);
const instrPath      = app(mkPath,  wordToken0(opPath),  pathSy);

const instrLoadpage  = fmap(cnst(mkLoadpage()),  wordToken0(opLoadpage));
const instrLoadmedia = fmap(cnst(mkLoadmedia()), wordToken0(opLoadmedia));
const instrWaitInput = fmap(cnst(mkWaitInput()), wordToken0(opWaitInput));
const instrFinish    = fmap(cnst(mkFinish()),    wordToken0(opFinish));

const instrRender    = app(mkRender,  wordToken0(opRender), geoSpec);
const instrPlace     = app(mkPlace,   wordToken0(opPlace),  geoSpec);
const instrMove      = app(mkMove,    wordToken0(opMove),   durSy, geoSpec);

const instrFadein    = app(mkFadein,  wordToken0(opFadein),  durSy, transSy);
const instrFadeout   = app(mkFadeout, wordToken0(opFadeout), durSy, transSy);

const instrWait      = app(mkWait,    wordToken0(opWait), statusSy, jobSy);
const instrDelay     = app(mkDelay,   wordToken0(opDelay),  durSy);
const instrStatus    = app(mkStatus,  wordToken0(opStatus), statusSy);

const instr0   = alts(instrInit,
                      instrType,
                      instrFrame,
                      instrText,
                      instrPath,
                      instrLoadpage,
                      instrLoadmedia,
                      instrRender,
                      instrPlace,
                      instrMove,
                      instrFadein,
                      instrFadeout,
                      instrDelay,
                      instrWaitInput,
                      instrWait,
                      instrStatus,
                      instrFinish,
                      );

const instr1 = cx(manyBlanks,
                  instr0,
                  seq(pCut, newline),
                 );

const code = cxR(many(instr1), eof);

// --------------------
