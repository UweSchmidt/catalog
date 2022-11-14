// ----------------------------------------
//
// Play picture slideshow


// ----------------------------------------
// screen geo

const layout = {
    screenGeo        : zeroV2,
    stageGeo         : zeroV2,
    stageOff         : zeroV2,
    stageIsCentered  : false,
    stageAspectRatio : null,  // e.g. {x: 16, y: 10}, null: no constraint
};

function resizedScreen() {
    const g = V2(window.innerWidth, window.innerHeight);
    trc(1, `resizedScreen: new geo=${showGeo(g)}`);
    layout.screenGeo = g;
    setSizeImgTab();
}

function setAspectRatio(p, c) {
    trc(1, "setAspectRatio " + c);
    layout.stageAspectRatio = p;
    if ( c !== undefined ) {
        layout.stageIsCentered = c;
    }
    setSizeImgTab();
}

function setSizeImgTab() {
    layout.stageGeo = layout.screenGeo;
    layout.stageOff = zeroV2;

    if ( layout.stageAspectRatio != null ) {
        const g1 = roundV2(
            fitIntoGeo(layout.stageAspectRatio, layout.screenGeo));
        layout.stageGeo = g1;

        if ( layout.stageIsCentered ) {
            const o = divV2(subV2(layout.screenGeo, g1), 2);
            layout.stageOff = o;
        }
    }
    setGeoCSS(imgTab, layout.stageGeo, layout.stageOff);
}

function stageGeo()  { return layout.stageGeo; }
function screenGeo() { return layout.screenGeo; }

// ----------------------------------------
//
// id's for DOM elements

const title      = "head-title";
const imgTab     = "imageTab";
const info       = "info";
const infoTab    = "info-table";
const help       = "help";
const panoCss    = "panorama-css";
const statusId   = "status";
const stageId    = imgTab;
const editGeoId  = "edit-geo";
const editTextId = "edit-text";

// statusbar object

const statusBar     = mkStatusBar(statusId);
const editGeoPanel  = mkEditGeoPanel(editGeoId);
const editTextPanel = mkEditTextPanel(editTextId);

// ----------------------------------------

function initHandlers() {
    const imageShowAnims = [
        info,     'info',
        help,     'info',
        statusId, 'info'
        // img1,     'image',
        // img2,     'image'
    ];
    initAnimHandlers(imageShowAnims);
    editGeoPanel.initHandlers();
    editTextPanel.initHandlers();
}

// ----------------------------------------
//
// event handlers

function initPlay() {
    trc(1, "initPlay");
    resizedScreen();
    initHandlers();
    statusBar.show("Slideshow initialized");
}

addEvent(window, 'resize', resizedScreen);
addEvent(window, 'load', initPlay);

// ----------------------------------------
// --------------------
//
// job creation

var newJobNo = 0;

function mkJob(jno,code) {
    return {jno: jno, jcode: code};
}

var j1 =
    cJob('HalloWelt',
         cLoadText1("<div style='width: 100%'><h1>Hallo Welt</h1><p>Hier bin ich!</p></div>",
                    GS('fix', V2(1), 'NW', V2(0.10,0.10)),
                   ),
         cViewStd0(1.0, trFadein,
                   2.0, trCrossfade,
                   [...cView('click'),
                    ...cView(3.0),
                   ],
                  )
        );

var j2 =
    cJob('Hallo',
         cLoadText(rightHalfGeo(),
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
                  defaultFrameGS(),
                  defaultGS(),
                 ),
         cViewStd0(2.0, trCrossfade,
                   2.0, trCrossfade,
                   [...cView('click'),
                    ...cMove(3.0,GS('fill',V2(2),'center',V2())),
                    ...cView(3.0),
                    ...cMove(5.0,GS('fitInto',V2(0.5),'center',V2())),
                    ...cView(3.0),
                    ...cMove(2.0,GS('fix',V2(1),'center',V2())),
                    ...cView(3.0),
                    ...cMove(1.0,GS('fitInto',V2(0.5),'W',V2())),
                    ...cView(3.0),
                    ...cMove(1.0,defaultGS()),
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
                  defaultFrameGS(),
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
                    defaultGS(),
                   ),
         cViewStd(1.0, trFadein, 'click', 5.0, trFadeout)
        );

var jobList = [
    j1, j3, j2,
    j6,
//    j4,
//    j5,
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

// example "main"

function ttt() {
    setAspectRatio(V2(4,3));

    resetVM();
    initVMCode(mkVMProg0(j1,j3,j2,j6));
    startVM();
}


// ----------------------------------------
