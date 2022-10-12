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
    if ( c != undefined ) {
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

const title    = "head-title";
const imgTab   = "imageTab";
const info     = "info";
const infoTab  = "info-table";
const help     = "help";
const panoCss  = "panorama-css";
const statusId  = "status";

// statusbar object

const statusBar = mkStatus(statusId);

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

// ----------------------------------------
