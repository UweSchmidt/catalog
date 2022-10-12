// ----------------------------------------

// import Prelude
// import Data.V2
// import Data.Geo
// import DOM.Manipulate
// import DOM.Animation
// import Catalog.Urls
// import Catalog.MetaData

/* ---------------------------------------- */
/* id's */

const title   = "head-title";
const imgTab  = "imageTab";
const img1    = "image1";
const img2    = "image2";
const nextimg = {image1: img2, image2: img1};

// dynamically generated animation css
const panoCss = "panorama-css";

const info     = "info";
const infoTab  = "info-table";
const help     = "help";
const statusId = "status";

var defaultTransitionDur = 1.0;

// default video attributes

const videoAttrs = { controls: "",
                     autoplay: null,
                     muted:    null
                   };
/* some test data */

const g1 = {x:1920,y:1200};
const g2 = {x:6000,y:4000};
const g3 = {x:600, y:800};
const g4 = {x:2000,y:3000};
const g5 = {x:10000,y:1000};
const g6 = {x:10000,y:2000};
/* end test data */

// --------------------

function resizeTo(frameGeo) {
    function resize(dim) {
        function go(g) {
            return dim(g, frameGeo());
        }
        return go;
    }
    return resize;
}

const resizeToImgTab        = resizeTo(imgTabGeo);
const resizeToScreenHeight  = resizeToImgTab(sameHeightGeo);
const resizeToScreenWidth   = resizeToImgTab(sameWidthGeo);
const resizeToFillScreen    = resizeToImgTab(fillGeo);
const resizeToFitIntoScreen = resizeToImgTab(fitIntoGeo);


// add imgTabGeo() as implicit 1. argument

const withGeo = withArg(imgTabGeo);

const placeOnScreen      = withGeo(placeOnFrame);

const showPath           = withGeo(showPath1);
const showImgAlg         = withGeo(showImgAlg1);
const showMovieAlg       = withGeo(showMovieAlg1);
const isTinyImgPage      = withGeo(isTinyImgPage1);

const jsonReqToUrl       = withGeo(jsonReqToUrl1);

// --------------------

const layout = {
    theScreenGeo         : zeroV2,
    theImgTabGeo         : zeroV2,
    theImgTabOff         : zeroV2,
    theImgTabIsCentered  : false,
    theImgTabAspectRatio : null,  // e.g. {x: 16, y: 10}, null: no constraint
    theEditTabIsVisible  : false,
    theEditTabWidth      : 0.0,
};

function resizedScreen() {
    const g = V2(window.innerWidth, window.innerHeight);

    trc(1, `resizedScreen: new geo=${showGeo(g)}`);

    layout.theScreenGeo = g;
    setSizeImgTab();
}

function setAspectRatio(p, c) {
    trc(1, "setAspectRatio " + c);
    layout.theImgTabAspectRatio = p;
    if ( c != undefined ) {
        layout.theImgTabIsCentered = c;
    }
    setSizeImgTab();
}

function setSizeImgTab() {
    const g0 = roundV2(mulV2( layout.theScreenGeo,
                              V2(1 - layout.theEditTabWidth, 1)
                              )
                       );
    layout.theImgTabGeo = g0;
    layout.theImgTabOff = zeroV2;

    if ( layout.theImgTabAspectRatio != null ) {
        const g1 = roundV2(fitIntoGeo(layout.theImgTabAspectRatio, g0));
        layout.theImgTabGeo = g1;

        if ( layout.theImgTabIsCentered ) {
            const o = divV2(subV2(layout.theScreenGeo, g1), 2);
            layout.theImgTabOff = o;
        }
    }

    setGeoCSS(imgTab, layout.theImgTabGeo, layout.theImgTabOff);
}


function imgTabGeo() {
    return layout.theImgTabGeo;
}

/* ---------------------------------------- */
/* urls */

// ----------------------------------------

function getCurrImgElem() {
    return getElem(mkImgId(currImgId()));
}

/* ---------------------------------------- */

function currImgId() {
    return isHiddenAnim(img1, 'image') ? img2 : img1;
}

function nextImgId() {
    return nextimg[currImgId()];
}

function toggleImg12(id)  {
    const trans = getTransition(currPage, lastPage);
    trans(id, nextimg[id], defaultTransitionDur, 'image');
}

// ----------------------------------------
// simplest transition: exchange images without animation

function getTransition(cp, lp) {
    if ( defaultTransitionDur === 0) {
        return cut;
    }
    if ( isImgPage(currPage)
         && lastPage != null
         && isImgPage(lastPage)
       ) {
        return crossFade;
    }
    if ( isColPage(currPage)
         && lastPage != null
         && isColPage(lastPage)
       ) {
        return crossFade;
    }
    return fadeOutIn;
}

// ----------------------------------------

function slowDownTransAnim() {
    defaultTransitionDur += 0.25;
    showAnimDur();
}

function speedUpTransAnim() {
    defaultTransitionDur = Math.max(0, defaultTransitionDur - 0.25);
    showAnimDur();
}

function showAnimDur() {
    const msg = "Animation bei Bildwechsel "
          + (defaultTransitionDur === 0
             ? "aus"
             : defaultTransitionDur + " sec."
            );
    statusBar.show(msg);
}

/* ---------------------------------------- */
/* global state */

var lastPage = null;
var currPage = null;

var picCache = new Image();

/* ---------------------------------------- */
/* initialization */

function initShow() {
    trc(1, "initShow");

    resizedScreen();
    initHandlers();
    showPath(pathCollections());
}

// ----------------------------------------
// display an ordinary image

// --------------------

function loadImgFG1(frameGeo, id, req, geo, resizeAlg) {
    if (resizeAlg === "zoom") {
        loadZoomableImg(frameGeo, id, req, geo);
    }
    else {
        loadAnImg(id, frameGeo, req, geo, resizeAlg);
    }
}

function showImgAlg1(frameGeo, page, resizeAlg) {
    const imgReq = page.imgReq;
    const orgGeo = readGeo(page.oirGeo[0]);  // original geo of image

    var ig = null;
    if ( resizeAlg === "fullsize" ) {
        // scrollable fullsize image in 1:1 resolution
        ig = { ref : oneV2,
               img : orgGeo
             };
    }
    else if ( resizeAlg == "panorama" && isPano(orgGeo) ) {
        // animated panorama
        g1 = fillGeo(orgGeo, frameGeo);
        ig = { ref : g1,
               img : g1
             };
    }
    else if ( resizeAlg === "zoom" ) {
        // zoomable image
        ig = { ref : oneV2,
               img : fitIntoGeo(orgGeo, frameGeo)
             };
    }
    else {
        ig = { ref : bestFitToGeo(frameGeo, orgGeo),
               img : fitIntoGeo(orgGeo, frameGeo)
             };
    };

    const imgUrl = imgReqToUrl(imgReq, ig.ref);

    trc(1, "showImgAlg: imgUrl=" + imgUrl + ", geo=" + showGeo(ig.img));

    // picCache: global image cache
    picCache.onload = () => {
        const id = nextImgId();
        loadImgFG1(frameGeo, id, imgUrl, ig.img, resizeAlg);
        toggleImg12(id);
    };
    picCache.src = imgUrl; // the .onload handler is triggered here
}

function showImg(page)          { showImgAlg(page, "no-magnify");
                                  // showZoomImg(page);
                                }
function showMagnifiedImg(page) { showImgAlg(page, "magnify");    }
function showFullSizeImg(page)  { showImgAlg(page, "fullsize");   }
function showPanoramaImg(page)  { showImgAlg(page, "panorama");   }
function showZoomImg(page)      { showImgAlg(page, "zoom");       }

// ----------------------------------------

function showMovieAlg1(frameGeo, page, resizeAlg) {
    const id     = nextImgId();
    loadMovie(id, frameGeo, page, resizeAlg);
    toggleImg12(id);
}

function showBlog1(frameGeo, page) {
    const id  = nextImgId();
    loadBlog(id, frameGeo, page);
    toggleImg12(id);
}

function showCol1(frameGeo, page) {
    const id = nextImgId();
    loadCollection(id, frameGeo, page);   // in Show.Load
    toggleImg12(id);
}

// ----------------------------------------

function showImg(page)          { showImgAlg(page, "no-magnify");
                                  // showZoomImg(page);
                                }
function showMagnifiedImg(page) { showImgAlg(page, "magnify");    }
function showFullSizeImg(page)  { showImgAlg(page, "fullsize");   }
function showPanoramaImg(page)  { showImgAlg(page, "panorama");   }
function showZoomImg(page)      { showImgAlg(page, "zoom");       }

const    showBlog                = withGeo(showBlog1);
const    showCol                 = withGeo(showCol1);

function showMovie(page)          { showMovieAlg(page, "no-magnify"); }
function showMagnifiedMovie(page) { showMovieAlg(page,    "magnify"); }


function showPage(page) {
    trc(1, "showPage:" + page);

    // store page as new currPage
    lastPage = currPage;
    currPage = page;

    buildInfo();
    setPageTitle();

    const rty = getPageType(page);

    if (rty == "json") {
        showCol(page);
    }
    else if (rty === "movie" || rty === "gif") {
        showMovie(page);
    }
    else if (rty === "page") {
        showBlog(page);
    }
    else if ( ["img", "imgfx", "icon", "iconp"].includes(rty) ) {
        showImg(page);
    }
    else {
        trc(1, "showPage: illegal rType " + rty);
    }

}

function showPath1(frameGeo, path) {
    const jUrl = mkJsonUrl(path, showGeo(bestFitToGeo(frameGeo)));
    gotoUrl(jUrl);
}

function gotoUrl(url) {
    getJsonPage(url, showPage, showErr);
}

function showNextPage(req) {
    if (! nullReq(req)) {
        gotoUrl(jsonReqToUrl(req));
        return true;
    } else {
        trc(1, "showNextPage: req is empty");
        return false;
    }
}

function showErr(errno, url, msg) {
    const txt = showErrText(errno, url, msg);
    statusBar.show('<span class="errormsg">' + txt + '</span>', 4);
}

// ----------------------------------------
// predicates for currPage

function isPanoImgPage() {               // a panoaram image is larger
    if ( isImgPage(currPage) ) {         // tha the screen and has an
        const req = currPage.imgReq;     // aspect ratio >= 2
        if ( isPicReq(req) ) {
            return isPano(readGeo(currPage.oirGeo[0]));
            }
        }
    return false;
}

function isTinyImgPage1(frameGeo) {
    if ( isImgPage(currPage) ) {
        const req = currPage.imgReq;
        if ( isPicReq(req) || isMovieReq(req) ) {
            const orgGeo = readGeo(currPage.oirGeo[0]);
            return lessThanV2(orgGeo, frameGeo);
        }
    }
    return false;
}

// ----------------------------------------
// predicates for current visible part of DOM

function isPic() {
    const e = getCurrImgElem();
    return e && e.classList.contains("img");
}

function isMovie() {
    const e = getCurrImgElem();
    return e && e.classList.contains("movie");
}

function isFullImg() {
    const e = getCurrImgElem();
    return e && e.classList.contains("fullsize");
}

function isMagnifiedImg() {
    const e = getCurrImgElem();
    return e && e.classList.contains("magnify");
}

function isPanoImg() {
    const e = getCurrImgElem();
    return e && e.classList.contains("panorama");
}

// ----------------------------------------
// event handler

function gotoPrev() {
    const req = getNavReq("prev", currPage);
    return showNextPage(req);
}

function gotoNext() {
    const req = getNavReq("next", currPage);
    return showNextPage(req);
}

function gotoPar() {
    const req = getNavReq("par", currPage);
    return showNextPage(req);
}

// for slideshows
function goForward() {
    const req = getNavReq("fwrd", currPage);
    return showNextPage(req);
}

function gotoChild(i, page) {
    page = page || currPage;
    if (isColPage(page)) {
        const c = page.contIcons[i];
        if (c) {
            return showNextPage(c.eReq);
        }
    }
    return false;
}

// reload
function stayHere() {
    const req = getPageReq(currPage);
    return showNextPage(req);
}

// call catalog edit

function openEdit() {
    const page = currPage;
    const req  = getPageReq(currPage);
    const pPos = req.rPathPos;
    openEditPage(pPos[0], pPos[1]);
}

// ----------------------------------------

function buildInfo() {
    const md = getPageMeta(currPage);
    const it = getElem(infoTab);
    buildMetaInfo(it, md);
}

function showInfo() {
    hideHelp();
    showAnim(info, 'info');
}

function hideInfo() {
    hideAnim(info, 'info');
}

function toggleInfo() {
    if (isHiddenAnim(info, 'info')) {
        showInfo();
    } else {
        hideInfo(info);
    }
}

function showHelp() {
    hideInfo();
    showAnim(help, 'info');
}

function hideHelp() {
    hideAnim(help, 'info');
}

function toggleHelp() {
    if (isHiddenAnim(help, 'info')) {
        showHelp();
    } else {
        hideHelp(help);
    }
}

function toggleFullImg() {
    if ( isPic() ) {
        if ( isFullImg() ) {
            showImg(currPage);            // reset to normal size
        } else {
            showFullSizeImg(currPage);    // show in full resolution
        }
    }
}

function toggleMagnifiedImg() {
    if ( isPic() || isMovie() ) {
        if ( isTinyImgPage() ) {
            if ( isMagnifiedImg() ) {
                showPage(currPage);
            } else {
                if ( isPic() ) {
                    showMagnifiedImg(currPage);
                } else {
                    showMagnifiedMovie(currPage);
                }
            }
        }
    }
}

function togglePanoAnimation() {
    trc(1, "togglePanoAnimation fired");
    if (isPanoImg()) {
        const i = getCurrImgElem();
        const s = i.style.animationPlayState;
        setCSS(i, { animationPlayState: (s === "running" ? "paused" : "running")
                  });
    }
}

function restartPanoAnimation() {
    trc(1, "restartPanoAnimation fired");
    if (isPanoImg()) {
        const i = getCurrImgElem();
        i.style.animationPlayState = null;
        i.classList.remove("panorama");
        setTimeout(() => {
            i.classList.add("panorama");
            togglePanoAnimation();
        }, 1000);
    }
}

function togglePanoImg() {
    if (isPanoImgPage()) {
        if (isPanoImg()) {
            stayHere();
        } else {
            showPanoramaImg(currPage);
        }
    }
}

// ----------------------------------------
// video play config options

function toggleVideoMutedDefault()    { toggleVideoAttrDefault("muted"); }
function toggleVideoAutoplayDefault() {
    const v = toggleVideoAttrDefault("autoplay");
    const msg = 'Videos werden'
          + (v === null ? ' nicht' : '')
          + 'automatisch gestartet';
    statusBar.show(msg);
}
function toggleVideoControlsDefault() { toggleVideoAttrDefault("controls"); }

function toggleVideoAttrDefault(a) {
    // toggle video attr default
    const v = videoAttrs[a];
    const n = v === null ? '' : null;
    videoAttrs[a] = n;
    trc(1, "video attr default: " + a + "=" + n);
    return n;
}

function toggleVideoMuted()    { toggleVideoAttr("muted"); }
function toggleVideoAutoplay() { toggleVideoAttr("autoplay"); }
function toggleVideoControls() { toggleVideoAttr("controls"); }

function toggleVideoAttr(a) {
    // toggle current video attr
    const e = getCurrImgElem();
    if (e.classList.contains("video")) {
        if (e.hasAttribute(a)) {
            trc(1, "current video attr " + a + "removed");
            e.removeAttribute(a);
        } else {
            trc(1, "current video attr " + a + "set");
            e.setAttribute(a,"");
        }
    }
}

// ----------------------------------------

function setPageTitle() {
    let txt = "";
    if ( isColPage(currPage) ) {
        const cd = currPage.colDescr;
        txt = cd.eMeta["Descr:Title"]
            ||
            baseName(cd.eReq.rPathPos);
    } else {
        txt = baseName(currPage.img[0]);
    }
    const e = getElem(title);
    clearCont(e).appendChild(newText(txt));
}


// ----------------------------------------
// keyboard input

// just for testing

function charToKey(c) {
    return {keyCode: c.charCodeAt(0), which: c.charCodeAt(0)};
}

function isKey(e, c, s) {
    if ((e.keyCode == 0
         ||
         e.keyCode == e.which
        )
        &&
        e.which == c
       ) { return true;}
    return false;
}

function keyCodeToString(e, c) {
    if ((e.keyCode == 0
         ||
         e.keyCode == e.which
        )
        &&
        e.which == c
       ) { return String.fromCharCode(c); }
    return "";
}

function keyUp(e) {
    if (! e)
        e = window.event;

    trc(1, "keyUp: keyCode=" + e.keyCode + " which=" + e.which);

    if ( (e.keyCode == 39)   /* right arrow */
         ||
         (e.keyCode == 34)   /* page down, presenter: right arrow */
       ) {
        stopSlideShow();
        gotoNext();
        return false;
    }
    if ( (e.keyCode == 37)   /* left arrow */
         ||
         (e.keyCode == 33)   /* page up, presenter: left arrow */
         ||
         (e.keyCode == 8)    /* backspace*/
       ) {
        stopSlideShow();
        gotoPrev();
        return false;
    }
    if ( (e.keyCode == 27)     /* escape, presenter: left screen icon */
         ||                    /* presenter: left screen: 116, 27, 116, 27, ... */
         (e.keyCode == 116)    /* F5, presenter: left screen icon */
         ||
         (e.keyCode == 38)     /* up arrow */
       ) {
        stopSlideShow();
        gotoPar();
        return false;
    }
    if ( (e.keyCode == 40)     /* down arrow */
         ||
         (e.keyCode == 190)    /* '.' , presenter right screen icon */
       ) {
        stopSlideShow();
        gotoChild(0);
        return false;
    }
}

function keyPressed (e) {
    if (! e)
        e = window.event;

    trc(1, "keyPressed: KeyCode=" + e.keyCode + " which=" + e.which);

    if ( isKey(e, 32, " ")
       ) {
        stopSlideShow();
        goForward();
        return false;
    }

    if ( isKey(e, 32, " ")
         ||
         isKey(e, 62, ">")
         ||
         isKey(e, 110, "n")
       ) {
        stopSlideShow();
        gotoNext();
        return false;
    }

    if ( isKey(e, 60, "<")
         ||
         isKey(e, 112, "p")
       ) {
        stopSlideShow();
        gotoPrev();
        return false;
    }

    if ( isKey(e, 94, "^")
         ||
         isKey(e, 117, "u")
       ) {
        stopSlideShow();
        gotoPar();
        return false;
    }

    if ( isKey(e, 118, "v")
         ||
         isKey(e, 100, "d")
       ) {
        stopSlideShow();
        gotoChild(0);
        return false;
    }

    if ( isKey(e, 120, "x")
       ) {
        stopSlideShow();
        stayHere();
        return false;
    }

    if ( isKey(e, 115, "s") ) {
        startStopSlideShow("thisColl");
        return false;
    }

    if ( isKey(e,  83, "S") ) {
        startStopSlideShow("allColls");
        return false;
    }

    if ( isKey(e, 116, "t") ) {
        slowDownTransAnim();
        return false;
    }

    if ( isKey(e,  84, "T") ) {
        speedUpTransAnim();
        return false;
    }

    if ( isKey(e, 105, "i") ) {
        toggleInfo();
        return false;
    }

    if ( isKey(e, 43, "+") ) {
        speedUpSlideShow();
        return false;
    }

    if ( isKey(e, 45, "-") ) {
        slowDownSlideShow();
        return false;
    }

    if ( isKey(e, 48, "0") ) {
        resetSpeedSlideShow();
        return false;
    }

    if ( isKey(e, 102, "f") ) {
        stopSlideShow();
        toggleFullImg();
        return false;
    }

    if ( isKey(e, 108, "l") ) {
        stopSlideShow();
        toggleMagnifiedImg();
        return false;
    }

    if ( isKey(e, 97, "a") ) {
        stopSlideShow();
        togglePanoImg();
        return false;
    }

    if ( isKey(e, 113, "q") ) {
        togglePanoAnimation();
        return false;
    }

    if ( isKey(e, 101, "e") ) {
        stopSlideShow();
        openEdit();
        return false;
    }

    if ( isKey(e, 109, "m") ) {
        toggleVideoMuted();
        return false;
    }

    if ( isKey(e, 99, "c") ) {
        toggleVideoControls();
        return false;
    }

    if ( isKey(e, 65, "A") ) {
        toggleVideoAutoplayDefault();
        return false;
    }

    if ( isKey(e, 77, "M") ) {
        toggleVideoMutedDefault();
        return false;
    }

    if ( isKey(e, 67, "C") ) {
        toggleVideoControlsDefault();
        return false;
    }

    s = keyCodeToString(e.keyCode);
    if ( s != "" ) {
        toggleHelp();
        return false;
    }

    return true;
}

// ----------------------------------------

// install keyboard event handlers

// document.onkeypress = keyPressed;
// document.onkeyup    = keyUp;

document.addEventListener("keypress", keyPressed);
document.addEventListener("keyup",    keyUp);
// document.addEventListener("resize",   resizedScreen);

// ----------------------------------------
// status bar

const statusBar = mkStatus(statusId);

// ----------------------------------------

function initHandlers() {
    const imageShowAnims = [
        info,     'info',
        help,     'info',
        statusId, 'info',
        img1,     'image',
        img2,     'image'
    ];
    initAnimHandlers(imageShowAnims);
}

// ----------------------------------------
