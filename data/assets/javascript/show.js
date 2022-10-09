// ----------------------------------------

// import Prelude
// import Data.V2
// import Data.Geo
// import DOM.Manipulate
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
const zoomCss = "image-zoom-css";

const info    = "info";
const infoTab = "info-table";
const help    = "help";
const status  = "status";

var defaultTransitionDur = 1.0;
var defaultZoomDur       = 1.5;

var editMode  = false; // true;

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


// add the current frame geo as 1. arg to a function

function withGeo(frameGeo, f) {
    function go(...args) {
        return f(frameGeo(), ...args);
    }
    return go;
}

const bestFitToScreenGeo = withGeo(imgTabGeo, bestFitToGeo);
const fitToScreenGeo     = withGeo(imgTabGeo, fitToFrameGeo);
const placeOnScreen      = withGeo(imgTabGeo, placeOnScreen1);
const loadPanoramaImg    = withGeo(imgTabGeo, loadPanoramaImg1);
const showBlog           = withGeo(imgTabGeo, showBlog1);
const showCol            = withGeo(imgTabGeo, showCol1);
const isTinyImgPage      = withGeo(imgTabGeo, isTinyImgPage1);

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

function showHideEditTab () {
    if ( layout.theEditTabIsVisible ) {
        setCSS( "editTab",
                { width: toPx(layout.theScreenGeo.x - layout.theImgTabGeo.x),
                  visibility: "visible",
                }
              );
    }
    else {
        setCSS( "editTab", { visibility: "hidden" });
    }
}

function toggleEditTab () {
    if ( ! layout.theEditTabIsVisible ) {
        layout.theEditTabIsVisible = true;
        layout.theEditTabWidth     = 0.2;
        layout.theImgTabIsCentered = false;
    }
    else {
        layout.theEditTabIsVisible = false;
        layout.theEditTabWidth     = 0.0;
        layout.theImgTabIsCentered = true;
    }
    setSizeImgTab();
    showHideEditTab();
    stayHere();
}

function imgTabGeo() {
    return layout.theImgTabGeo;
}

// if geometry g fits into the screen and
// the resize algorithm isn't "magnify"
// do not expand geometry (img stays as small as it is)

function fitToFrameGeo(fameGeo, geo, blowUp) {
    trc(1,`fitToFrameGeo: fg=${showGeo(fameGeo)}, g=${showGeo(geo)}, ${blowUp}`);
    if ( ! fitsIntoV2(geo, fameGeo)
         ||
         (blowUp === "magnify")
       ) {
        return fitIntoGeo(geo, fameGeo);
    }
    return geo;
}

function placeOnScreen1(frameGeo, geo, shift0) {
    const shift   = shift0 || zeroV2;
    const leftTop = scaleV2(subV2(frameGeo, geo), 0.5);
    return addV2(leftTop, shift);
}

const geoOrg = oneV2;

const serverSupportedGeos =
      [ "160x120",
        "160x160",
        "900x600",
        "1280x800",
        "1400x1050",
        "1600x1200",
        "1920x1200",
        "2560x1440"
      ];

function bestFitToGeo (s) {
    for (let v of serverSupportedGeos) {
        const g = readGeo(v);
        if (fitsIntoV2(s, g))
            return g;
    }
    return geoOrg;
}

function bestFitIconGeo(frameGeo) {
    if (frameGeo.x <= 1280)
        return readGeo("120x90");
    if (frameGeo.x <= 1400)
        return readGeo("140x105");

    return readGeo("160x120");
}

/* ---------------------------------------- */
/* urls */

function jsonReqToUrl(req) {
    const req1 = mkRequest('json',
                           req.rPathPos,
                           showGeo(bestFitToScreenGeo()));
    return reqToUrl(req1);
}

// ----------------------------------------

function mkImgId(id) {
    return id + "-img";
}

function newImgElem(id, css, cls) {
    return newElem4("img", id, css, cls);
}

function newMovElem(id, css, cls) {
    return newElem4("video", id, css, cls);
}

function newBlogElem(id, css, cls) {
    return newElem4("div", id, css, cls);
}

function newElem4(elem, id, css, cls) {
    return newElem(elem, mkImgId(id), css, cls);
}

function getCurrImgElem() {
    return getElem(mkImgId(currImgId()));
}

/* ---------------------------------------- */

function currImgId() {
    return isHiddenImage(img1) ? img2 : img1;
}

function nextImgId() {
    return nextimg[currImgId()];
}

function toggleImg12(id)  {
    const trans = getTransition(currPage, lastPage);
    trans(id, nextimg[id]);
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
// simplest transition: exchange images without animation

function cut(id1, id2) {
    trc(1, "cut: " + id1);
    const e1 = getElem(id1);
    const e2 = getElem(id2);

    nextAnimClass(e2, "visible-image", "hidden-image");
    nextAnimClass(e1, "hidden-image", "visible-image");
    clearImageElem(e2);
}

// ----------------------------------------
// crossfade images

function crossFade(id1, id2, dur0) {
    const e1 = getElem(id1);
    const e2 = getElem(id2);
    const dur = dur0 || defaultTransitionDur;
    trc(1, `crossFade: ${id1}, ${dur}sec`);

    setAnimDur(e2, dur);
    nextAnimClass(e2, "visible-image", "fadeout-image")
        || nextAnimClass(e2, "fadein-image", "fadeout-image");

    setAnimDur(e1, dur);
    nextAnimClass(e1, "hidden-image", "fadein-image");
}

// ----------------------------------------
// fadeout fadein

function fadeOutIn(id1, id2, dur0) {
    const e1 = getElem(id1);
    const e2 = getElem(id2);
    const dur = (dur0 || defaultTransitionDur) / 1;
    trc(1, `fadeOutIn: ${id1}, ${dur}sec`);

    setAnimDur(e2, dur);
    nextAnimClass(e2, "visible-image", "fadeout-image")
        || nextAnimClass(e2, "fadein-image", "fadeout-image");

    // setCSS(e1, {opacity: 0});
    setAnimDur(e1, dur, dur);
    nextAnimClass(e1, "hidden-image", "fadein-image");
}

// ----------------------------------------

function setAnimDur(e, dur, delay) {
    const del = delay || 0;
    setCSS(e, {"animation-duration": dur + "s",
               "animation-delay":    del + "s"
              });
}

function clearImageElem(e) {
    clearCont(e);
    setCSS(e, {"animation-duration": null,
               "animation-delay":    null,
              });
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
    showStatus(msg);
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

function loadImg(id, url, geo, resizeAlg) {
    const imgGeo = fitToScreenGeo(geo, resizeAlg);
    const o      = toPx(placeOnScreen(imgGeo));
    const g      = toPx(imgGeo);

    const istyle = { width    : g.x,
                     height   : g.y,
                     left     : o.x,
                     top      : o.y,
                     position : "absolute",
                     overflow : "hidden"
                   };
    // get element, clear contents and set style attributes
    const e = clearBlock(id);
    const i = newImgElem(id, istyle, "img " + resizeAlg);
    i.src   = url;
    e.appendChild(i);
}

// --------------------

var zoomState = { id     : "",
                  idImg  : "",
                  orgGeo : zeroV2,  // original geometry of image
                  curGeo : zeroV2,  // geometry of image displayed on screen
                  curOff : zeroV2,  // left and top coords of displayed image
                  scale  : 1,    // resize factor of image
                  shift  : zeroV2,  // shift org image from center
                                 // {x: 0.2, y: 0.1}:
                                 // center moves 20% of width to the right
                                 // and 10% of height to the bottom
                };

function loadZoomableImg(id, url, geo, scale, shift) {
    initZoomState(id);
    initZoomGeo(geo);
    zoomTo(scale, shift);

    // get element, clear contents and set style attributes
    const e = clearBlock(id);
    const s = zoomCSS();
    const i = newImgElem(id, s, "img zoom");
    i.src   = url;
    e.appendChild(i);
    i.addEventListener("dblclick", setImgCenter);
}

function isZoomImg() {
    const i = getCurrImgElem();
    if ( i == null ) return false;
    return i.classList.contains("zoom");
}

function setImgCenter(e) {
    trc(1, `setImgCenter: shift image
            offsetX: ${e.offsetX}
            offsetY: ${e.offsetY}
           `);

    const sh = xy2shift({x: e.offsetX, y: e.offsetY});
    zoomImg(null, sh);
    // animImgZoom(null, mulV2(sh, -1));
}

function xy2shift(xy) {
    const p1 = divV2(xy, zoomState.scale);               // pos in original pic
    const p2 = subV2(p1, mulV2(zoomState.orgGeo, 0.5)); // offset from center
    const sh = divV2(p2, zoomState.orgGeo);              // offset rel to size
    return mulV2(sh, -1);
}

function initZoomState(id) {
    const g0 = readGeo(currPage.oirGeo[0]);
    zoomState = { id     : id,
                  idImg  : mkImgId(id),
                  orgGeo : g0,
                  curGeo : g0,
                  curOff : placeOnScreen(g0, zeroV2),
                  shift  : zeroV2,
                  scale  : 1,
                };
}

function initZoomGeo(geo) {
    zoomState.curGeo = geo;
    zoomState.curOff = placeOnScreen(geo, zoomState.shift);
    zoomState.scale = geo.x / zoomState.orgGeo.x;
}

function zoomTo(scale, shift) {
    const sc = scale || zoomState.scale;
    const sh = shift || zoomState.shift;

    const g0 = zoomState.orgGeo;
    const g1 = mulV2(g0, sc);

    const s1 = mulV2(mulV2(sh, g0), sc);
    const o1 = placeOnScreen(g1, s1);

    zoomState.curGeo = g1;
    zoomState.curOff = o1;
    zoomState.shift  = sh;
    zoomState.scale  = sc;

    trc(1, "zoomTo: new zoomState=" + JSON.stringify(zoomState));
}

function zoomCSS() {
    const g = toPx(zoomState.curGeo);
    const o = toPx(zoomState.curOff);
    return { width    : g.x,
             height   : g.y,
             left     : o.x,
             top      : o.y,
             position : "absolute",
             overflow : "hidden"
           };
}

function setImgZoomCSS() { setStyles(zoomState.idImg, zoomCSS()); }

function animImgZoom(scale, shift, duration) {

    function zoomCSSa() {
        const g = toPx(zoomState.curGeo);
        const o = toPx(zoomState.curOff);
        return `width: ${g.x}; height: ${g.y}; left: ${o.x}; top: ${o.y}`;
    }

    const s0 = zoomCSSa();
    zoomTo(scale, shift);
    const s1 = zoomCSSa();

    const cn = "zoom-anim";
    const kf = "zoom-move";
    const d  = duration || defaultZoomDur;  // default zoom duration

    const cssKeyFrames = `
          @keyframes ${kf} {
                0% {${s0}}
              100% {${s1}}
          }
          `;
    const cssZoomClass = `
          img.${cn} {
              animation-name:            ${kf};
              animation-duration:        ${d}s;
              animation-delay:           0s;
              animation-direction:       normal;
              animation-iteration-count: 1;
              animation-timing-function: ease-in-out;
              animation-fill-mode:       both;
          }
          `;

    const css = clearCont(zoomCss);
    css.appendChild(newText(cssKeyFrames + cssZoomClass));

    function animationEndFunction(ev) {
        trc(1, `${ev.animationName} animation has finished`);

        ev.stopPropagation();
        const i = getCurrImgElem();
        i.classList.remove(cn);
        i.removeEventListener("animationend", animationEndFunction);

        setImgZoomCSS();
    }

    trc(1, `animZoomImg: animation started: ${cn}`);

    const i = getCurrImgElem();
    i.addEventListener("animationend", animationEndFunction);
    i.classList.add(cn);    // start anim
}

function animImgZoom1() { animImgZoom(1, zeroV2); }
function animImgPlus()  { animImgZoom(zoomState.scale * 1.2); }
function animImgMinus() { animImgZoom(zoomState.scale / 1.2); }
function animImgZoomFit() {
    var g0 = zoomState.orgGeo;
    var g1 = resizeToFitIntoScreen(g0);
    animImgZoom(g1.x / g0.x, zeroV2);
}
function animImgZoomFill() {
    var g0 = zoomState.orgGeo;
    var g1 = resizeToFillScreen(g0);
    animImgZoom(g1.x / g0.x, zeroV2);
}

// --------------------

function zoomImg(scale, shift) {
    zoomTo(scale, shift);
    setImgZoomCSS();
}

function zoomImg1()     { zoomImg(1); }
function zoomImgPlus()  { zoomImg(zoomState.scale * 1.2); }
function zoomImgMinus() { zoomImg(zoomState.scale / 1.2); }

function zoomImgFit() {
    var g0 = zoomState.orgGeo;
    var g1 = resizeToFitIntoScreen(g0);
    zoomImg(g1.x / g0.x, zeroV2);
}

function zoomImgFill() {
    var g0 = zoomState.orgGeo;
    var g1 = resizeToFillScreen(g0);
    zoomImg(g1.x / g0.x, zeroV2);
}

// --------------------

function editModeOn() {
    if ( ! editMode ) {
        editMode = true;
        trc(1, "editModeOn");
    }
}

function editModeOff() {
    if ( editMode ) {
        editMode = false;
        trc(1, "editModeOff");
    }
}

function editKeyPressed(e) {
    const handled = handleKeyPressed(e);
    if ( handled ) {
        e.stopPropagation();
    }
    else {
        keyPressed(e);
    }
}

function handleKeyPressed(e) {
    if ( ! editMode ) {
        return false;
    }
    if ( ! isZoomImg() ) {
        return false;
    }
    const k = e.key;
    trc(1,`handleKeyPressed: ${k}`);

    // img resize

    if ( k === "+" || k === "*" ) {
        blowupImg(k === "*");
        return true;
    }
    if ( k === "-" || k === "_") {
        shrinkImg(k === "_");
        return true;
    }

    if ( k === "0") {
        zoomImgFit();
        return true;
    }

    if ( k === "1") {
        zoomImgFill();
        return true;
    }
    if ( k === "2") {
        zoomImg1();
        return true;
    }

    // img move
    if ( k === "l" || k === "L") {
        backwardImg(k == "L");
        return true;
    }
    if ( k === "r" || k === "R") {
        forwardImg(k == "R");
        return true;
    }
    if ( k === "u" || k === "U") {
        upwardImg(k == "U");
        return true;
    }
    if ( k === "d" || k === "D") {
        downwardImg(k == "D");
        return true;
    }

    return false;
}

function zmstep(smallStep) { return smallStep ? 1.01 : 1.1; }

function blowupImg(smallStep) {
    const s = zmstep(smallStep);
    zoomImg(zoomState.scale * s);
}
function shrinkImg(smallStep) {
    const s = zmstep(smallStep);
    zoomImg(zoomState.scale / s);
}

function mvstep(smallStep) { return smallStep ? 0.005 : 0.05; }

function forwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(s, 0)));
}
function backwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(0 - s, 0)));
}
function upwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(0, 0 - s)));
}
function downwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(0, s)));
}

// --------------------

function loadFullImg(id, url, imgGeo) {
    const off    = toPx(zeroV2);
    const g      = toPx(imgGeo);

    const istyle = { width    : g.x,
                     height   : g.y,
                     left     : off.x,
                     top      : off.y,
                     position : "absolute",
                     overflow : "auto"     // !!! image becomes scrollable
                   };
    // get element, clear contents and set style attributes
    const e = clearBlock(id);
    const i = newImgElem(id, istyle, "img fullsize");
    i.src   = url;
    e.appendChild(i);
}

function loadPanoramaImg1(frameGeo, id, url, imgGeo) {

    const isH    = isHorizontal(imgGeo);
    const ar     = aspectRatioV2(imgGeo);
    const offset = isH ? frameGeo.x - imgGeo.x : frameGeo.y - imgGeo.y;
    const d = 2.5 * 7.0 * Math.max(ar, 1/ar);

    // add keyframe style
    const s = clearCont(panoCss);

    const kf  = "pano-move";
    const dr  = isH ? "left" : "bottom";
    const dr1 = isH ? "top"  : "left";

    // css for animated panoramas
    const cssKeyFrames = `
          @keyframes ${kf} {
                0% {${dr}: 0px}
               45% {${dr}: ${offset}px}
               55% {${dr}: ${offset}px}
              100% {${dr}: 0px}
          }
          `;

    const cssPanoClass = `
          img.panorama {
              position: absolute;
              ${dr}:     0px;
              ${dr1}:    0px;
              animation-name:            ${kf};
              animation-duration:        ${d}s;
              animation-delay:           2s;
              animation-iteration-count: 1;
              animation-timing-function: ease-in-out;
              animation-play-state:      paused;
          }
          `;

    s.appendChild(newText(cssKeyFrames + cssPanoClass));

    function animationEndFunction() {
        trc(1, "animation of panorama has finished");
    }

    const e = clearBlock(id);
    const i = newImgElem(id, {}, "img panorama");
    i.addEventListener("load", togglePanoAnimation);
    i.addEventListener("animationend", animationEndFunction);
    i.src = url;
    e.appendChild(i);
}

function loadImg1(id, req, geo, resizeAlg) {
    if (resizeAlg === "fullsize") {
        loadFullImg(id, req, geo);
    }
    else if (resizeAlg === "panorama") {
        loadPanoramaImg(id, req, geo);
    }
    else if (resizeAlg === "zoom") {
        loadZoomableImg(id, req, geo);
    }
    else {
        loadImg(id, req, geo, resizeAlg);
    }
}

function showImg1(page, resizeAlg) {
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
        g1 = resizeToFillScreen(orgGeo);
        ig = { ref : g1,
               img : g1
             };
    }
    else if ( resizeAlg === "zoom" ) {
        // zoomable image
        ig = { ref : oneV2,
               img : resizeToFitIntoScreen(orgGeo)
             };
    }
    else {
        ig = { ref : bestFitToScreenGeo(orgGeo),
               img : resizeToFitIntoScreen(orgGeo)
             };
    };

    const imgUrl = imgReqToUrl(imgReq, ig.ref);

    trc(1, "showImg: imgUrl=" + imgUrl + ", geo=" + showGeo(ig.img));

    // picCache: global image cache
    picCache.onload = () => {
        const id = nextImgId();
        loadImg1(id, imgUrl, ig.img, resizeAlg);
        toggleImg12(id);
    };
    picCache.src = imgUrl; // the .onload handler is triggered here
}

function showImg(page)          { showImg1(page, "no-magnify");
                                  // showZoomImg(page);
                                }
function showMagnifiedImg(page) { showImg1(page, "magnify");    }
function showFullSizeImg(page)  { showImg1(page, "fullsize");   }
function showPanoramaImg(page)  { showImg1(page, "panorama");   }
function showZoomImg(page)      { showImg1(page, "zoom");       }

// ----------------------------------------

function loadMovie(id, url, geo, rType, resizeAlg) {
    const movGeo  = fitToScreenGeo(geo, resizeAlg);
    const off     = toPx(placeOnScreen(movGeo));
    const g       = toPx(movGeo);
    const istyle  = { width    : g.x,
                      height   : g.y,
                      left     : off.x,
                      top      : off.y,
                      position : "absolute",
                      overflow : "hidden"
                   };

    const e = clearBlock(id);

    // build video/img element

    if (rType === "movie") {
        const v    = newMovElem(id, istyle, "movie video " + resizeAlg);
        v.width    = movGeo.x;
        v.height   = movGeo.y;
        if (videoAttrs.autoplay != null) {
            v.autoplay = "autoplay";
        }
        if (videoAttrs.controls != null) {
            v.controls = "controls";
        }
        if (videoAttrs.muted != null) {
            v.muted = "muted";
        }

        const s = newElem("source");
        s.src  = url;
        s.type = "video/mp4";

        const w = newElem("span")
                  .appendChild(
                      newText("your browser does not support HTML5 video"));
        v.appendChild(s).appendChild(w);

        // insert new content into element
        e.appendChild(v);

    }
    else if (rType === "gif") {
        const v2 = newImgElem(id,
                              { width:  g.x,
                                height: g.y
                              },
                              "movie gif " + resizeAlg
                             );
        v2.src   = url;

        e.appendChild(v2);
    }
}

function showMovie1(page, resizeAlg) {
    const movReq = page.imgReq;
    const movGeo = readGeo(page.oirGeo[0]);
    const movUrl = toMediaUrl(page.img);
    const id     = nextImgId();

    trc(1, "showMovie: url=" + movUrl + ", geo=" + showGeo(movGeo));

    loadMovie(id, movUrl, movGeo, movReq.rType, resizeAlg);
    toggleImg12(id);
}

function showMovie(page)          { showMovie1(page, "no-magnify"); }
function showMagnifiedMovie(page) { showMovie1(page,    "magnify"); }

// ----------------------------------------

function showBlog1(frameGeo, page) {
    const req = page.imgReq;
    const geo = toPx(frameGeo);
    const txt = getPageBlog(page);
    const id  = nextImgId();

    trc(1, "showBlog1: " + txt);

    // get element, clear contents and set style attributes
    const e  = clearBlock(id);

    // build blog contents div
    const b  = newBlogElem(id,
                           { height:   "100%",
                             overflow: "auto"
                           },
                           "blog"
                          );
    b.innerHTML = txt;
    e.appendChild(b);
    toggleImg12(id);
}

// ----------------------------------------

function showCol1(frameGeo, page) {
    const colDescr = page.colDescr;
    const colReq   = colDescr.eReq;
    const colMeta  = colDescr.eMeta;
    const colBlog  = getPageBlog(page);
    const navIcons = page.navIcons;
    const c1Icon   = page.c1Icon;
    const colIcons = page.contIcons;
    const iconReq  = { rType: "icon",
                       rPathPos: colReq.rPathPos
                     };
    const g        = toPx(frameGeo);
    const id       = nextImgId();
    const e = clearCont(id);
    setCSS(e, { width:    g.x,
                height:   g.y,
                left:     "0px",
                top:      "0px",
                overflow: "auto"
              });
    e.appendChild(buildCollection(frameGeo,
                                  colReq, iconReq,
                                  colMeta,
                                  navIcons, c1Icon, colIcons, colBlog));

    toggleImg12(id);
}

function buildCollection(frameGeo,
                         colReq, iconReq,
                         colMeta,
                         navIcons, c1Icon, colIcons, colBlog) {
    // geometry for navigation grid
    const gap      = 4;
    const padding  = 10;
    const border   = 1;
    const border2  = 2 * border;
    const gridGeo  = V2(3, 2);

    // geometry for icons with/without border
    // in full and half size

    const iconGeo  = bestFitIconGeo(frameGeo); // the geometry of nav icons
    const reqGeo   = bestFitToGeo(iconGeo);  // the geometry of the requested icons (maybe larger than iconGeo)

    // icon geo with border
    const iconGeoB = addV2(iconGeo, border2);

    const i2h      = div(iconGeo.y - border2 - gap, 2);
    const i2w      = div(3 * i2h, 2);
    const ico2Geo  = V2(i2w, i2h);
    const ico2GeoB = addV2(ico2Geo,border2);

    const gapGeo   = mulV2(subV2(gridGeo, 1), gap);
    const navGeo   = addV2(mulV2(ico2GeoB, gridGeo), gapGeo);

    const numCols = Math.max(div(frameGeo.x - 2 * padding, iconGeoB.x + gap));

    // geometry in px
    const iG       = toPx(iconGeo);
    const iGB      = toPx(iconGeoB);
    const i2G      = toPx(ico2Geo);
    const i2GB     = toPx(ico2GeoB);

    const cssBorderColor = "#444";
    const cssBorder      = border + "px solid " + cssBorderColor;

    trc(1, "iconGeo="  + showGeo(iconGeo));
    trc(1, "ico2Geo="  + showGeo(ico2Geo));
    trc(1, "iconGeoB=" + showGeo(iconGeoB));
    trc(1, "ico2GeoB=" + showGeo(ico2GeoB));
    trc(1, "navGeo="   + showGeo(navGeo));
    trc(1, "reqGeo="   + showGeo(reqGeo));
    trc(1, "numCols="  + numCols);

    function buildIcon(req) {
        const r = newElem("img",
                          { width:  "100%",
                            height: "100%"
                          });
        r.src = imgReqToUrl(iconReq, reqGeo);
        return r;
    }

    function toIconUrl(req) {
        const ireq = { rType: "icon", rPathPos: req.rPathPos};
        return imgReqToUrl(ireq, reqGeo);
    }

    function addBild(req, txt) {
        return txt.endsWith(" ")
            ? ( txt +
                ( isColReq(req)
                  ? "Album"
                  : "Bild"
                )
              )
            : txt;
    }

    function buildColHeaderFooter(isHeader) {

        function buildHeadIcon() {
            if ( isHeader ) {
                const r = newElem("div",
                                  { width:  iG.x,
                                    height: iG.y,
                                    border: cssBorder
                                  },
                                  "collection-header-icon");

                const a = newElem("a");
                a.href  = "javascript:openEdit()";
                a.appendChild(buildIcon(iconReq));

                r.appendChild(a);
                return r;
            } else {
                return newElem("div");
            }
        }

        function buildLine(cls, txt) {
            const l = newElem("div", "", {}, cls);
            l.appendChild(newText(txt));
            return l;
        }

        function buildHeadLine() {
            if ( isHeader ) {
                const r  = newElem("div", {}, "collection-header-title");
                const t1 = colMeta["Descr:Title"];
                const t2 = colMeta["Descr:Subtitle"];
                const t3 = colMeta["Descr:Comment"];

                if (t1) {
                    r.appendChild(buildLine("title", t1));
                }
                if (t2) {
                    r.appendChild(buildLine("subtitle", t2));
                }
                if (t3) {
                    r.appendChild(buildLine("comment", t3));
                }
                return r;
            } else {
                return newElem("div");
            }
        }

        function buildNav() {

            function buildNavIcon(ico, tt0) {
                if (! ico) {
                    return newElem("div");
                }

                const req = ico.eReq;
                const md  = ico.eMeta;

                const r = newElem("div",
                                  { width:  i2G.x,
                                    height: i2G.y,
                                    border: cssBorder
                                  },
                                  "navicon"
                                 );
                if (! req) {
                    return r;
                }

                const a = newElem("a");
                a.href  = toHref(jsonReqToUrl(req));
                a.title = md["Descr:Title"]
                       || addBild(req, tt0)
                       || "";

                const i = newElem("img",
                                  { width:  "100%",
                                    height: "100%"
                                  });
                i.src = toIconUrl(req, reqGeo);
                a.appendChild(i);

                r.appendChild(a);
                return r;
            }

            const r = newElem("div",
                              { display:                 "grid",
                                "grid-template-columns": replicate(3, " " + i2GB.x),
                                "grid-template-rows":    replicate(2, " " + i2GB.y),
                                "grid-gap":              gap + "px"
                              });

            const i1 = buildNavIcon();
            const i2 = buildNavIcon(navIcons.par,  "umfassendes Album");
            const i3 = buildNavIcon();
            const i4 = buildNavIcon(navIcons.prev, "voriges ");
            const i5 = buildNavIcon(c1Icon,        "erstes ");
            const i6 = buildNavIcon(navIcons.next, "nächstes ");


            r.appendChild(i1);
            r.appendChild(i2);
            r.appendChild(i3);
            r.appendChild(i4);
            r.appendChild(i5);
            r.appendChild(i6);
            return r;
        }

        const h = newElem("div",
                          { display: "grid",
                            "grid-template-columns": iGB.x + " auto " + navGeo.x + "px",
                            "grid-column-gap": "1em"
                          },
                          isHeader ? "collection-header" : "collection-footer");
        h.appendChild(buildHeadIcon());
        h.appendChild(buildHeadLine());
        h.appendChild(buildNav());
        return h;
    }

    function buildColContents() {
        const r = newElem("div",
                          { display: "grid",
                            "grid-template-columns": replicate(numCols, " " + iGB.x),
                            "grid-auto-rows": iGB.y,
                            "grid-gap":       gap + "px",
                          },
                          "collection-contents");


        for (let i = 0; i < colIcons.length; i++) {
            const ce = colIcons[i];

            // trc(1, "buildColContents: i=" + i + ", val=" + JSON.stringify(ce));

            const ir = ce.eReq;
            const md = ce.eMeta;
            const e  = newElem("div",
                               { width:  iG.x,
                                 height: iG.y,
                                 border: cssBorder
                               });

            const a  = newElem("a");
            a.href   = toHref(jsonReqToUrl(ir));
            a.title  = md["Descr:Title"]
                    || addBild(ir, (i + 1) + ". ");

            const im = newElem("img",
                               { width:  "100%",
                                 height: "100%"
                               });
            im.src = toIconUrl(ir, reqGeo);
            a.appendChild(im);
            e.appendChild(a);
            r.appendChild(e);
        }
        return r;
    }

    function buildColBlog(){
        const r = newElem("div", {}, "collection-blog");
        r.innerHTML = colBlog;
        return r;
    }

    function ruler() {
        const r = newElem("div",
                          { "padding-left":     padding + "px",
                            "padding-right":    padding + "px",
                            "margin-top":       padding + "px",
                            "margin-bottom":    padding + "px",
                            height:             border2 + "px",
                            "background-color": cssBorderColor
                          },
                          "ruler"
                         );
        return r;
    }

    const c = newElem("div",
                      { padding:      padding + "px",
                        "min-height": frameGeo.y + "px"
                      },
                      "collection"
                     );
    c.appendChild(buildColHeaderFooter(true));
    c.appendChild(ruler());
    if (colBlog) {
        c.appendChild(buildColBlog());
        c.appendChild(ruler());
    }
    c.appendChild(buildColContents());
    c.appendChild(ruler());
    c.appendChild(buildColHeaderFooter(false));
    c.appendChild(ruler());
    return c;
}

// ----------------------------------------

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

function showPath(path) {
    const jUrl = mkJsonUrl(path, showGeo(bestFitToScreenGeo()));
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
    showStatus('<span class="errormsg">' + txt + '</span>', 4);
}

// ----------------------------------------
// predicates for currPage

function isColPage(page) {
    page = page || currPage;
    return ! page.imgReq;
}

function isImgPage(page) {
    page = page || currPage;
    return ! isColPage(page);
}

function isPanoImgPage() {               // a panoaram image is larger
    if ( isImgPage() ) {                 // tha the screen and has an
        const req = currPage.imgReq;     // aspect ratio >= 2
        if ( isPicReq(req) ) {
            return isPano(readGeo(currPage.oirGeo[0]));
            }
        }
    return false;
}

function isTinyImgPage1(frameGeo) {
    if ( isImgPage() ) {
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
// page access functions

function getPageType(page) {
    page = page || currPage;
    return getPageReq(page).rType;
}

function getPageReq(page) {
    page = page || currPage;
    return isColPage(page)
        ? page.colDescr.eReq
        : page.imgReq;
}

function getPageMeta(page) {
    page = page || currPage;
    return isColPage(page)
        ? page.colDescr.eMeta
        : page.img[2];
}

function getPageBlog(page) {
    page = page || currPage;
    const md = getPageMeta(page);   // new blog access: blog is part of metadata
    const bt = md["Descr:Blog"];
    if (bt) {
        return bt;
    } // else {return "";}         // TODO cleanup, when server has been updated
    return page.blogCont;      // old blog access: blog is in blogCont field;
}

function getNavReq(nav, page) {
    page = page || currPage;
    return isColPage(page)
        ? page.navIcons[nav].eReq
        : page.imgNavRefs[nav];
}
// ----------------------------------------
// event handler

function gotoPrev() {
    const req = getNavReq("prev");
    return showNextPage(req);
}

function gotoNext() {
    const req = getNavReq("next");
    return showNextPage(req);
}

function gotoPar() {
    const req = getNavReq("par");
    return showNextPage(req);
}

// for slideshows
function goForward() {
    const req = getNavReq("fwrd");
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
    const req = getPageReq();
    return showNextPage(req);
}

// call catalog edit

function openEdit() {
    const page = currPage;
    const req  = getPageReq();
    const pPos = req.rPathPos;
    openEditPage(pPos[0], pPos[1]);
}

function openEditPage(path, pos) {
    var url = "edit-4.5.0.html"
        + "?path=" + path
        + ( pos
            ? "&picno=" + picno
            : ""
          );
    trc(1, "openEditPage: url=" + url);
    window.open(url, "_blank");
}

// ----------------------------------------

function buildInfo() {
    const md = getPageMeta();
    const it = getElem(infoTab);
    buildMetaInfo(it, md);
}

function showInfo() {
    hideHelp();
    showAnimElem(info);
}

function hideInfo() {
    hideAnimElem(info);
}

function toggleInfo() {
    if (isHiddenAnim(info)) {
        showInfo();
    } else {
        hideInfo(info);
    }
}

function showHelp() {
    hideInfo();
    showAnimElem(help);
}

function hideHelp() {
    hideAnimElem(help);
}

function toggleHelp() {
    if (isHiddenAnim(help)) {
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
    showStatus(msg);
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
    if ( isColPage() ) {
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

document.addEventListener("keypress", editKeyPressed);
document.addEventListener("keyup",    keyUp);
// document.addEventListener("resize",   resizedScreen);

// ----------------------------------------
// slideshow stuff

var slideShow      = false;
var slideShowTimer;
var slideShowType  = "";

const slideShowDefaultSpeed = 5000;  // default: 5 sec in milliseconds
var   slideShowSpeed = slideShowDefaultSpeed;  // default: 5 sec in milliseconds

function slideDur() {
    const md = getPageMeta();
    const d  = md["Descr:Duration"];
    let   t  = 1; // seconds
    if (d) {
        t = d * 1;         // convert to number
        if (!t) { t = 1;}  // no conv: reset to defaoult
    }
    return t * slideShowSpeed;
}

function advanceSlideShow() {
    trc(1, "advance SlideShow");
    const hasNext = ( slideShowType == "allColls")
          ? goForward()
          : ( isColPage()
              ? gotoChild(0)
              : gotoNext()
            );
    if (! hasNext) {
        stopSlideShow();
        gotoPar();
    } else {
        const ms = slideDur();
        slideShowTimer = setTimeout(advanceSlideShow, ms);
        trc(1, "advanceSlideShow timer set msec: " + ms + " (" + slideShowType + ")");
    }
}

function stopSlideShow() {
    if (slideShow) {
        if (typeof slideShowTimer != "undefined") {
            clearTimeout(slideShowTimer);
            trc(1, "timer cleared");
        }
        slideShow      = false;
        slideShowType  = "";
        slideShowTimer = undefined;
        showStatus("Automatischer Bildwechsel beendet");
    }
}

function startSlideShow() {
    if (! slideShow) {
        slideShow = true;
        showStatus("Automatischer Bildwechsel gestartet");
        advanceSlideShow();
    }
}

function startStopSlideShow(stype) {
    slideShowType=stype;
    toggleSlideShow();
}

function toggleSlideShow() {
    if (slideShow) {
        stopSlideShow();
    } else {
        startSlideShow();
    }
}

function resetSpeedSlideShow() {
    slideShowSpeed = slideShowDefaultSpeed;
    showDur();
}

function slowDownSlideShow() {
    slideShowSpeed = slideShowSpeed * 1.2;
    showDur();
}

function speedUpSlideShow() {
    slideShowSpeed = Math.max(slideShowSpeed / 1.2, 2500);
    showDur();
}

function showDur() {
    const s =  Math.round(slideShowSpeed / 100) / 10;
    showStatus('Automatischer Bildwechsel nach ' + s + " sec.");
}

// ----------------------------------------
// status line

var statusEnabled = true;
var statusTimer   = undefined;
const statusDur   = 2.0 * 1000;  // default: status messages are shown for 2 seconds

function showStatus(msg, dur) {
    if (statusEnabled) {
        dur = dur || 1;
        dur = statusDur * dur;
        hideStatus();
        const s = getElem(status);
        s.innerHTML = msg;
        statusTimer = setTimeout(hideStatus, dur);
        showAnimElem(status);
    }
}

function hideStatus() {
    if (typeof statusTimer != "undefined") {
        clearTimeout(statusTimer);
    }
    hideAnimElem(status);
}

// ----------------------------------------

function initHandlers() {
    for (let id of [info, help, status]) {
        const e = getElem(id);
        e.addEventListener("animationend",
                           function () {
                               handleInfoAnim(id);
                           });
    }
    for (let id of [img1, img2]) {
        const e = getElem(id);
        e.addEventListener("animationend",
                           function (ev) {
                               handleImageAnim(id, ev);
                            });
    }
}

// ----------------------------------------
// image1 / image2 overlay animation

function handleImageAnim(id, ev) {
    const n = ev.animationName;
    trc(1, `handleImageAnim: ${id} ${n}`);
    if ( n === "kf-fadein-image" || n === "kf-fadeout-image" ) {
        ev.stopPropagation();
        const e = getElem(id);
        nextAnimClass(e, "fadein-image", "visible-image");
        nextAnimClass(e, "fadeout-image", "hidden-image") && clearImageElem(e);
    }
}

function hideImageElem(id) {
    trc(1, "hideImageElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadein-image",  "fadeout-image");
    nextAnimClass(e, "visible-image", "fadeout-image") && clearCont(e);
}

function showImageElem(id) {
    trc(1, "showImageElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadeout-image", "fadein-image");
    nextAnimClass(e, "hidden-image",  "fadein-image");
}

function isHiddenImage(id) {
    // trc(1, 'isHiddenImage: id=' + id);
    const cs = getElem(id).classList;
    return cs.contains("hidden-image") || cs.contains("fadeout-image");
}

// ----------------------------------------
// info, help & status animation

function handleInfoAnim(id) {
    trc(1, "handleInfoanim:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadein-info", "visible-info");
    nextAnimClass(e, "fadeout-info", "hidden-info");
}

function nextAnimClass(e, cur, nxt) {
    const cs = e.classList;
    if (cs.contains(cur)) {
        cs.remove(cur);
        cs.add(nxt);
        trc(1, "nextAnim: cur=" + cur +", nxt=" + nxt + ", cs=" + cs.toString());
        return true;
    }
    return false;
}

function hideAnimElem(id) {
    // trc(1, "showAnimElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadein-info",  "fadeout-info");
    nextAnimClass(e, "visible-info", "fadeout-info");
}

function showAnimElem(id) {
    // trc(1, "toggleAnimElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadeout-info", "fadein-info");
    nextAnimClass(e, "hidden-info",  "fadein-info");
}

function isHiddenAnim(id) {
    // trc(1, 'isHiddenAnim: id=' + id);
    const cs = getElem(id).classList;
    return cs.contains("hidden-info") || cs.contains("fadeout-info");
}

// ----------------------------------------
