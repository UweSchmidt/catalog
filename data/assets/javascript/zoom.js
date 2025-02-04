/* zoom.js single page slideshow with zoom */

function trc (t, Text) {
    if ( t > 0 ) {
        console.log(Text);
    }
}

/* ---------------------------------------- */
/* id's */

const title   = "head-title";
const imgTab  = "imageTab";
const img1    = "image1";
const img2    = "image2";
const nextimg = {image1: img2, image2: img1};

const info    = "info";
const infoTab = "info-table";
const help    = "help";
const status  = "status";

// default video attributes

const videoAttrs = { controls: "",
                     autoplay: null,
                     muted:    null
                   };

function constF(x) {
    return (y) => { return x; };
}

function idF(x) { trc(1, "idF"); return x; }

function fstF(x, y) { return x; }
function sndF(x, y) { return y; }

/* ---------------------------------------- */
/* basic operations */

function div(x, y) {
    return Math.floor(x / y);
}

function mod(x, y) {
    return Math.floor(x % y);
}

function odd(x) {
    return mod(x, 2) === 1;
}

function even(x) {
    return mod(x, 2) === 0;
}

function replicate(n, s) {
    if (n < 1)
        return "";
    if (n < 2)
        return s;
    if (odd(n))
        return s + replicate(n - 1, s);
    if (even(n)) {
        const s1 = replicate(div(n,2), s);
        return s1 + s1;
    };
}

function qt(s) {
    return '"' + s + '"';
}

function intercalate(s, xs) {
    const l = xs.length;
    if (l === 0) {
        return "";

    } else {
        let res = xs[0];
        for (let i = 1; i < l; i++) {
            res += s + xs[i];
        }
        return res;
    }
}

/* ---------------------------------------- */
/* geometry ops */

const infiniteWidth = 1000000;
const nullGeo = { w : 0, h : 0 };
const oneGeo  = { w : 1, h : 1 };

function eqGeo(g1, g2) {
    return g1.w === g2.w && g1.h === g2.h;
}

function addGeo(g1, g2) {
    if (typeof g2 === "number") {
        return { w : g1.w + g2,   // geo + scalar
                 h : g1.h + g2
               };

    } else {
        return { w : g1.w + g2.w,
                 h : g1.h + g2.h
               };
    }
}

function subGeo(g1, g2) {
    if (typeof g2 === "number") {
        return { w : g1.w - g2,   // geo - scalar
                 h : g1.h - g2
               };

    } else {
        return { w : g1.w - g2.w,
                 h : g1.h - g2.h
               };
    }
}

function mulGeo(g1, g2) {
    if (typeof g2 === "number") {
        return { w : g1.w * g2,   // geo + scalar
                 h : g1.h * g2
               };

    } else {
        return { w : g1.w * g2.w,
                 h : g1.h * g2.h
               };
    }
}

function divGeo(g1, g2) {
    if (typeof g2 === "number") {
        return { w : g1.w / g2,   // geo + scalar
                 h : g1.h / g2
               };

    } else {
        return { w : g1.w / g2.w,
                 h : g1.h / g2.h
               };
    }
}

function halfGeo(g) {
    return divGeo(g,2);
}

function roundGeo(g) {
    return { w : Math.round(g.w),
             h : Math.round(g.h)
           };
}

function pxGeo(g) {
    const g1 = roundGeo(g);
    return { w : g1.w + "px",
             h : g1.h + "px"
           };
}

function styleSize(geo) {
    if ( ! geo )
        return {};

    const gpx = pxGeo(geo);
    return {
        width  : gpx.w,
        height : gpx.h
    };
}

function stylePos(geo) {
    const gpx = pxGeo(geo);
    return {
        left   : gpx.w,
        top    : gpx.h
    };
}

function styleGeo(geo, off, ov) {
    const res = styleSize(geo);
    const opx = pxGeo(off);

    res.left = opx.w;
    res.top  = opx.h;

    res.overflow = ov || "auto";

    return res;
}

function fitsInto(g1, g2) {
    return g1.w <= g2.w && g1.h <= g2.h;
}

function lessThan(g1, g2) {
    return g1.w < g2.w && g1.h < g2.h;
}

function isPano(g) {
    const ar = aspectRatio(g);
    return fitsInto(screenGeo(), g)
        && ( ar >= 2 || ar <= 0.5 );
}

function isHorizontal(g) {
    return aspectRatio(g) > 1;
}

function aspectRatio(g) {
    return g.w / g.h;
}

function readGeo(txt) {
    if (txt === "org")
        return { w : 1, h : 1 };

    const a = txt.split('x');
    return { w : 1 * a[0],
             h : 1 * a[1]
           };
}

function showGeo(geo) {
    if (geo.w === 1 && geo.h === 1)
        return "org";
    return "" + geo.w + "x" + geo.h;
}

function toPx(obj) {
    const res = {};
    for (k in obj) {
        res[k] = obj[k] + "px";
    }
    return res;
}

function shrinkGeo(s, d) {
    if (fitsInto(s,d))
        return s;
    else
        return resizeGeo(s, d);
}

// computes the maximum geo with aspect ratio of s
// that fits into d

function resizeGeo(s, d) {
    if (s.w * d.h >= d.w * s.h)
        return { w : d.w,
                 h : div(d.w * s.h, s.w)
               };
    else
        return { w : div(d.h * s.w, s.h),
                 h : d.h
               };
}

function resize(s, d) {
    const resized = resizeGeo(s, d);
    const scale   = divGeo(resized, s);
    const offset  = halfGeo(subGeo(d, resized));
    const aspectS = s.w / s.h;
    const aspectD = d.w / d.h;


    return {
        org:     s,
        target:  d,
        resized: resized,
        scale:   scale,
        offset:  offset,
        aspectr: aspectS,

        isLandscape:  aspectS >= aspectD,
        isPortrait:   aspectS <= aspectD,
        fits:         eqGeo(resized, d),
    };
}

function resizeWidth(g, w) {
    return resize(g, {w: w, h: infiniteWidth});
}

function resizeHeight(g, h) {
    return resize(g, {w: infiniteWidth, h: h});
}

function resizeToScreen(g) {
    return resize(g, screenGeo());
}

function resizeToScreenH(g) {
    return resizeHeight(g, screenGeo().h);
}

function resizeToScreenW(g) {
    return resizeWidth(g, screenGeo().w);
}


function resizeToHeight(s, d) {
    return resizeGeo(s, {w: infiniteWidth, h: d.h});
}

function resizeToWidth(s, d) {
    return resizeGeo(s, {w: d.w, h: infiniteWidth});
}

function resizeToScreenHeight(g) {
    return resizeToHeight(g, screenGeo());
}

function resizeToScreenHeight1(g) {
    if ( fitsInto(screenGeo(), g) )
        return readGeo("org");
    return resizeToScreenHeight(g);
}

function resizeToScreenHeightPano(g) {
    if ( ! isPano(g) )
        return g;
    return resizeToScreenHeight(g);
}

function resizeToScreenWidth(g) {
    return resizeToWidth(g, screenGeo());
}

var theScreenGeo = { w : 0, h : 0 };

function screenGeo() {
    const w1 = window.innerWidth;
    const h1 = window.innerHeight;

    if ( w1 != theScreenGeo.w || h1 != theScreenGeo.h ) { // screenGeo has changed
        theScreenGeo.w = w1;
        theScreenGeo.h = h1;
        setCSS(imgTab, {width : w1 + "px", height : h1 + "px"});
    }
    return theScreenGeo;
}

function fitToScreenGeo(geo, blowUp) {
    return (blowUp === "magnify" ? resizeGeo : shrinkGeo)(geo, screenGeo());
}

function placeOnScreen(geo) {
    return halfGeo(subGeo(screenGeo(), geo));
}

const geoOrg = readGeo("org");

const serverSupportedGeos =
      [ "160x120",
        "160x160",
        "320x240",
        "900x600",
        "1280x800",
        "1400x1050",
        "1600x1200",
        "1920x1200",
        "2560x1440",
        "3840x2160"    // 4k Eizo monitor
      ];

function bestFitToGeo (s) {
    for (let v of serverSupportedGeos) {
        const g = readGeo(v);
        if (g.w >= s.w && g.h >= s.h)
            return g;
    }
    return geoOrg;
}

function bestFitToScreenGeo () {
    return bestFitToGeo(screenGeo());
}

function bestFitIconGeo() {
    const s = screenGeo();
    if (s.w <= 1280)
        return readGeo("120x90");
    if (s.w <= 1400)               // cannon beamer geo 1400x1050
        return readGeo("140x105");
    if (s.w <= 2560)
        return readGeo("160x120"); // iMac 27''

    return readGeo("256x144");     // eizo 4k display
}

/* ---------------------------------------- */
/* urls */

function toHref(url) {
    return "javascript:gotoUrl('"
        + url
        + "');";
}

function jsonReqToUrl(jsonReq) {
    return jsonReqToUrl1(jsonReq, bestFitToScreenGeo());
}

function jsonReqToUrlFS(jsonReq) {
    return jsonReqToUrl1(jsonReq, readGeo("org"));
}

function jsonReqToUrl1(jsonReq, geo) {
    const pp = rPathPosToUrl(jsonReq.rPathPos);
    return "/docs/json/"
        + showGeo(geo)
        + pp
        + ".json";
}

function imgReqToUrl(imgReq, imgGeo) {
    const pp = rPathPosToUrl(imgReq.rPathPos);
    return "/docs/"
        + imgReq.rType
        + "/"
        + ( imgGeo
            ? showGeo(imgGeo)
            : showGeo(bestFitToScreenGeo())
          )
        + pp
        + ".jpg";
}

function rPathPosToUrl(rPathPos) {
    const pos = rPathPos[1];
    return rPathPos[0] + posToUrl(rPathPos[1]);
}

function posToUrl(pos) {
    if (typeof pos === "number") {
        let txt = "" + pos;
        let len = txt.length;
        let px  = Math.max(0, 4 - txt.length);
        return "/pic-" + "00000000".substr(0,px) + txt;
    }
    return "";
}

function toMediaUrl(img) {
    return dirPath(img[0]) + "/" + img[1];
}

function dirPath(p) {
    const ix = p.lastIndexOf("/");
    return p.substr(0, ix);
}

function baseName(x) {      // basename works with requests, pathPos and strings
    trc(1, "baseName: x=" + JSON.stringify(x));
    if ( typeof x === "string") {
        const ix = x.lastIndexOf("/");
        return x.slice(ix + 1);
    }
    if (x.rPathPos) {
        return baseName(x.rPathPos);
    }
    return baseName(x[0]);
}

function nullReq(req) {
    return (! req);
}

function isColReq(req) {
    const pos = req.rPathPos[1];
    return (typeof pos != "number");
}

function isPicReq(req) {
    return ["img", "imgfx", "icon", "iconp"].includes(req.rType);
}

function isMovieReq(req) {
    return ["movie", "gif"].includes(req.rType);
}

function pathToPathPos(path0) {
    const path  = path0.split("/");
    const t0    = path.pop();
    const t1    = t0.split("-");
    const n     = t1[1] * 1;

    if (t1[0] === "pic" && typeof n === "number") {
        return [intercalate("/", path), n];
    }
    return [path0, null];
}

// path constants
function pathArchive()     { return "/archive"; }
function pathCollections() { return "/archive/collections"; }

/* ---------------------------------------- */

function newText(txt) {
    return document.createTextNode(txt);
}

function newElem0(tag) {
    return document.createElement(tag);
}

function getElem(id) {
    const e = document.getElementById(id);
    if (! e) {
        trc(1, "getElem: warning: no elem found for " + id);
    }
    return e;
}

function clearCont(e) {
    if (typeof e === "string") {
        const e1 = getElem(e);
        if (e1 != null) {
            clearCont(e1);
        }
        return e1;
    } else {
        e.innerHTML = '';
        trc(1, "clearCont:" + JSON.stringify(e));
        return e;
    }
}

function setCSS(e, attrs, val) {
    if (typeof e === "string") {  // e is an id
        setCSS(getElem(e), attrs, val);
    }
    else if (typeof attrs == "string"
             &&
             typeof val === "string"
            ) {
        e.style[attrs] = val;
    }
    else if (typeof e === "object"
             &&
             typeof attrs === "object"
            ) {
        for (let a in attrs) {    // e is an element
            e.style[a] = "" + attrs[a];
        }
        return e;
    }
    else {
        throw ("setCSS: illegal arg types: (" +
               typeof e + "," +
               typeof attrs + "," +
               typeof val + ")");
    }
}

function newElem(tag, x2, x3, x4) {
    let id = "";
    if (typeof x2 === "string") {  // elem id found
        id = x2;
        x2 = x3;
        x3 = x4;
    }
    let css = {};
    if (typeof x2 === "object") {  // style obj found
        css = x2;
        x2  = x3;
    }
    let cls = "";
    if (typeof x2 === "string") {  // css class found
        cls = x2;
    }

    const e = newElem0(tag);
    setCSS(e, css);                // set style
    if (id) {
        e.id = id;                 // set id
    }
    if (cls) {
        const clss = cls.split(" ");
        for (let i = 0; i < clss.length; i++ ) {
            const c = clss[i];
            if (c) {
                e.classList.add(c);
            }
        }
    }
    return e;
}

// --------------------

function setContents(id, cont) {
    document.getElementById(id).innerHTML = cont;
}

function setAttr(id, attr, val) {
    document.getElementById(id)[attr] = val;
}

function setStyles(id, attrs) {
    const e = document.getElementById(id);
    for (let a in attrs) {
        e.style[a] = attrs[a];
    }
}

function setStyle(id, attr, val) {
    document.getElementById(id).style[attr] = val;
}

function getStyle(id, attr) {
    return document.getElementById(id).style[attr];
}

// ----------------------------------------

function mkImgId(id) {
    return id + "-img";
}

function newImgElem(id, css, cls) {
    return newElem("img", mkImgId(id), css, cls);
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

function toggleImg12(id, noTrans)  {
    const trans = getTransition(currPage,
                                lastPage,
                                noTrans ? true : false);
    trans(id, nextimg[id]);
}

// ----------------------------------------
// simplest transition: exchange images without animation

function getTransition(cp, lp, noTrans) {
    if ( noTrans || defaultTransDur === 0) {
        return cut;
    }
    if (isImgPage(currPage)
        && lastPage != null
        && isImgPage(lastPage)
       ) {
        return crossFade;
    }
    if (isColPage(currPage)
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

    nextAnimClass(e2, "visibleImage", "hiddenImage");
    nextAnimClass(e1, "hiddenImage", "visibleImage");
    clearImageElem(e2);
}

// ----------------------------------------
// crossfade images

let defaultTransDur = 1.0;

function crossFade(id1, id2, dur0) {
    const e1 = getElem(id1);
    const e2 = getElem(id2);
    const dur = dur0 || defaultTransDur;
    trc(1, `crossFade: ${id1}, ${dur}sec`);

    setAnimDur(e2, dur);
    nextAnimClass(e2, "visibleImage", "fadeoutImage")
        || nextAnimClass(e2, "fadeinImage", "fadeoutImage");

    setAnimDur(e1, dur);
    nextAnimClass(e1, "hiddenImage", "fadeinImage");
}

// ----------------------------------------
// fadeout fadein

function fadeOutIn(id1, id2, dur0) {
    const e1 = getElem(id1);
    const e2 = getElem(id2);
    const dur = (dur0 || defaultTransDur) / 1;
    trc(1, `fadeOutIn: ${id1}, ${dur}sec`);

    setAnimDur(e2, dur);
    nextAnimClass(e2, "visibleImage", "fadeoutImage")
        || nextAnimClass(e2, "fadeinImage", "fadeoutImage");

    // setCSS(e1, {opacity: 0});
    setAnimDur(e1, dur, dur);
    nextAnimClass(e1, "hiddenImage", "fadeinImage");

}

// ----------------------------------------

function setAnimDur(e, dur, delay) {
    const del = delay || 0;
    setCSS(e, {"animation-duration": dur + "s",
               "animation-delay":    del + "s"
              });
}
/* old stuff

function clearImageElem0(e) {
    // e : <div id="image1/2" ...><img id="image1/2-img" ...></img></div>

    const id = e.id;
    trc(1, "clearImageElem: id=" + id);
    if (id === img1 || id === img2) {

        // cancel animations
        e.getAnimations({ subtree: true }).map((animation) => animation.finished);

        // remove <img> element
        clearCont(e);

        // remove style attributes
        setCSS(e, {"animation-duration": '',
                   "animation-delay":    '',
                   "width":              '',
                   "height":             '',
                   "left":               '',
                   "right":              '',
                   "bottom":             '',
                   "top":                '',
                   "overflow":           '',
                   "opacity":            '',
                  });
    }
}
*/

// <div id="image1/2" ...>...</div> is thrown away and
// and a new but empty elem with same id is recreated

function clearImageElem(e) {
    const id = e.id;
    const p  = e.parentElement;
    e.remove();
    p.insertBefore(newElem("div", id, {}, "hiddenImage"), p.children[0]);
}

// ----------------------------------------

function slowDownTransAnim() {
    defaultTransDur += 0.25;
    showAnimDur();
}

function speedUpTransAnim() {
    defaultTransDur = Math.max(0, defaultTransDur - 0.25);
    showAnimDur();
}

function showAnimDur() {
    const msg = "Animation bei Bildwechsel "
          + (defaultTransDur === 0
             ? "aus"
             : defaultTransDur + " sec."
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
    const g = pxGeo(screenGeo());
    console.log("initShow: screen geo=" + showGeo(g));

    initHandlers();

    setCSS(imgTab, {width : g.w, height : g.h});

    clearImageElem(getElem(img1));
    clearImageElem(getElem(img2));

    showPath(pathCollections());
}

// ----------------------------------------
// display an ordinary image

// old, new: addImgToDom

function insertImg(id, url, style, geo, cls, addHandler) {
    // get element, clear contents and set style attributes

    const e = clearCont(id);
    setCSS(e, style);
    const i = newImgElem(id, styleSize(geo), cls);
    addHandler(i);
    i.src   = url;
    e.appendChild(i);

}

function insertZoomCss(cssId, kf, view1Geo, view1Off, view1Scale, view2Geo, view2Off, view2Scale) {
    const dur        = 2;
    const delay      = 0;

    const zoom1 = `
              transform: matrix(${view1Scale.w}, 0, 0, ${view1Scale.h}, ${view1Off.w}, ${view1Off.h});
    `
    ;

    const zoom2 = `
              transform: matrix(${view2Scale.w}, 0, 0, ${view2Scale.h}, ${view2Off.w}, ${view2Off.h});
    `
    ;

    const cssKeyFrames = `
          @keyframes ${kf} {
              0% { ${zoom1} }
            100% { ${zoom2} }
          }
          `;
    const cssClass = `
          img.zoom-init { ${zoom1} }
          img.zoom-end  { ${zoom2} }

          img.zoom {
              transform-origin: left top;

              animation-name:            ${kf};
              animation-duration:        ${dur}s;
              animation-delay:           ${delay}s;
              animation-iteration-count: 1;
              animation-timing-function: ease-in-out;
              animation-play-state:      paused;
          }
          `;

    trc(1, cssKeyFrames);
    trc(1, cssClass);

    const css = clearCont(cssId);
    css.appendChild(newText(cssKeyFrames + cssClass));
}

function loadImg(id, url, geo, resizeAlg) {
    const imgGeo = fitToScreenGeo(geo, resizeAlg);
    const offset = placeOnScreen(imgGeo);
    const style  = styleGeo(imgGeo, offset, "hidden");

    function initZoom(e) {
        trc(1, "initZoom: start zooming image with id=" + this.id );

        const pos = { w: e.offsetX,
                      h: e.offsetY
                    };
        trc(1, "pos=" + showGeo(pos));
        showZoomImg(currPage, pos);
    }

    function addHandler(i) {
        i.addEventListener("dblclick", initZoom);
    }

    insertImg(id, url, style, geo, "img " + resizeAlg, addHandler);
}

function loadZoomImg(id, url, orgGeo, clickPos) {
    const scrGeo     = screenGeo();

    const viewGeo    = fitToScreenGeo(orgGeo, "no-magnify");
    const viewCenter = halfGeo(viewGeo);
    const viewScale  = divGeo(viewGeo, orgGeo);
    const viewOff    = placeOnScreen(viewGeo);

    const orgOff     = placeOnScreen(orgGeo);
    const orgScale   = oneGeo;

    const clickDisp  = subGeo(viewCenter, clickPos);
    const clickScale = divGeo(orgGeo, viewGeo);
    const clickOff   = addGeo(orgOff, mulGeo(clickDisp, clickScale));

    const style      = styleGeo(scrGeo, nullGeo, "hidden");

    trc(1, `loadZoomImg: ${url}, ${showGeo(orgGeo)}, ${showGeo(clickPos)}`);

    function finishZoom(e) {
        trc(1,"finishZoom: back to normal view");
        showNoTransImg(currPage);
    }

    function animationEndFunction() {
        trc(1, "animation of zoom-move finished");
        const i = getCurrImgElem();
        i.classList.remove("zoom-init");
        i.classList.add("zoom-end");
    }

    function addHandler(i) {
        i.addEventListener("dblclick", finishZoom);
        i.addEventListener("load", toggleZoomAnimation);
        i.addEventListener("animationend", animationEndFunction);
    }

    insertZoomCss("zoom-css", "zoom-move", viewGeo, viewOff, viewScale, orgGeo, clickOff, orgScale);

    insertImg(id, url, style, null, "img zoom zoom-init", addHandler);
}

function loadFullImg(id, url, imgGeo) {
    const scrGeo = screenGeo();
    const style  = styleGeo(scrGeo, nullGeo);

    insertImg(id, url, style, imgGeo, "img fullsize", () => {});
}

function loadPanoramaImg(id, url, imgGeo) {
    const scrGeo = screenGeo();
    const isH    = isHorizontal(imgGeo);
    const ar     = aspectRatio(imgGeo);
    const offset = isH ? scrGeo.w - imgGeo.w : scrGeo.h - imgGeo.h;
    const d = 2.5 * 7.0 * Math.max(ar, 1/ar);

    // add keyframe style
    const s = clearCont("panorama-css");

    const kf  = "pano-move";
    const dr  = isH ? "left" : "bottom";
    const dr1 = isH ? "top"  : "left";
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
              animation-direction:       alternate;    /* redundant due to animation-iteration-count: 1 */
              animation-timing-function: ease-in-out;
              animation-play-state:      paused;
          }
          `;

    s.appendChild(newText(cssKeyFrames + cssPanoClass));

    const style = styleGeo(scrGeo, nullGeo, "hidden");

    function animationEndFunction() {
        trc(1, "animation of panorama finished");
    }

    function addHandler(i) {
        i.addEventListener("load", togglePanoAnimation);
        i.addEventListener("animationend", animationEndFunction);
    }

    insertImg(id, url, style, {}, "img panorama", addHandler);
}

function loadImg1(id, req, geo, resizeAlg, pos) {
    if (resizeAlg === "fullsize") {
        loadFullImg(id, req, geo);
    }
    else if (resizeAlg === "zoom") {
        loadZoomImg(id, req, geo, pos);
    }
    else if (resizeAlg === "panorama") {
        loadPanoramaImg(id, req, geo);
    }
    else {
        loadImg(id, req, geo, resizeAlg);
    }
}

function showImg1(page, resizeAlg, noTrans, zoomPos0) {
    const imgReq = page.imgReq;
    const orgGeo = readGeo(page.oirGeo[0]);  // original geo of image
    const imgGeo = resizeToScreenHeight(orgGeo);
    const scrGeo = screenGeo();
    const zoomPos = zoomPos0 || halfGeo(scrGeo);

    const imgUrl = ( (   resizeAlg === "fullsize"
                      || resizeAlg === "zoom"
                     )
                     && fitsInto(screenGeo(), orgGeo)
                   )
          ? imgReqToUrl(imgReq, readGeo("org"))
          : ( resizeAlg === "panorama" && isPano(imgGeo)
              ? imgReqToUrl(imgReq, resizeToScreenHeight(imgGeo))
              : imgReqToUrl(imgReq)
            );

    trc(1, "showImg: imgUrl=" + imgUrl + ", geo=" + showGeo(imgGeo));

    picCache.onload = () => {
        const id = nextImgId();
        trc(1, `onload loadImg1: ${id}`);
        loadImg1(id, imgUrl,
                 ( resizeAlg === "fullsize"
                   ||
                   resizeAlg === "zoom"
                 )
                 ? orgGeo : imgGeo,
                 resizeAlg,
                 zoomPos);
        toggleImg12(id, noTrans);
    };
    picCache.src = imgUrl; // the .onload handler is triggered here
}

function showImg(page)          { showImg1(page, "no-magnify"); }
function showNoTransImg(page)   { showImg1(page, "no-magnify", true); }
function showMagnifiedImg(page) { showImg1(page, "magnify");    }
function showFullSizeImg(page)  { showImg1(page, "fullsize");   }
function showZoomImg(page, pos) { showImg1(page, "zoom", true, pos);  }
function showPanoramaImg(page)  { showImg1(page, "panorama");   }


// ----------------------------------------

// old, new: buildMovieSlide

function loadMovie(id, url, geo, rType, resizeAlg) {
    const movGeo = fitToScreenGeo(geo, resizeAlg);
    const offset = pxGeo(placeOnScreen(movGeo));
    const g      = pxGeo(movGeo);
    const style  = { width  : g.w,
                     height : g.h,
                     left   : offset.w,
                     top    : offset.h
                   };

    // get element, clear contents and set style attributes
    const e = clearCont(id);
    setCSS(e, style);

    // build video/img element

    if (rType === "movie") {
        const v    = newElem("video", mkImgId(id), {}, "movie video " + resizeAlg);
        v.width    = movGeo.w;
        v.heigth   = movGeo.h;
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
                              { width:  g.w,
                                height: g.h
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

// old, new: buildBlogSlide

function showBlog(page) {
    const req = page.imgReq;
    const geo = pxGeo(screenGeo());
    const txt = getPageBlog(page);
    const id  = nextImgId();

    trc(1, "showBlog: " + txt);

    // get element, clear contents and set style attributes
    const e  = clearCont(id);
    setCSS(e, { width:  geo.w,
                height: geo.h,
                top:    "0px",
                left:   "0px"
              });

    // build blog contents div
    const b  = newElem("div", id + "-blog",
                       { width:    geo.w,
                         height:   geo.h,
                         overflow: "auto"
                       },
                       "blog"
                      );
    b.innerHTML = txt;
    e.appendChild(b);
    toggleImg12(id);
}

// ----------------------------------------

// old: the new fct is buildCollectionPage

function showCol(page) {
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
    const g        = pxGeo(screenGeo());
    const id       = nextImgId();
    const e = clearCont(id);
    setCSS(e, { width:    g.w,
                height:   g.h,
                left:     "0px",
                top:      "0px",
                overflow: "auto"
              });
    e.appendChild(buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons, colBlog));

    toggleImg12(id);
}

// this is used by buildCollectionPage

function buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons, colBlog) {
    // geometry for navigation grid
    const gap      = 4;
    const padding  = 10;
    const border   = 1;
    const border2  = 2 * border;
    const gridGeo  = {w: 3, h: 2};

    // geometry for icons with/without border
    // in full and half size

    const iconGeo  = bestFitIconGeo();       // the geometry of nav icons
    const reqGeo   = bestFitToGeo(iconGeo);  // the geometry of the requested icons (maybe larger than iconGeo)
    const scrGeo   = screenGeo();

    // icon geo with border
    const iconGeoB = addGeo(iconGeo, border2);

    const i2h      = div(iconGeo.h - border2 - gap, 2);
    const i2w      = div(3 * i2h, 2);
    const ico2Geo  = {w: i2w, h: i2h};
    const ico2GeoB = addGeo(ico2Geo,border2);

    const gapGeo   = mulGeo(subGeo(gridGeo, 1), gap);
    const navGeo   = addGeo(mulGeo(ico2GeoB, gridGeo), gapGeo);

    const numCols = Math.max(div(scrGeo.w - 2 * padding, iconGeoB.w + gap));

    // geometry in px
    const iG       = pxGeo(iconGeo);
    const iGB      = pxGeo(iconGeoB);
    const i2G      = pxGeo(ico2Geo);
    const i2GB     = pxGeo(ico2GeoB);

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
                                  { width:  iG.w,
                                    heigth: iG.h,
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

        function buildLineGPS(deg, url) {
            const l = newElem("div", "", {}, "gpsposition");
            // hack, hack, hack: global variable for 2. param
            gpsUrl = url;
            const e = fmtGPS(deg);
            l.appendChild(e);
            return l;
        }

        function buildHeadLine() {
            if ( isHeader ) {
                const r  = newElem("div", {}, "collection-header-title");
                const t1 = colMeta["Descr:Title"];
                const t2 = colMeta["Descr:Subtitle"];
                const t3 = colMeta["Descr:Comment"];
                const t4 = colMeta["Descr:GPSPositionDeg"];
                const t5 = colMeta["Descr:GPSurl"];

                if (t1) {
                    r.appendChild(buildLine("title", t1));
                }
                if (t2) {
                    r.appendChild(buildLine("subtitle", t2));
                }
                if (t3) {
                    r.appendChild(buildLine("comment", t3));
                }
                if (t4) {
                    r.appendChild(buildLineGPS(t4, t5));
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
                                  { width:  i2G.w,
                                    height: i2G.h,
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
                                "grid-template-columns": replicate(3, " " + i2GB.w),
                                "grid-template-rows":    replicate(2, " " + i2GB.h),
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
                            "grid-template-columns": iGB.w + " auto " + navGeo.w + "px",
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
                            "grid-template-columns": replicate(numCols, " " + iGB.w),
                            "grid-auto-rows": iGB.h,
                            "grid-gap":       gap + "px",
                          },
                          "collection-contents");


        for (let i = 0; i < colIcons.length; i++) {
            const ce = colIcons[i];

            // trc(1, "buildColContents: i=" + i + ", val=" + JSON.stringify(ce));

            const ir = ce.eReq;
            const md = ce.eMeta;
            const e  = newElem("div",
                               { width:  iG.w,
                                 height: iG.h,
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
                        "min-height": (scrGeo.h - 2 * padding) + "px"
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
    const rPathPos = pathToPathPos(path);
    const jReq = { rType: "json", rPathPos: rPathPos};
    const jUrl = jsonReqToUrl(jReq);
    gotoUrl(jUrl);
}

function showNextSlide(req) {
    if (! nullReq(req)) {
        gotoUrl(jsonReqToUrl(req));
    }
}

function showErr(errno, url, msg) {
    trc(1, "showErr:" + url + ": " + errno + ", " + msg);

    let txt = "";
    if (errno === 500) {
        txt = 'Server Fehler: ' + msg + ', url=' + url;
    } else if (errno === 404) {
        txt = 'Server Fehler: Kein Eintrag gefunden für ' + url;
    } else {
        txt = 'Server Fehler: ' + errno + ', url=' + url;
    }
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

function isFullsizeImgPage(page) {
    page = page || currPage;
    if ( isImgPage(page) ) {
        if ( isPicReq(page.imgReq) ) {
            return eqGeo(readGeo(page.oirGeo[1]), oneGeo);
        }
    }
    return false;
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

function isTinyImgPage() {
    if ( isImgPage() ) {
        const req = currPage.imgReq;
        if ( isPicReq(req) || isMovieReq(req) ) {
            const orgGeo = readGeo(currPage.oirGeo[0]);
            return lessThan(orgGeo, screenGeo());
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

function isZoomImg() {
    const e = getCurrImgElem();
    return e && e.classList.contains("zoom");
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
    showNextSlide(req);
}

function gotoNext() {
    const req = getNavReq("next");
    showNextSlide(req);
}

function gotoPar() {
    const req = getNavReq("par");
    showNextSlide(req);
}

// for slideshows
function goForward() {
    const req = getNavReq("fwrd");
    showNextSlide(req);
}


function gotoChild0() {
    if ( isColSlide(cs.slideType) ) {
        const c = cs.page.contIcons[0];
        if (c) {
            showNextSlide(c.eReq);
        }
    }
}

// reload
function stayHere() {
    return showNextSlide(cs.slideReq);
}

// call catalog edit
function openEdit() {
    const req  = getCurrSlideReq();
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

function toggleZoomImg(zoomPos) {
    if ( isPic() ) {
        if ( isZoomImg() ) {
            showNoTransImg(currPage);       // reset to normal size without animation
        } else {
            showZoomImg(currPage, zoomPos); // zoom into full resolution
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

function toggleZoomAnimation() {
    trc(1, "toggleZoomAnimation fired");
    if ( isZoomImg() ) {
        const i = getCurrImgElem();
        const s = i.style.animationPlayState;
        trc(1, "toggleZoomAnimation: " + s);
        setCSS(i, { animationPlayState: (false ? "paused" : "running")        });
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
        gotoChild0();
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
        gotoChild0();
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

    if ( isKey(e, 104, "h") || isKey(e, 63, "?") ) {
        toggleHelp();
        return false;
    }

    return true;
}

// ----------------------------------------

// install keyboard event handlers

document.onkeypress = keyPressed;
document.onkeyup    = keyUp;

// ----------------------------------------
// build metadata table

const metaInfo = {
    "Descr:Title":                  "Titel",
    "Descr:Subtitle":               "Untertitel",
    "Descr:TitleEnglish":           "Titel engl.",
    "Descr:TitleLatin":             "Titel lat.",
    "Descr:Comment":                "Kommentar",
    "Descr:CommentImg":             "Kommentar Kopie",
    "EXIF:CreateDate":              "Aufnahmedatum",
    "Descr:Address":                "Adresse",
    "Descr:Location":               "Ort",
    // "Descr:GPSPosition":            "Position",
    "Descr:GPSPositionDeg":         "Position in Grad",
    "Descr:GPSAltitude":            "Höhe",
    "Descr:Web":                    "Web",
    "Descr:Wikipedia":              "Wikipedia",
    "Descr:Keywords":               "Schlüsselwörter",
    "EXIF:Model":                   "Kamera",
    "Composite:LensSpec":           "Objektiv",
    "Composite:LensID":             "Objektiv Typ",
    "EXIF:FocalLength":             "Brennweite",
    "EXIF:FocalLengthIn35mmFormat": "Brennweite in 35mm",
    "EXIF:ExposureTime":            "Belichtungszeit",
    "EXIF:FNumber":                 "Blende",
    "EXIF:ExposureCompensation":    "Belichtungskorrektur",
    "EXIF:ISO":                     "ISO",
    "EXIF:ExposureMode":            "Belichtungsmessung",
    "EXIF:ExposureProgram":         "Aufnahmebetriebsart",
    "MakerNotes:FocusDistance":     "Entfernung",
    "Composite:DOF":                "Tiefenschärfe",
    "Composite:FOV":                "Sichtfeld",
    "Composite:HyperfocalDistance": "Hyperfokale Distanz",
    "MakerNotes:ShootingMode":      "Aufnahmemodus",
    "EXIF:WhiteBalance":            "Weißabgleich",
    "MakerNotes:ShutterCount":      "Aufnahmezähler",
    "Composite:ImageSize":          "Geometrie",
    "File:RefImg":                  "Bilddatei",
    "File:RefRaw":                  "Raw-Datei",
    "File:DirName":                 "Verzeichnis",
    "File:MimeType":                "Dateityp",
    "Composite:Megapixels":         "Megapixel",
    "File:FileSize":                "Dateigröße",
    "File:RefJpg":                  "Bild-Kopie",
    "Gif:AnimationIterations":      "Animation: Wiederh.",
    "Gif:Duration":                 "Animation: Dauer",
    "Gif:FrameCount":               "Animation: # Bilder",
    "QuickTime:Duration":           "Video-Dauer",
    "QuickTime:VideoFrameRate":     "Frame-Rate",
    "File:DateTime":                "Bearbeitet",
    "Descr:Rating":                 "Bewertung"
};

// the function table for formating values
// default is newText(v)

const metaFmt = {
    "Composite:FOV":        fmtDeg,
    "Composite:Megapixels": fmtMPX,
    "Descr:GPSPositionDeg": fmtGPS,
    "Descr:Rating":         fmtRating,
    "Descr:Web":            fmtWeb,
    "Descr:Wikipedia":      fmtWeb
};

function lookupFmt(k) {
    return metaFmt[k] || newText;
}

function fmtMPX(t) {
    return newText("" + (1 * t));
}

function fmtDeg(t) {
    const deg = String.fromCharCode(176);
    return newText(t.replace(/ deg/g, deg));  // regex to replace all occourences
}

var gpsUrl;  // hack: global var for 2. parameter

function fmtGPS(t) {
    const a   = newElem("a");
    const txt = fmtDeg(t);

    a.href    = gpsUrl;
    a.target  = "_blank";
    a.classList.add("gpslink");
    a.appendChild(txt);
    return a;
}

function toDegree(t) {
    trc(1, "toDegree: " + t);
    return t;
}

function fmtRating(n) {
    const star  = String.fromCharCode(9733);
    const stars = replicate(1 * n, star);   // conversion to int
    const txt   = newText(stars);
    const spn   = newElem("span");
    spn.classList.add("rating");
    spn.appendChild(txt);
    return spn;
}

// web attribute can be a list of urls separated by whitespace

function fmtWeb(t) {
    const urls = t.split("|");
    if (urls.length === 0) {
        return newText("");
    }

    var url = fmtWeb1(urls[0]);
    if (urls.length === 1) {
        return url;
    }

    var res = newElem("div");
    res.append(url);

    for (let i = 1; i < urls.length; i++) {
        url = fmtWeb1(urls[i]);
        res.append(newElem("br"));
        res.append(url);
    }

    return res;
}

function fmtWeb1(url) {
    const txt = newText(url.trim());
    const a   = newElem("a");
    a.href    = url;
    a.target  = "_blank";
    a.classList.add("weblink");
    a.appendChild(txt);
    return a;
}

function buildMetaInfo (t, md) {
    // clear metadata table
    clearCont(t);

    // hack for google maps url
    gpsUrl = md["Descr:GPSurl"];

    for (k in metaInfo) {
        const v = md[k];
        if ( v ) {
            // build key div
            const kw = newElem("div", "", "key");
            const kx = newText(metaInfo[k]);
            kw.appendChild(kx);

            // build value div
            const vl = newElem("div", "", "value");
            const tx = lookupFmt(k)(md[k]);
            vl.appendChild(tx);

            // insert key-value into info table
            t.appendChild(kw);
            t.appendChild(vl);
        }
    }
}

// ----------------------------------------

function getJsonPage(url, processRes, processErr, processNext) {
    trc(1, "getJsonPage: " + url);

    $.ajax({
        type: "GET",
        url: url
    }).done(function (res) {
        trc(1, "getJsonPage: new page=" + res);
        processRes(res);
    }).fail(function (err) {
        trc(1, "getJsonPage: server error=" + err + ", status=" + err.status);
        const msg = err.responseJSON || err.responseText;
        processErr(err.status, url, msg);
    }).always(processNext);
}

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
              ? gotoChild0()
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
const statusDur   = 2.0 * 1000;      // default: status messages are shown for 2 seconds

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
                           function () {
                               handleImageAnim(id);
                            });
    }
}

// ----------------------------------------
// image1 / image2 overlay animation

function handleImageAnim(id) {
    trc(1, `handleImageAnim: ${id}`);
    const e = getElem(id);
    nextAnimClass(e, "fadeinImage", "visibleImage");
    nextAnimClass(e, "fadeoutImage", "hiddenImage")
        && clearImageElem(e);
}

function hideImageElem(id) {
    trc(1, "hideImageElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadeinImage",  "fadeoutImage");
    nextAnimClass(e, "visibleImage", "fadeoutImage") && clearCont(e);
}

function showImageElem(id) {
    trc(1, "showImageElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadeoutImage", "fadeinImage");
    nextAnimClass(e, "hiddenImage",  "fadeinImage");
}

function isHiddenImage(id) {
    trc(1, 'isHiddenImage: id=' + id);
    const cs = getElem(id).classList;
    return cs.contains("hiddenImage") || cs.contains("fadeoutImage");
}

// ----------------------------------------
// info, help & status animation

function handleInfoAnim(id) {
    trc(1, "handleInfoanim:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadeinInfo", "visibleInfo");
    nextAnimClass(e, "fadeoutInfo", "hiddenInfo");
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
    nextAnimClass(e, "fadeinInfo",  "fadeoutInfo");
    nextAnimClass(e, "visibleInfo", "fadeoutInfo");
}

function showAnimElem(id) {
    // trc(1, "toggleAnimElem:" + id);
    const e = getElem(id);
    nextAnimClass(e, "fadeoutInfo", "fadeinInfo");
    nextAnimClass(e, "hiddenInfo",  "fadeinInfo");
}

function isHiddenAnim(id) {
    // trc(1, 'isHiddenAnim: id=' + id);
    const cs = getElem(id).classList;
    return cs.contains("hiddenInfo") || cs.contains("fadeoutInfo");
}

// ----------------------------------------


function fade(start, end) {
    function doit(dur) {
        const kf = [
            { opacity: start},
            { opacity: end},
        ];
        const t = {
            duration:   dur,
            iterations: 1,
            fill:      "forwards",
        };
        return { keyFrames: kf,
                 timing:    t,
               };
    }
    return doit;
}

const fadeout = fade(1, 0);
const fadein  = fade(0, 1);
const noAnim  = fade(1, 1);  // do nothing for a while

function scale(s1, s2) {
    function doit(dur) {
        const kf = [
            { transform: `scale(${s1})`},
            { transform: `scale(${s2})`},
        ];
        const t = {
            duration:   dur,
            iterations: 1,
            fill:      "forwards",
        };
        return { keyFrames: kf,
                 timing:    t,
               };
    }
    return doit;
}

const scalein  = scale(0,1);
const scaleout = scale(1,0);

const magnify  = (s) => { return scale(1, s); };
const shrink   = (s) => { return scale(s, 1); };

const magnify2 = magnify(2);
const shrink2  = shrink(2);

function runAnim(e, a) {

    function doit(k) {
        trc(1, "runAnim: animation = " + JSON.stringify(a));

        var animation;

        function k1() {
            trc(1, e.tagName + " " + e.id + ": animation finished");

            // Commit animation state to style attribute
            animation.commitStyles();

            // Cancel the animation
            animation.cancel();

            trc(1, "continuation called");
            k();
        }

        // animation created, installed and returned
        trc(1, "start anim");
        animation = e.animate(a.keyFrames, a.timing);
        trc(1, "anim started");
        animation.onfinish = k1;
    };
    return doit;
}

// run 2 animations sequentially for 2 elemennts

function runAnimSeq2(a1, a2) {
    function seq(e1, e2) {
        function doit(k) {
            runAnim(e1, a1)(() => { runAnim(e2, a2)(k); });
        }
        return doit;
    }
    return seq;
}

// run 2 animations sequentially for the same elemennts

function runAnimSeq(a1, a2) {
    return (e) => {
        return runAnimSeq2(a1, a2)(e, e);
    };
}

// run 2 animations for 2 different elemennts in parallel
// syncronisation is doen with snd animation

function runAnimPar2(a1, a2) {
    function par(e1, e2) {
        function doit(k) {
            runC(runAnim(e1, a1)); // run async continuation
            runAnim(e2, a2)(k);
        };
        return doit;
    }
    return par;
}

// --------------------

function finishImg(e, a) {

    function doit(k) {
        runAnim(e, a)(() => {
            clearImageElem(e);
            k();
        });
    }
    return doit;
}

function animVisible(e, a) {

    function doit(k) {
        setCSS(e, {display: "block", opacity: 0});
        runAnim(e, a)(k);
    }
    return doit;
}

function animNotVisible(e, a) {

    function doit(k) {
        runAnim(e, a)(() => {
            setCSS(e, {display: none, opacity: ''});
            k();
        });
    }
    return doit;
}

function fadeoutImg(e, dur) { return finishImg(e, fadeout(dur)); }

// ----------------------------------------
// cs: current slide
// ls: last slide

var cs = { url       : "",
           page      : {},
           imgId     : img1,
           slideType : "",
           resizeAlg : "",
         };
var ls = { url       : "",
           page      : {},
           imgId     : img2,
           slideType : "",
           resizeAlg : "",
         };

function gotoUrl(url, resizeAlg, zoomPos) {
    runC(compl([ gotoSlide(url, resizeAlg, zoomPos),
                 switchSlide(),
                 animTransitionDefault(),
               ])
        );
}

function gotoSlide(url, resizeAlg, zoomPos) {

    function doit(k) {

        function jsonPage(page) {
            trc(1, "jsonSlide: " + JSON.stringify(page));

            // save old slide context
            ls = cs;

            // build new slide context
            const req = page.imgReq || page.colDescr.eReq;

            cs = { url       : url,
                   page      : page,
                   imgId     : nextimg[ls.imgId],
                   slideType : "",
                   resizeAlg : resizeAlg || "no-magnify",
                   zoomPos   : zoomPos   || halfGeo(screenGeo()),
                   slideType : req.rType,
                   slideReq  : req,
                 };

            k();
        }
        getJsonPage(url, jsonPage, showErr);
    }
    return doit;
}

function switchSlide() {
    function doit(k) {
        trc(1, "switchSlide");

        const page = cs.page;
        const sType  = cs.slideType;

        // for old show funtions
        lastPage = ls.page;
        currPage = cs.page;

        buildInfo();
        setPageTitle();

        if (sType == "json") {
            buildCollectionSlide()(k);
        }
        else if (sType === "movie" || sType === "gif") {
            buildMovieSlide()(k);
        }
        else if (sType === "page") {
            buildBlogSlide()(k);
        }
        else if ( ["img", "imgfx", "icon", "iconp"].includes(sType) ) {
            loadImgCache()(k);
        }
        else {
            trc(1, "switchSlide stop continuation: illegal slide type " + sType);
        }
    }
    return doit;
}

function buildMovieSlide() {
    function doit(k) {
        const page   = cs.page;
        const id     = cs.imgId;
        const rType  = cs.slideType;      // movie / gif
        const rAlg   = cs.resizeAlg;      // magnify / no-magnify

        const geo    = readGeo(page.oirGeo[0]);
        const url    = toMediaUrl(page.img);   // url of the original media file, not the coll entry url

        const movGeo = fitToScreenGeo(geo, rAlg);
        const offset = pxGeo(placeOnScreen(movGeo));
        const g      = pxGeo(movGeo);
        const style  = { width  : g.w,
                         height : g.h,
                         left   : offset.w,
                         top    : offset.h
                       };

        trc(1, `buildMovieSlide: url=${url}, geo=${showGeo(geo)}, movGeo=${showGeo(movGeo)}`);


        // get element, clear contents and set style attributes
        const e = getElem(id);   // ??? clearCont(id);
        setCSS(e, style);

        if (rType === "movie" ) {

            const cls = "movie gif " + rAlg;
            const v   = newElem("video", mkImgId(id), {}, cls);

            v.width   = movGeo.w;
            v.heigth  = movGeo.h;

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
            s.src   = url;
            s.type  = "video/mp4";

            const w = newElem("span");
            w.appendChild(newText("your browser does not support HTML5 video"));

            v.appendChild(s);
            v.appendChild(w);
            e.appendChild(v);

        }
        else if ( rType === "gif" ) {

            const css = { width:  g.w, height: g.h };
            const cls = "movie gif " + rAlg;
            const v2  = newImgElem(id, css, cls);

            v2.src    = url;
            e.appendChild(v2);
        }

        k();   // start slide transition
    }
    return doit;
}

function buildBlogSlide() {
    function doit(k) {
        const page = cs.page;
        const id   = cs.imgId;

        const req  = page.imgReq;
        const geo  = pxGeo(screenGeo());
        const txt  = getPageBlog(page);

        trc(1, "buildBlogPage: " + txt);

        // get element, clear contents and set style attributes
        const e  = clearCont(id);
        setCSS(e, { width:  geo.w,
                    height: geo.h,
                    top:    "0px",
                    left:   "0px"
                  });

        // build blog contents div
        const b  = newElem("div", id + "-blog",
                           { width:    geo.w,
                             height:   geo.h,
                             overflow: "auto"
                           },
                           "blog"
                          );
        b.innerHTML = txt;
        e.appendChild(b);

        k();  // show blog slide
    }
    return doit;
}

function buildCollectionSlide() {
    function doit(k) {
        const page     = cs.page;
        const id       = cs.imgId;

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
        const g        = pxGeo(screenGeo());

        const e        = getElem(id);
        setCSS(e, { width:    g.w,
                    height:   g.h,
                    left:     "0px",
                    top:      "0px",
                    overflow: "auto"
                  });
        e.appendChild(buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons, colBlog));

        k();   // continue with transition from last slide to this one
    }
    return doit;
}

function loadImgCache() {

    function doit(k) {
        const scrGeo    = screenGeo();
        const resizeAlg = cs.resizeAlg;
        const zoomPos   = cs.zoomPos;

        const page    = cs.page;
        const imgReq  = page.imgReq;
        const orgGeo  = readGeo(page.oirGeo[0]);  // original geo of image
        const imgGeo  = resizeToScreenHeight(orgGeo);

        const urlImg = ( ( resizeAlg === "fullsize"
                           ||
                           resizeAlg === "zoom"
                         )
                         && fitsInto(screenGeo(), orgGeo)
                       )
              ? imgReqToUrl(imgReq, readGeo("org"))
              : ( resizeAlg === "panorama"
                  &&
                  isPano(imgGeo)
                  ? imgReqToUrl(imgReq, resizeToScreenHeight(imgGeo))
                  : imgReqToUrl(imgReq)
                );

        cs.urlImg    = urlImg;  // save image url in context

        trc(1, "loadImgCache: urlImg=" + urlImg + ", geo=" + showGeo(imgGeo));

        function k1() {
            const id = cs.imgId;

            trc(1, `onload loadImgCache: ${id}` );

            const geo =
                  ( resizeAlg === "fullsize"
                    ||
                    resizeAlg === "zoom"
                  )
                  ? orgGeo
                  : imgGeo;

            switchResizeImg(urlImg, geo, resizeAlg, zoomPos)(k);
        };

        picCache.onload = k1;
        picCache.src    = urlImg; // the .onload handler is triggered here
    }
    return doit;
}

function switchResizeImg(url, geo, resizeAlg, pos) {
    const id = cs.imgId;

    function doit(k) {
        if (resizeAlg === "fullsize") {
            loadFullImg(id, url, geo);
            k();
        }
        else if (resizeAlg === "zoom") {
            loadZoomImg(id, url, geo, pos);
            k();
        }
        else if (resizeAlg === "panorama") {
            loadPanoramaImg(id, url, geo);
            k();
        }
        else {
            addZoomableImg(url, geo, resizeAlg)(k);
        }
    }
    return doit;
}

function addZoomableImg(url, geo, resizeAlg) {

    function doit(k) {
        const id     = cs.imgId;
        const imgGeo = fitToScreenGeo(geo, resizeAlg);
        const offset = placeOnScreen(imgGeo);
        const style  = styleGeo(imgGeo, offset, "hidden");

        function initZoom(e) {
            trc(1, "initZoom: start zooming image with id=" + this.id );

            const pos = { w: e.offsetX,
                          h: e.offsetY
                        };
            trc(1, "pos=" + showGeo(pos));
            showZoomImg(currPage, pos);  // TODO
        }

        function addHandler(i) {
            i.addEventListener("dblclick", initZoom);
        }

        addImgToDom(id, url, style, geo, "img " + resizeAlg, addHandler);
        k();
    }
    return doit;
}

function addImgToDom(id, url, style, geo, cls, addHandler) {
    // get element, clear contents and set style attributes

    const e = clearCont(id);
    setCSS(e, style);
    const i = newImgElem(id, styleSize(geo), cls);
    addHandler(i);
    i.src   = url;
    e.appendChild(i);
}

// --------------------

function animCurrent(a) {
    function doit(k) {
        const e = getElem(cs.imgId);
        e.classList.value = "visibleImage";
        runAnim(e, a)(k);
    }
    return doit;
}

function animLast(a) {
    function doit(k) {
        if ( ls.slideType != "") {
            const e = getElem(ls.imgId);
            runAnim(e, a)(() => {
                e.classList.value = "hiddenImage";
                clearImageElem(e);
                k();
            });
        } else {
            k();
        }
    }
    return doit;
}

function transFadeOutIn(aout, ain) {
    return comp(animLast(aout), animCurrent(ain));
}

function transCrossFade(aout, ain) {
    function doit(k) {
        runC(animLast(aout)); // run async continuation
        animCurrent(ain)(k);
    }
    return doit;
}

// --------------------

function animTransition(dur) {

    function doit(k) {
        // global context variables cs and ls need to be accessed here
        // not when animTranition is executed

        // default transition: fadeout-fadein
        var tr = transFadeOutIn(fadeout(dur), fadein(dur));

        if ( dur === 0 ) {

            // transition with duration 0 sec: only CSS settings needed
            tr = transCrossFade(fadeout(dur), fadein(dur));
        }
        else if ( ( isMediaSlide(cs.slideType) && isMediaSlide(ls.slideType) )
                  ||
                  ( isColSlide(cs.slideType) && isColSlide(ls.slideType) )
                ) {

            // both slides are media slides or both a collection slides: cross-fade
            tr = transCrossFade(fadeout(dur), fadein(dur));
        };
        tr(k);
    }
    return doit;
}

function animTransitionDefault() {
    return animTransition(animDur());
}

function animDur() {
    return defaultTransDur * 1000;
}

// --------------------
//
// slide predicates

function isColSlide(slideType) {
    return slideType === "json";
}

function isMediaSlide(slideType) {
    return ["img",
            "imgfx",
            "icon",
            "iconp",
            "gif",
            "movie",
           ].includes(slideType);
}

function isBlogSlide(slideType) {
    return slideType === "page";
}

// ------------------------------------------------------------
//
// continuation composition

// identity continuation

function idC(k) {
    // trc(1, "idC");
    k();
}

// continuation composition

function comp(f1, f2) {
    function doit(k) {
        // trc(1, "comp.doit");
        f1(() => { f2(k); });
    }
    return doit;
}

//  composition of a list of continuations

function compl(fs) {
    var i = fs.length;
    var f = idC;

    while ( i > 0) {
        i--;
        f = comp(fs[i], f);
    }
    function doit(k) { f(k); }
    return doit;
}

// terminate continuation chain

var ccount = 0;

function runC(c) {
    var cid = ++ccount;

    function fin() {
        trc(1, `runC: (${cid}) terminated`);
    }
    trc(1, `runC: (${cid}) started`);
    c(fin);
}

// --------------------
//
// some simple tests

const c0 = "/docs/json/1600x1200/archive/collections/albums.json";
const c1 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder.json";
const u1 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder/pic-0000.json";
const u2 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder/pic-0001.json";
const u3 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder/pic-0002.json";

function k1(u) {
    return compl(
        [ gotoSlide(u),
          switchSlide(),
          animTransitionDefault(),
          animCurrent(magnify(0.5)(1000)),
          animCurrent(noAnim(1000)),
          animCurrent(shrink(0.5)(1000)),
        ]);
}

function ttt(u) {
    runC(comp(gotoSlide(u), switchSlide()));
}

// ----------------------------------------
