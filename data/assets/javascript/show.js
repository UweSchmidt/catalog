/* show.js single page slideshow */

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

function scaleGeo(g, s) {
    return { w : Math.floor(g.w * s),
             h : Math.floor(g.h * s)
    };
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

function resizeToHeight(s, d) {
    return resizeGeo(s, {w: infiniteWidth, h: d.h});
}

function resizeToWidth(s, d) {
    return resizeGeo(s, {w: d.w, h: infiniteWidth});
}

function resizeToScreenHeight(g) {
    return resizeToHeight(g, screenGeo());
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
    const sg = screenGeo();
    return { x : div(sg.w - geo.w, 2),
             y : div(sg.h - geo.h, 2)
           };
}

const geoOrg = readGeo("org");

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
    if (s.w <= 1400)
        return readGeo("140x105");

    return readGeo("160x120");
}

/* ---------------------------------------- */
/* urls */

function toHref(url) {
    return "javascript:gotoUrl('"
        + url
        + "');";
}

function jsonReqToUrl(jsonReq) {
    const pp = rPathPosToUrl(jsonReq.rPathPos);
    return "/docs/json/"
        + showGeo(bestFitToScreenGeo())
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

function toggleImg12(id)  {
    const trans = getTransition(currPage, lastPage);
    trans(id, nextimg[id]);
}

// ----------------------------------------
// simplest transition: exchange images without animation

function getTransition(cp, lp) {
    if ( defaultTransDur === 0) {
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

function clearImageElem(e) {
    clearCont(e);
    setCSS(e, {"animation-duration": null,
               "animation-delay":    null,
              });
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
    const g = toPx(screenGeo());
    console.log("initShow: screen geo=" + showGeo(g));

    initHandlers();

    setCSS(imgTab, {width : g.w, height : g.h});
    showPath(pathCollections());
}

// ----------------------------------------
// display an ordinary image

function loadImg(id, url, geo, resizeAlg) {
    const imgGeo = fitToScreenGeo(geo, resizeAlg);
    const offset = placeOnScreen(imgGeo);
    const off    = toPx(offset);
    const g      = toPx(imgGeo);
    const style  = { width    : g.w,
                     height   : g.h,
                     left     : off.x,
                     top      : off.y,
                     overflow : "hidden"
                   };
    // get element, clear contents and set style attributes
    const e = clearCont(id);
    setCSS(e, style);

    const i = newImgElem(id,
                         { width:  g.w,
                           height: g.h
                         },
                         "img " + resizeAlg
                        );
    i.src   = url;
    e.appendChild(i);
}

function loadFullImg(id, url, imgGeo) {
    const scrGeo = screenGeo();
    const style  = { width    : scrGeo.w + "px",
                     height   : scrGeo.h + "px",
                     left     : "0px",
                     top      : "0px",
                     overflow : "auto"     // !!! image becomes scrollable
                   };
    // get element, clear contents and set style attributes
    const e = clearCont(id);
    setCSS(e, style);

    const i = newImgElem(id,
                         { width:  imgGeo.w + "px",  // original geo
                           height: imgGeo.h + "px"   // often larger than screen geo
                         },
                         "img fullsize"
                        );
    i.src   = url;
    e.appendChild(i);
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

    const style = { width    : scrGeo.w + "px",
                    height   : scrGeo.h + "px",
                    left     : "0px",
                    top      : "0px",
                    overflow : "hidden"
                  };
    const e = clearCont(id);
    setCSS(e, style);

    function animationEndFunction() {
        trc(1, "animation of panorama has finished");
    }

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
    else {
        loadImg(id, req, geo, resizeAlg);
    }
}

function showImg1(page, resizeAlg) {
    const imgReq = page.imgReq;
    const orgGeo = readGeo(page.oirGeo[0]);  // original geo of image
    const imgGeo = resizeToScreenHeight(orgGeo);

    const imgUrl = (resizeAlg === "fullsize" && fitsInto(screenGeo(), orgGeo))
          ? imgReqToUrl(imgReq, readGeo("org"))
          : ( resizeAlg === "panorama" && isPano(imgGeo)
              ? imgReqToUrl(imgReq, resizeToScreenHeight(imgGeo))
              : imgReqToUrl(imgReq)
            );

    trc(1, "showImg: imgUrl=" + imgUrl + ", geo=" + showGeo(imgGeo));

    picCache.onload = () => {
        const id = nextImgId();
        loadImg1(id, imgUrl,
                 resizeAlg === "fullsize" ? orgGeo : imgGeo,
                 resizeAlg);
        toggleImg12(id);
    };
    picCache.src = imgUrl; // the .onload handler is triggered here
}

function showImg(page)          { showImg1(page, "no-magnify"); }
function showMagnifiedImg(page) { showImg1(page, "magnify");    }
function showFullSizeImg(page)  { showImg1(page, "fullsize");   }
function showPanoramaImg(page)  { showImg1(page, "panorama");   }

// ----------------------------------------

function loadMovie(id, url, geo, rType, resizeAlg) {
    const movGeo = fitToScreenGeo(geo, resizeAlg);
    const offset = toPx(placeOnScreen(movGeo));
    const g      = toPx(movGeo);
    const style  = { width  : g.w,
                     height : g.h,
                     left   : offset.x,
                     top    : offset.y
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

function showBlog(page) {
    const req = page.imgReq;
    const geo = toPx(screenGeo());
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
    const g        = toPx(screenGeo());
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

document.onkeypress = keyPressed;
document.onkeyup    = keyUp;

// ----------------------------------------
// build metadata table

const metaInfo = {
    "Descr:Title":                  "Titel",
    "Descr:Subtitle":               "Untertitel",
    "Descr:TitleEnglish":           "Titel engl.",
    "Descr:TitleLatin":             "Titel lat.",
    "Descr:Comment":                "Kommentar (Aufnahme)",
    "Descr:CommentImg":             "Kommentar (Kopie)",
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

function fmtWeb(url) {
    const txt = newText(url);
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
