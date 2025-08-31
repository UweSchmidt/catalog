/* zoom.js single page slideshow with zoom */

function trc (t, Text) {
    if ( t > 0 ) {
        console.log(Text);
    }
}

/* ---------------------------------------- */
/* id's */

const version      = "PhotoShow (zoom.html) 0.5.12.1 2025-07-30";

const titleId      = "head-title";

const imgTab       = "imageTab";
const img1Id       = "image1";
const img2Id       = "image2";
const nextimg      = {image1: img2Id, image2: img1Id};

const infoDescr    = { id:      "info",
                       visible: false,
                       tab:     "info-table",
                     };

const helpDescr    = { id:      "help",
                       visible: false,
                     };

const statusDescr  = { id:      "status",
                       visible: false,
                       enabled: true,
                       dur:     1500,
                     };

const audioDescr  = { id: "audio-control",
                      visible: false,
                    };

const videoAttrs  = { controls: "",   // default video attributes
                      autoplay: null,
                      muted:    null,
                    };

/* ---------------------------------------- */
/* functional basics */

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
/* geometry data structures */

function mkGeo(w, h) {        // size of a rectangle
    return { w: w,
             h: h
           };
}

function isGeo(g) {
    return (typeof g === "object")
        && g.hasOwnProperty("w")
        && g.hasOwnProperty("h");
}

const mkOff = mkGeo;
const isOff = isGeo;

// --------------------

function mkRect(geo, off) {
    return { geo: geo,
             off: off || nullGeo,
           };
}

function isRect(r) {
    return (typeof r === "object")
        && r.hasOwnProperty("geo")
        && r.hasOwnProperty("off");
}

function showRect(r) {
    return showGeo(r.geo) + showOff(r.off.w) + showOff(r.off.h);
}

function showOff(x) {
    return ( x >= 0 ? "+" : "") + x;
}

// --------------------

function mkTrans(start, finish) {
    return { start:  start,
             finish: finish
           };
}

function isTrans(t) {
    return (typeof t === "object")
        && t.hasOwnProperty("start")
        && t.hasOwnProperty("finish");
}

function invertTrans(t) { return mkTrans(t.finish, t.start); }

// --------------------

const nullGeo       = mkGeo(0, 0);
const oneGeo        = mkGeo(1, 1);

// --------------------

function mapGeo(op) {
    return (g) => { return mkGeo(op(g.w), op(g.h)); };
}

const mapOff = mapGeo;

const roundGeo = mapGeo( (x) => { return Math.round(x); } );
const absGeo   = mapGeo( (x) => { return Math.abs(x); } );
const toPxGeo  = mapGeo( (x) => { return x + "px"; } );

function halfGeo(g) { return divGeo(g,2); }
function pxGeo  (g) { return toPxGeo(roundGeo(g)); }

// --------------------

function liftGeo(op) {

    function doit(g1, g2) {
        if ( typeof g1 === "number" ) {                 // scalar `op` geo
            return liftGeo(op)({w: g1, h: g1}, g2);
        }

        if ( typeof g2 === "number" ) {                 // geo `op` scalar
            return liftGeo(op)(g1, {w: g2, h: g2});
        }

        return {w: op(g1.w, g2.w), h: op(g1.h, g2.h)};  // geo `op` geo
    }
    return doit;
}

const addGeo = liftGeo( (x, y) => { return x + y; } );
const subGeo = liftGeo( (x, y) => { return x - y; } );
const mulGeo = liftGeo( (x, y) => { return x * y; } );
const divGeo = liftGeo( (x, y) => { return x / y; } );
const maxGeo = liftGeo( (x, y) => { return Math.max(x, y); } );
const minGeo = liftGeo( (x, y) => { return Math.min(x, y); } );

function eqGeo(g1, g2) { return g1.w === g2.w && g1.h === g2.h; }
function ltGeo(g1, g2) { return g1.w  <  g2.w && g1.h  <  g2.h; }
function leGeo(g1, g2) { return g1.w <=  g2.w && g1.h <=  g2.h; }

// --------------------

function similarGeo(g1, g2, d) {
    d = d || 10;
    const g = absGeo(subGeo(g1, g2));
    return g.w <= d && g.h <= d;
}

function isPano(g) {
    const ar = aspectRatio(g);
    return leGeo(cs.screenGeo, g)
        && ( ar >= 2 || ar <= 0.5 );
}

function isTiny(g) {
    return ltGeo(g, cs.screenGeo);
}

function isHorizontal(g) {
    return aspectRatio(g) > 1;
}

function isVertical(g) {
    return aspectRatio(g) < 1;
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

function isOrgGeo(geo) {
    return showGeo(geo) === "org";
}

// ----------------------------------------
//
// "datatypes" for image geometry, display and animation

const ImgSize = {
    small  : 'small',
    medium : 'medium',
    large  : 'large',

    build : (screen, img) => {
        if ( ltGeo(img, screen) ) {
            return ImgSize.small;        // image fits into screen
        }
        if ( leGeo(screen, img) ) {
            return ImgSize.large;        // image fills whole screen
        }
        return ImgSize.medium;           // no fit, no fill
    },
};

const Aspect = {
    square    : 'square',
    landscape : 'landscape',
    portrait  : 'portrait',

    build : (geo) => {
        const ar = aspectRatio(geo);
        if ( ar > 1 ) {
            return  Aspect.landscape;
        }
        if ( ar < 1 ) {
            return Aspect.portrait;
        }
        return Aspect.square;
    },
};

const Pano = {
    noPano     : 'noPano',
    horizontal : 'horizontal',
    vertical   : 'vertical',

    build : (geo) => {
        const ar = aspectRatio(geo);
        if ( ar > 2 ) {
            return Pano.horizontal;
        }
        if ( ar < 0.5 ) {
            return Pano.vertical;
        }
        return Pano.noPano;
    },
};

const Dir = {
    center : 'center',       // align image
    top    : 'top',
    bottom : 'bottom',
    left   : 'left',
    right  : 'right',
};


const Display = {
    noAnim   : "NoAnim",         // display unanimated image
    withAnim : "WithAnim",       // move and scale animation

    NoAnim   : (pl) => {
        return { name  : Display.noAnim,
                 place : pl,
               };
    },

    WithAnim : (ms) => {
        return { name      : Display.withAnim,
                 moveScale : ms,
               };
    },
};

const Place = {
    // algorithm descr to place an image onto screen

    // constructor names
    fill     : 'Fill',       // fill the whole stage
    fitsInto : 'FitsInto',   // show the whole image
    align    : 'Align',      // align image: center, top, ...
    scale    : 'Scale',      // scale image
    move     : 'Move',       // shift image on stage
    zoom     : 'Zoom',       // zoom image to 1-1 resolution
    last     : 'Last',       // geometry from last shown image
    id       : 'Id',
    seq      : 'Seq',        // apply 2 algorithms sequentially

    // constructor functions
    Fill     : () => {
        return { name : Place.fill
               };
    },
    FitsInto : () => {
        return { name : Place.fitsInto
               };
    },
    Align    : (d) => {
        let dl  = [].concat(d);         // hack: create a list
        let res = { name : Place.align,
                    dir  : dl.pop()
                  };
        while ( dl.length > 0) {
            const p1 = { name : Place.align,
                         dir  : dl.pop(),
                       };
            res = Place.Seq(p1, res);
        }
        return res;
    },
    Scale    : (s) => {
        return { name  : Place.scale,
                 scale : s
               };
    },
    Move     : (o) => {
        return { name : Place.move,
                 off  : o
               };
    },
    Zoom     : (o) => {
        return { name   : Place.zoom,
                 relOff : o || halfGeo(oneGeo)  // center of image
               };
    },
    Last     : () => {
        return { name : Place.last
               };
    },
    Id       : () => {
        return { name : Place.id
               };
    },
    Seq      : (a1, a2) => {
        return { name : Place.seq,
                 p1  : a1,
                 p2  : a2
               };
    },

    // default place algorithm, can be configured, e.g fill/fitsInto

    default  : 'Fill',    // unspecified

    Default  : () => {
        return Place.Fill();
    },

    setDefault : (c) => {
        Place.Default = c;
        Place.default = c().name;
    },
};

const MoveScale = {
    // algorithm for move/scale animation

    // constructor names

    zoomIn        : 'zoomIn',
    zoomOut       : 'zoomOut',
    fitToFill     : 'fitToFill',
    smallToFit    : 'smalltoFill',
    panoLeftRight : 'panoLeftRight',
    panoBottomTop : 'panoBottomTop',

    // constructor functions
    ZoomIn : (o) => {
        return { name    : MoveScale.zoomIn,
                 relOff  : o,
                 reverse : false,
               };
    },
    ZoomOut : () => {
        return { name    : MoveScale.zoomIn,
                 reverse : false,
               };
    },
    FitToFill : (rev) => {
        return { name    : MoveScale.fitToFill,
                 reverse : rev || false,
               };
    },
    SmallToFit : (rev) => {
        return { name    : MoveScale.smallToFit,
                 reverse : rev || false,

               };
    },
    PanoLeftRight : (rev) => {
        return { name    : MoveScale.panoLeftRight,
                 reverse : rev || false,
               };
    },
    PanoBottomTop : (rev) => {
        return { name    : MoveScale.panoBottomTop,
                 reverse : rev || false,
               };
    },
};

// ----------------------------------------

function imgDescr(scGeo, imgGeo) {
    const size   = ImgSize.build(scGeo, imgGeo);
    const aspect = Aspect.build(imgGeo);
    const pano   = Pano.build(imgGeo);

    const descr  = {
        geo    : imgGeo,
        screen : scGeo,
        size   : size,
        aspect : aspect,
        pano   : ( size === ImgSize.large ? pano : Pano.noPano ),
    };
    return descr;
}

// ----------------------------------------
//
// algorithms to show an image

function evalDisplay(display, img) {
    const sg  = screenGeo();
    const res = evalDisplay1(sg)(display, mkRect(img, nullGeo));
    trc(1,
        "evalPlace: screen = " + showGeo(sg) +
        ", name = "  + display.name +
        ", img = " + showGeo(img) +
        ", res.start = "  + showRect(res.start) +
        ", res.finish = " + showRect(res.finish)
       );
    return res;
}

function evalDisplay1(sg) {

    function doit(s, r) {
        switch ( s.name ) {

        case Display.noAnim:
            const r1 = evalPlace1(sg)(s.place, r);
            return mkTrans(r1, r1);

        case Display.withAnim:
            return evalMoveScale1(sg)(s.moveScale, r);

        default:
            trc(1, "evalDisplay: unknown anim algorithm: " + p.name);
            return mkTrans(r, r);
        }
    }

    return doit;
}


function evalMoveScale1(sg) {

    function doit(ms, r) {
        return mkTrans(r, r);   // TODO: dummy
    }

    return doit();
}

function evalPlace(place, rect) {
    const sg  = screenGeo();
    const res = evalPlace1(sg)(place, rect);
    trc(1,
        "evalPlace: screen = " + showGeo(sg) +
        ", name = "  + place.name +
        ", rect = " + showRect(rect) +
        ", res = "  + showRect(res)
       );
    return res;
}

function evalPlace1(sg) {

    function doit(p, r) {
        switch ( p.name ) {

        case Place.fill:                                             // resize image
            return mkRect(fills(r.geo, sg), r.off);

        case Place.fitsInto:
            return mkRect(fitsInto(r.geo, sg), r.off);

        case Place.scale:
            return mkRect(mulGeo(r.geo, p.scale), r.off);

        case Place.move:                                             // position image
            return mkRect(r.geo, addGeo(r.off, p.off));

        case Place.align:
            switch ( p.dir ) {

            case Dir.center:
                return mkRect(r.geo, placeCenter(r.geo, sg));

            case Dir.top:
                return mkRect(r.geo, mkOff(r.off.w, 0));

            case Dir.bottom:
                return mkRect(r.geo, mkOff(r.off.w, sg.h - r.geo.h));

            case Dir.left:
                return mkRect(r.geo, mkOff(0, r.off.h));

            case Dir.right:
                return mkRect(r.geo, mkOff(sg.w - r.geo.w, r.off.h));

            return r;
            }

        case Place.zoom:                                             // compound place algs
            const r1 = doit(Place.Align(Dir.center), r);  // center image
            const r2 = moveRectRelative(p.relOff, r1);    // move image to clicked position
            return r2;

        case Place.last:
            try {
                return ls.showAnim.finish;                // TODO: cs.showAnim not yet implemented
            } catch (e) {
                return doit(Place.Default(), r);          // last geo not set: take default geo
            }

        case Place.id:
            return r;

        case Place.seq:
            return doit(p.p2, doit(p.p1, r));            // sequence of 2 place algs

        default:
            trc(1, "evalPlace: unknown place algorithm: " + p.name);
            return r;
        }
    }
    return doit;
}

function evalMoveScale1(sg) {
    const evalP = evalPlace1(sg);

    function doit(ms, r) {
        var res;
        switch ( ms.name ) {

        case MoveScale.zoomIn:
            res = mkTrans(evalP(Place.Last(),          r),
                          evalP(Place.Zoom(ms.relOff), r)
                         );
            break;

        case MoveScale.zoomOut:
            res = mkTrans(evalP(Place.Last(),    r),
                          evalP(Place.Default(), r)
                         );
            break;

        case MoveScale.fitToFill:
            res = mkTrans(evalP(Place.FitsInto(), r),
                          evalP(Place.Fill(),     r)
                         );
            break;

        case MoveScale.smallToFit:
            res = mkTrans(r,
                          evalP(Place.FitsInto(), r)
                         );
            break;

        case MoveScale.panoLeftRight:
            { const r1 = evalP(Place.Fill(), r);
              res      = mkTrans(evalP(Place.Align(Dir.left),  r),
                                 evalP(Place.Align(Dir.right), r)
                                );
            }
            break;

        case MoveScale.PanoBottomTop:
            { const r1 = evalP(Place.Fill(), r);
              res      = mkTrans(evalP(Place.Align(Dir.bottom),  r),
                                 evalP(Place.Align(Dir.top), r)
                                );
            }
            break;

        default:
            trc(1, "evalMoveScale1: unknown moveScale algorithm: " + ms.name);
            return mkTrans(r, r);
        }
        if ( ms.reverse ) {
            res = invertTrans(res);
        }
        return res;
    }
    return doit;
}

// ----------------------------------------
//
// computes the maximum geo with aspect ratio of s
// that fits into d

function fitsInto(s, d) {
    if (s.w * d.h >= d.w * s.h)            // s.w / s.h >= d.w / d.h
        return { w : d.w,                  // s is more landscape than d
                 h : d.w * s.h / s.w
               };
    else
        return { w : d.h * s.w / s.h,      // s is more portrait than d
                 h : d.h
               };
}

// computes the minimum geo with aspect ratio of s
// that covers d

function fills(s, d) {
    if (s.w * d.h >= d.w * s.h)            // s.w / s.h >= d.w / d.h
        return { h : d.h,                  // s is more landscape than d
                 w : d.h * s.w / s.h
               };
    else
        return { h : d.w * s.h / s.w,      // s is more portrait than d
                 w : d.w
               };
}

function noScale(s, g) { return s; }       // don't change geo

function resizeToHeight(s, d) {
    return fills(s, {w: 0, h: d.h});
}

function resizeToWidth(s, d) {
    return fills(s, {w: d.w, h: 0});
}

function pano(s, d) {
    return maxGeo( resizeToHeight(s, d),
                   resizeToWidth (s, d)
                 );
}

function cutoffArea(s0, d) {
    const s1 = fills(s0, d);     // resize s0 to cover whole d
    const d1 = subGeo(s1, d);    // area not covered
    const rs = divGeo(d1, s1);   // area rel to d
    return Math.max(rs.w, rs.h);
}

function zoomDist(r0, r1) {
    const zoomDist = halfGeo(absGeo(subGeo(r0.geo, r1.geo)));           // zoom distance
    const center0  = addGeo(r0.off, halfGeo(r0.geo));
    const center1  = addGeo(r1.off, halfGeo(r1.geo));
    const moveDist = absGeo(subGeo(center0, center1));                  // move distance
    const dr       = divGeo(addGeo(zoomDist, moveDist), cs.screenGeo);  // rel. to screen size
    return Math.max(dr.w, dr.h);
}

// --------------------
//
// algorithms for placing image on screen

function placeCenter(g, s) {
    return halfGeo(subGeo(s, g));
}

function placeStart(g, s) {
    const isH = isHorizontal(g);
    const off = isH ? nullGeo : subGeo(cs.panoramaS.geo, cs.screenGeo);
    return off;
}

function placeFinish(g, s) {
    const isH = isHorizontal(g);
    const off = isH ? subGeo(cs.screenGeo, cs.panoramaS.geo) : nullGeo;  // negative offset
    return off;
}

function screenOffsetToRelOffset(co, r) {  // co: click pos in img element
    // trc(1,"screenOffsettoreloffset co=" + showGeo(co) + ", r.geo=" + showGeo(r.geo) + ", r.off=" + showGeo(r.off));
    return divGeo(co, r.geo);              // offset relative to image size
}

function moveRectRelative(ro, r) { // ro: offset relative to rectangle size
    const ro1 = subGeo(halfGeo(oneGeo), ro);
    const off = addGeo(r.off, mulGeo(ro1, r.geo));
    return mkRect(r.geo, off);
}

// --------------------
//
// compute scaled geo and offset for a geo (of an image)

function placeGeo(resizeAlg, placeAlg) {

    function doit(geo) {
        const sgeo = cs.screenGeo;
        const geo1 = resizeAlg(geo,  sgeo);
        const off1 = placeAlg (geo1, sgeo);
        return mkRect(geo1, off1);
    }
    return doit;
}

const fillScreen    = placeGeo(fills,    placeCenter);        // cover screen with image
const fitIntoScreen = placeGeo(fitsInto, placeCenter);        // show whole image on screen
const orgOnScreen   = placeGeo(noScale,  placeCenter);        // don't resize image an show center part
const panoStart     = placeGeo(pano,     placeStart);
const panoFinish    = placeGeo(pano,     placeFinish);

function zoomOnScreen(geo) {
    const rect = orgOnScreen(geo);
    return moveRectRelative(cs.zoomPos, rect);
}

// --------------------

// store current screen geo
// this may change, when browser window is resized

var theScreenGeo = { w : 0, h : 0 };

function screenGeo() {
    const w1 = window.innerWidth;
    const h1 = window.innerHeight;

    if ( w1 != theScreenGeo.w || h1 != theScreenGeo.h ) { // screenGeo has changed
        theScreenGeo.w = w1;
        theScreenGeo.h = h1;
        setCSS(imgTab, cssSize(theScreenGeo));
    }
    return theScreenGeo;
}

// --------------------

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
        if ( leGeo(s, g) ) // (g.w >= s.w && g.h >= s.h)
            return g;
    }
    return geoOrg;
}

function bestFitToScreenGeo () {
    return bestFitToGeo(screenGeo());   // not cs.screenGeo
}

function bestFitIconGeo() {
    const w = cs.screenGeo.w;
    if (w <= 1280)
        return readGeo("120x90");
    if (w <= 1400)                 // cannon beamer geo 1400x1050
        return readGeo("140x105");
    if (w <= 2560)
        return readGeo("160x120"); // iMac 27'' and MacBook Pro 1800x1126

    return readGeo("256x192");     // eizo 4k display
}

function getResizedGeo(cxt) {
    return readGeo(cxt.page.oirGeo[2]);
}

function similarGeoOnScreen() {
    const d = 0.03 * cs.screenGeo.w;  // threshold 3% of screen width in comparison
    const g1 = getResizedGeo(cs);
    const g2 = getResizedGeo(ls);

    return similarGeo(g1, g2, d);    // similar image geometies on screen
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

function cssSize(g, res) {
    res = res || {};
    const gpx  = pxGeo(g);

    res.width  = gpx.w;
    res.height = gpx.h;
    return res;
}

function cssPos(off, res) {
    res = res || {};
    const gpx  = pxGeo(off);

    res.left     = gpx.w;
    res.top      = gpx.h;
    res.position = "absolute";
    return res;
}

function cssGeo(g, off, res) { return cssPos(off, cssSize(g, res)); }

function cssRect(r, res)     { return cssGeo(r.geo, r.off, res); }

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
    return getElem(mkImgId(cs.imgId));
}

// ----------------------------------------

// element e with id is removed from DOM
// and substituted by a new unvislibe element (display: none)
// with same tagName and same id

function clearDomElem(e) {
    if ( e === null ) {
        return null;
    }

    if ( typeof e === "string" ) {
        return clearDomElem(getElem(e));
    }

    const id = e.id;
    const tn = e.tagName;
    const p  = e.parentElement;
    e.remove();

    const ne = newElem(tn, id, {}, "hiddenImage");
    p.insertBefore(ne, p.children[0]);
    return ne;
}

// ----------------------------------------

let defaultTransDur = 1.0;

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
          + ( defaultTransDur === 0
              ? "aus"
              : defaultTransDur + " sec."
            );
    showStatus(msg);
}

/* ---------------------------------------- */
/* initialization */

function initShow() {
    const g = screenGeo();
    console.log("initShow: screen geo=" + showGeo(g));

    // initHandlers(); // TODO cleanup

    setCSS(imgTab, cssSize(g));

    clearDomElem(img1Id);
    clearDomElem(img2Id);

    showPath(pathCollections());

    // turn on hiding mouse curser when mouse isn't moved for a few seconds
    HideCursor && HideCursor.waitThenHideMouse();
}

// ----------------------------------------

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

    // icon geo with border
    const iconGeoB = addGeo(iconGeo, border2);

    const i2h      = div(iconGeo.h - border2 - gap, 2);
    const i2w      = div(3 * i2h, 2);
    const ico2Geo  = {w: i2w, h: i2h};
    const ico2GeoB = addGeo(ico2Geo,border2);

    const gapGeo   = mulGeo(subGeo(gridGeo, 1), gap);
    const navGeo   = addGeo(mulGeo(ico2GeoB, gridGeo), gapGeo);

    const numCols  = Math.max(div(cs.screenGeo.w - 2 * padding, iconGeoB.w + gap));

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
                                    height: iG.h,
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
                const t1 = ( colMeta["Descr:Title"]
                             ||
                             colMeta["Descr:TitleEnglish"]
                             ||
                             colMeta["Descr:TitleLatin"]
                           );
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
                        "min-height": (cs.screenGeo.h - 2 * padding) + "px"
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
    showStatus('<span class="errormsg">' + txt + '</span>');
}

// ----------------------------------------
// page access functions

function getOrgGeo(s) {
    s = s || cs;
    if ( isMediaSlide(s) ) {
        return readGeo(s.page.oirGeo[0]);
    }
    return null;
}

function getSlideMeta(s) {
    s = s || cs;
    if ( isColSlide(s) ) {
        return s.page.colDescr.eMeta;
    }
    return s.page.img[2] || {};
}

function getSlideBlog(s) {
    s = s || cs;
    const md = getSlideMeta(s);   // new blog access: blog is part of metadata
    const bt = md["Descr:Blog"];
    if (bt) {
        return bt;
    } // else {return "";}         // TODO cleanup, when server has been updated
    return s.page.blogCont;      // old blog access: blog is in blogCont field;
}

function getNavReq(nav, s) {
    s = s || cs;
    if ( isColSlide(s) ) {
        return s.page.navIcons[nav].eReq;
    }
    return s.page.imgNavRefs[nav];
}

function getChild0Req(s) {
    s = s || cs;
    if ( isColSlide() ) {
        const req = s.page.contIcons[0].eReq;
        if ( req ) {
            return req;
        }
    }
    return null;
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
    const req = getChild0Req();
    showNextSlide(req);
}

// reload
function stayHere() {
    return showNextSlide(cs.slideReq);
}

// call catalog edit
function openEdit() {
    const pPos  = cs.slideReq.rPathPos;
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

function sameAsLastSlide() {
    const ppc = rPathPosToUrl(cs.slideReq.rPathPos);
    const lll = ls?.slideReq?.rPathPos;
    const ppl = lll ? rPathPosToUrl(lll) : "";
    return ppc === ppl;
}

// ------------------------------------------------------------
//
// event handlers for image animations

function toggleFitFill() {
    if ( isImgSlide() ) {
        if ( cs.resizeAlg == "fitsinto" ) {
            thisSlideWith("fill");
        }
        else if ( cs.resizeAlg == "fill" ) {
            thisSlideWith("fitsinto");
        }
    }
}

// just for testing, currently not used
function toggleZoomSlide(zoomPos) {
    if ( isImgSlide() ) {
        if ( cs.resizeAlg === "zoom" ) {
            thisSlideWith("zoom", zoomPos);
        } else {
            thisSlideWith("default");
        }
    }
}

function togglePanoSlide() {
    if ( isImgSlide() ) {
        if ( isPanoSlide() ) {
            if ( cs.resizeAlg != "panorama" ) {
                thisSlideWith("panorama");
            } else {
                thisSlideWith("default");
            }
        }
    }
}

function togglePanoAnimation() {
    trc(1, "togglePanoAnimation fired");
    if ( cs.resizeAlg === "panorama" ) {
        const i  = getCurrImgElem();
        const as = i.getAnimations();

        if ( as.length > 0 ) {
            const a = as[0];
            const ps = a.playState;
            if ( ps === "paused" ) {
                trc(1, "continue anim");
                a.play();
            }
            else if ( ps === "running" ) {
                trc(1, "pause anim");
                a.pause();
            }
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
    if ( isColSlide() ) {
        const cd = cs.page.colDescr;
        txt = cd.eMeta["Descr:Title"]
            ||
            baseName(cd.eReq.rPathPos);
    } else {
        txt = baseName(cs.page.img[0]);
    }
    const e = getElem(titleId);
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
        stopShow();
        gotoNext();
        return false;
    }
    if ( (e.keyCode == 37)   /* left arrow */
         ||
         (e.keyCode == 33)   /* page up, presenter: left arrow */
         ||
         (e.keyCode == 8)    /* backspace*/
       ) {
        stopShow();
        gotoPrev();
        return false;
    }
    if ( (e.keyCode == 27)     /* escape, presenter: left screen icon */
         ||                    /* presenter: left screen: 116, 27, 116, 27, ... */
         (e.keyCode == 116)    /* F5, presenter: left screen icon */
         ||
         (e.keyCode == 38)     /* up arrow */
       ) {
        stopShow();
        gotoPar();
        return false;
    }
    if ( (e.keyCode == 40)     /* down arrow */
         ||
         (e.keyCode == 190)    /* '.' , presenter right screen icon */
       ) {
        stopShow();
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
        stopShow();
        setAutoShow();
        advanceSlideShow();
        return false;
    }

    if ( // isKey(e, 32, " ")
         // ||
         isKey(e, 62, ">")
         ||
         isKey(e, 110, "n")
       ) {
        stopShow();
        gotoNext();
        return false;
    }

    if ( isKey(e, 60, "<")
         ||
         isKey(e, 112, "p")
       ) {
        stopShow();
        gotoPrev();
        return false;
    }

    if ( isKey(e, 94, "^")
         ||
         isKey(e, 117, "u")
       ) {
        stopShow();
        gotoPar();
        return false;
    }

    if ( isKey(e, 100, "d")
       ) {
        stopShow();
        gotoChild0();
        return false;
    }

    if ( isKey(e, 120, "x")
       ) {
        stopShow();
        stayHere();
        return false;
    }

    if ( isKey(e, 121, "y")
       ) {
        stopShow();
        toggleFitFill();
        return false;
    }

    if ( isKey(e, 89, "Y")
       ) {
        toggleDefaultAlg();
        return false;
    }

    if ( isKey(e, 122, "z") ) {
        hasAudioControl() && toggleAudio();
        return false;
    }

    if ( isKey(e, 115, "s") ) {
        startStopShow(setContColShow);
        return false;
    }

    if ( isKey(e,  83, "S") ) {
        startStopShow(setContGlobShow);
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

    if ( isKey(e, 97, "a") ) {
        stopShow();
        togglePanoSlide();
        return false;
    }

    if ( isKey(e, 113, "q") ) {
        togglePanoAnimation();
        return false;
    }

    if ( isKey(e, 118, "v") ) {
        showVersion();
        return false;
    }

    if ( isKey(e, 101, "e") ) {
        stopShow();
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
    "Descr:Audio":                  "Audio URL",
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

function buildInfo() {
    const md = getSlideMeta();
    const it = getElem(infoDescr.tab);
    buildMetaInfo(it, md);
}

function buildAudio() {
    trc(1, "buildAudio");
    const ac = getElem(audioDescr.id);
    if ( ! sameAsLastSlide() ) {
        trc(1, "buildAudio: clearCont");
        audioDescr.visible = false;
        clearCont(ac);

        if ( isImgSlide() ) {
            const md = getSlideMeta();
            const url = md["Descr:Audio"];

            function err(e) {
                trc(1, "audio url error: " + e.type);
                showStatus("Soundfile konnte nicht geladen werden: " + url);
                clearCont(ac);
            };

            if ( url ) {
                trc(1, "buildAudio: url=" + url);
                const src = newElem("source");
                src.src   = url;
                src.type  = "audio/mpeg";
                // src.addEventListener("error", err);  // ??? doesn't work

                src.onerror = err; // ???

                const ctl = newElem("audio");
                ctl.controls = "true";
                ctl.appendChild(src);

                ac.appendChild(ctl);
                toggleAudio();
            }
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
        trc(1, "getJsonPage: got new page from server");
        processRes(res);
    }).fail(function (err) {
        trc(1, "getJsonPage: server error=" + err + ", status=" + err.status);
        const msg = err.responseJSON || err.responseText;
        processErr(err.status, url, msg);
    }).always(processNext);
}

// ----------------------------------------
//
// show mode stuff

const ShowModes = {
    manual   : 'manual',       // advance by key press
    auto     : 'auto',         // advance by metadata of curr pic
    contCol  : 'contCol',      // advance through whole collection
    contGlob : 'contGlob'      // advanve through all collections
};

var showMode = ShowModes.manual;

function isManualShow()   { return showMode === ShowModes.manual; }
function isAutoShow()     { return showMode === ShowModes.auto; }
function isContColShow()  { return showMode === ShowModes.contCol; }
function isContGlobShow() { return showMode === ShowModes.contGlob; }
function isContShow()     { return isContColShow() || isContGlobShow(); }

function setManualShow()   { showMode = ShowModes.manual; }
function setAutoShow()     { showMode = ShowModes.auto; }
function setContColShow()  { showMode = ShowModes.contCol; }
function setContGlobShow() { showMode = ShowModes.contGlob; }

function stopShow() {
    clearMediaAnims();        // cancel all running animations
    if ( isContShow() ) {
        showStatus("Automatischer Bildwechsel beendet");
    }
    setManualShow();
}

function startShow(start) {
    start = start || setAutoShow;
    start();
    if ( isContShow() ) {
        showStatus("Automatischer Bildwechsel gestartet");
    }
    advanceSlideShow();
}

function startStopShow(start) {
    if ( isContShow() ) {
        stopShow();
    }
    else {
        startShow(start);
    }
}

function autoAdvanceShow() {
    const res =
          isContShow()      // show whole collection
          ||
          ( isAutoShow()    // advance slides by looking into metadata
            &&
            autoContShow()
          );
    trc(1, "autoAdvanceShow: " + res);
    return res;
}

// show metadata is set to advance after duration, not on click

function autoContShow() {
    const md = getSlideMeta();
    const c  = md["Show:Continue"] || "onclick";  // default advance is onclick
    trc(1, "autoContShow: " + c);
    return c === "duration";
}

// ----------------------------------------
//
// slide show acceleration

const slideShowDefaultAcceleration = 1.0;
const slideShowDefaultDuration     = 5.0;  // default: 5 sec
var   slideShowAcceleration        = slideShowDefaultAcceleration;

function slideDur() {
    const md = getSlideMeta();
    const d  = md["Show:Duration"];          // time in sec
    let   t  = slideShowDefaultDuration;
    if (d) {
        t = d * 1;         // convert to number
        if ( !t ) { t = slideShowDefaultDuration;}  // no conv: reset to defaoult
    }
    return Math.round(t * slideShowAcceleration * 1000);   // time scaled by acceleration (in msce)
}

// ----------------------------------------
//
// slideshow transition modes

const TransModes = {
    default   : 'default',
    crossfade : 'crossfade',
    fadeoutin : 'fadeoutin',
    cut       : 'cut',
};

function slideTransMode(s) {
    s = s || cs;
    const md = getSlideMeta(s);
    let   tm = md["Show:Transition"] || TransModes.default;

    if ( ! TransModes.hasOwnProperty(tm) ) {
        tm = TransModes.default;
    }
    return tm;
}

function getTrans() {
    let res = {
        dur     : cs.trans.dur,                     // time of slide to be shown without fadein/out
        fadeIn  : cs.trans.fadeIn  || 0,            // fadein from cs
        fadeOut : ls.trans.fadeOut || 0,            // fadeout and mode from ls
        mode    : ls.trans.mode    || TransModes.default,
    };

    if ( res.mode === TransModes.cut ) {            // simulate "cut" with "fadeoutin"
        res.fadeOut = 0;
        res.mode    = TransModes.fadeoutin;
    };

    return res;
}

function slideFadeInDur()  { return slideFadeDur("FadeIn");  }
function slideFadeOutDur() { return slideFadeDur("FadeOut"); }

function slideFadeDur(fadeinout) {
    const md = getSlideMeta();
    const d  = md["Show:" + fadeinout];
    let   t  = defaultTransDur;
    if (d) {
        t = d * 1;  // convert to number
        if ( !t ) { t  = defaultTransDur; }
    }
    return Math.round(t * 1000);   // time in msec
}

// ----------------------------------------
//
// advance slideshow

function nextReqGlobal() {
    return getNavReq("fwrd");
}

function nextReqLocal() {
    return isColSlide()
        ? getChild0Req()
        : getNavReq("next");
}

function advanceSlideShow() {
    trc(1, "advance SlideShow start");
    const req =
          isContGlobShow()
          ? nextReqGlobal()
          : nextReqLocal();

    if (! req) {
        stopShow();
        gotoPar();
    }
    else {
        showNextSlide(req);
    }
}

function resetSpeedSlideShow() {
    slideShowAcceleration = slideShowDefaultAcceleration;
    showDur();
}

function slowDownSlideShow() {
    slideShowAcceleration = slideShowAcceleration * 1.2;
    showDur();
}

function speedUpSlideShow() {
    slideShowAcceleration = Math.max(slideShowAcceleration / 1.2, 0.2);
    showDur();
}

function showDur() {
    const s =  Math.round(slideShowAcceleration * slideShowDefaultDuration * 10) / 10;
    showStatus('Automatischer Bildwechsel nach ' + s + " sec.");
}

function showVersion() {
    showStatus(version);
}

function toggleDefaultAlg() {
    defaultAlg = ( defaultAlg === "fitsinto"
                   ? "fill"
                   : "fitsinto"
                 );
}

// ----------------------------------------
// status line

function showStatus(msg, dur) {
    if ( statusDescr.enabled ) {
        dur = dur || statusDescr.dur;
        statusDescr.visible = false;
        const se = clearDomElem(statusDescr.id);
        se.innerHTML = msg;

        function getStatus() { return se; }

        runC(compl([ toggleOverlay(statusDescr),
                     animElement(getStatus,
                                 noAnim8(dur),
                                 setAnim(statusDescr.id)),
                     toggleOverlay(statusDescr),
                   ])
            );
    }
}

function toggleOverlay(o) {

    function doit(k) {
        const ie = getElem(o.id);
        clearAnim(o.id);

        function getO() { return ie; }

        // fadein
        var a    = fadein8(500);
        var css1 = { display: "block", opacity: 0.0 };
        var css2 = {};

        if ( o.visible ) {
            // fadeout
            var a = fadeout8(500);
            var css1 = {};
            var css2 = { display: "none", opacity: '' };
        }

        o.visible = ! o.visible;

        animElement1(getO, a, css1, css2, setAnim(o.id))(k);
    }
    return doit;
}

function hasAudioControl() {
    return getElem(audioDescr.id).children.length !== 0;
}
function toggleInfo()  { runC(toggleOverlay(infoDescr));  }
function toggleHelp()  { runC(toggleOverlay(helpDescr));  }
function toggleAudio() { runC(toggleOverlay(audioDescr)); }

// ----------------------------------------
//
// animation construction

function fade(start, end, ease) {

    function doit(dur) {
        const kf = [
            { opacity: start },
            { opacity: end   },
        ];
        const t = {
            duration:   dur,
            iterations: 1,
            easing:     ease || 'ease-in-out',
            fill:       "forwards",
        };
        return { keyFrames: kf,
                 timing:    t,
               };
    }
    return doit;
}

const fadein1  = fade(0, 1, "ease-out");
const fadeout1 = fade(1, 0, "ease-in");

const fadein   = fade(0, 1);
const fadeout  = fade(1, 0);

const fadein8  = fade(0, 0.7);       // show an overlay with opacity 0.8
const fadeout8 = fade(0.7, 0);
const noAnim8  = fade(0.7, 0.7);

const noAnim   = fade(1, 1);         // a simple delay: do nothing for a while

function scale(s1, s2) {
    function doit(dur) {
        const kf = [
            { transform: `scale(${s1})` },
            { transform: `scale(${s2})` },
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

// --------------------
//
// animation for resizing from (geo0, off0) to (geo1, off1)

function moveAndScale(tr) {
    geo0 = pxGeo(tr.start.geo);
    off0 = pxGeo(tr.start.off);
    geo1 = pxGeo(tr.finish.geo);
    off1 = pxGeo(tr.finish.off);

    function doit(dur) {
        const kf = [
            { width  : geo0.w,
              height : geo0.h,
              left   : off0.w,
              top    : off0.h,
            },
            { width  : geo1.w,
              height : geo1.h,
              left   : off1.w,
              top    : off1.h,
            },
        ];
        const t = {
            duration:   dur,
            easing:     'ease-in-out',
            delay:      0,
            iterations: 1,
            fill:      "forwards",
        };
        return { keyFrames: kf,
                 timing:    t,
               };

    }
    return doit;
}

// --------------------

function panorama(tr) {

    function doit(dur) {
        const a = moveAndScale(tr)(dur);
        a.timing.delay = 500;
        return a;
    }
    return doit;
}

// --------------------

// run an animation a on element e and continue with k

function anim(a, storeA) {

    function doit(e, k) {
        trc(1, `anim start: id=${e.id}: a=${JSON.stringify(a)}`);

        var animation;

        function cleanupAnim() {
            trc(1, `anim end: id=${e.id} state=${animation.playState}`);

            // Commit animation state to style attribute
            try {
                animation.commitStyles();
            } catch (err) {
                trc(1, "animation.commitStyles: " + err);
            }

            // Cancel the animation
            animation.cancel();

            // remove anim object from ls/cs
            storeA(null);
        }

        function k1() {
            cleanupAnim();
            k();
        }

        // animation created, installed and returned
        trc(1, "start anim");
        animation = e.animate(a.keyFrames, a.timing);
        trc(1, "anim started");

        // save anim obj in ls/cs
        storeA(animation);

        // set finished promise
        animation
            .finished
            .then(k1, cleanupAnim);
    };
    return doit;
}

// ----------------------------------------

function thisSlideWith(resizeAlg, zoomPos) {
    resizeAlg = checkResizeAlg(resizeAlg);

    const geo =
          ( resizeAlg === "zoom"
            ||
            resizeAlg === "tinyimg"
          )
          ? readGeo("org")
          : bestFitToScreenGeo();
    gotoUrl(jsonReqToUrl1(cs.slideReq, geo), resizeAlg, zoomPos);
}

function gotoUrl(url, resizeAlg, zoomPos) {
    runC(comp(gotoSlide(url, resizeAlg, zoomPos),
              playSlideShow()
             )
        );
}

function playSlideShow() {

    function doit(k) {
        trc(1, "playSlideShow: start");

        function advance() {
            trc(1,"playSlideShow: advance called");
            if ( autoAdvanceShow() ) {
                trc(1, "playSlideShow: advance");
                advanceSlideShow();   // this is a global goto, no continueation is called
            } else {
                k();
            }
        }

        if ( autoAdvanceShow() ) {
            const a = noAnim(cs.trans.dur);   // noop animation for simulating delay
            animElement2( () => getElem(cs.imgId),
                          a,
                          (e) => {},
                          (e) => {},
                          setShowAnim
                        )(advance);
        } else {
            k();
        }
    }
    return doit;
}

// ----------------------------------------
// cs: current slide
// ls: last slide

var cs = { url         : "",
           page        : {},
           imgId       : img1Id,
           slideType   : "",
           slideReq    : {},
           resizeAlg   : "",
           screenGeo   : {},
           zoomPos     : {},
           zoomDur     : 1000,
           trans       : { dur     : 0,
                           fadeIn  : 0,
                           fadeOut : 0,
                           mode    : "default",
                         },
           anim        : { fadeIn  : null,   // the fadeIn  anim object
                           fadeOut : null,   //  "  fadeOut   "    "
                           show    : null,   //  "  show      "    "
                         },
         };

var ls = { };

// animation store, used to cancel animations when stopping show

var animations = {
    fadeIn  : null,   // the fadeIn  anim object
    fadeOut : null,   //  "  fadeOut   "    "
    show    : null,   //  "  show      "    "
    info    : null,
    help    : null,
    status  : null,
};

// animation object setter

const setFadeInAnim  = setAnim("fadeIn");
const setFadeOutAnim = setAnim("fadeOut");
const setShowAnim    = setAnim("show");

function setAnim(sel) {
    return (anim) => {
        trc(1, "setAnim: animations[" + sel + "] = " + anim);
        animations[sel] = anim;
    };
}

function clearAnim(sel) {
    const a = animations[sel];
    if ( a ) {
        trc(1, "clearAnims: cancel animations." + sel);
        a.cancel();  // trigger error calback in finished promise
    }
}

const mediaAnims = ["fadeIn", "fadeOut", "show"];
function clearMediaAnims() { clearAnims(mediaAnims); }

function clearAnims(anims) { anims.map(clearAnim); }

var defaultAlg    = "fitsinto";
var defaultCutoff = 0.17;

function gotoSlide(url, resizeAlg, zoomPos) {

    function doit(k) {

        function jsonPage(page) {
            // trc(1, "jsonSlide: " + JSON.stringify(page));

            // save old slide context
            ls = cs;

            // build new slide context
            const req  = page.imgReq || page.colDescr.eReq;

            resizeAlg = checkResizeAlg(resizeAlg);

            cs = { url         : url,
                   page        : page,
                   imgId       : nextimg[ls.imgId],
                   slideType   : "",
                   resizeAlg   : checkResizeAlg(resizeAlg),  // set a decent default resize alg
                   slideType   : req.rType,
                   slideReq    : req,
                   screenGeo   : screenGeo(),
                 };
            cs.trans = {
                dur     : slideDur(),
                fadeIn  : slideFadeInDur(),
                fadeOut : slideFadeOutDur(),
                mode    : slideTransMode(),
            };
            cs.screen = mkRect(cs.screenGeo, nullGeo);

            if ( isMediaSlide() ) {
                cs.orgGeo    = readGeo(cs.page.oirGeo[0]);         // size of original media

                // normalize resize algs

                cs.isLargeImg = ltGeo(cs.screenGeo, cs.orgGeo);    // org image covers whole screen
                if ( ! cs.isLargeImg
                     &&
                     ( cs.resizeAlg === "zoom"
                       ||
                       cs.resizeAlg === "fill"
                     )
                   ) {
                    cs.resizeAlg = "default";                      // too small for zoom or fill
                }

                cs.isPanoImg = isPano(cs.orgGeo);
                if ( ! cs.isPanoImg
                     &&
                     cs.resizeAlg === "panorama"
                   ) {
                    cs.resizeAlg = "default";
                }

                cs.isTinyImg = ltGeo(cs.orgGeo, cs.screenGeo);
                if ( cs.isTinyImg
                     &&
                     cs.resizeAlg === "default"
                   ) {
                    cs.resizeAlg = "tinyimg";
                }

                if ( ! cs.isTinyImg
                     &&
                     cs.resizeAlg === "magnify"
                   ) {
                    cs.resizeAlg = "default";
                }

                cs.sameAsLast = sameAsLastSlide();

                cs.zoomPos   = zoomPos   || halfGeo(oneGeo); // default relative zoom position
                cs.zoomDur   = 2000;                         // default zoom duration 4 sec

                cs.fill      = fillScreen   (cs.orgGeo);
                cs.fitsinto  = fitIntoScreen(cs.orgGeo);
                cs.tinyimg   = orgOnScreen  (cs.orgGeo);
                cs.magnify   = cs.fitsinto;
                cs.zoom      = zoomOnScreen (cs.orgGeo);

                if ( cs.isPanoImg ) {
                    cs.panoramaS = panoStart (cs.orgGeo);
                    cs.panoramaF = panoFinish(cs.orgGeo);
                    cs.panorama  = cs.panoramaE;
                }

                cs.fillCutoff = cutoffArea(cs.fill.geo, cs.screenGeo);
                trc(1, "jsonSlide: fillCutoff=" + cs.fillCutoff);

                if ( cs.resizeAlg === "default" ) {
                    if ( defaultAlg === "fill"
                         &&
                         cs.fillCutoff <= defaultCutoff
                       ) {
                        cs.resizeAlg = "fill";
                    }
                    else {
                        cs.resizeAlg = "fitsinto";
                    }
                }

                cs.rect  = cs[cs.resizeAlg];
                if ( cs.sameAsLast ) {
                    cs.moveScaleTrans = mkTrans(ls.rect, cs.rect);
                }

                trc(1, "jsonSlide: slide context  initialized");

            }

            // create a new empty slide container
            clearDomElem(cs.imgId);

            switchSlide()(k);
        }
        getJsonPage(url, jsonPage, showErr);
    }
    return doit;
}

function switchSlide() {
    function doit(k) {
        trc(1, "switchSlide");

        buildInfo();
        buildAudio();
        setPageTitle();

        if ( isColSlide() ) {
            buildCollectionSlide()(k);
        }
        else if ( isMovieSlide() ) {
            buildMovieSlide()(k);
        }
        else if ( isBlogSlide() ) {
            buildBlogSlide()(k);
        }
        else if ( isImgSlide() ) {
            loadImgCache()(k);
        }
        else {
            trc(1, "switchSlide stop continuation: illegal slide type " + cs.slideType);
        }
    }
    return doit;
}

function buildMovieSlide() {
    function doit(k) {
        // url of the original media file, not the coll entry url
        // due to server streaming api for videos

        const url    = toMediaUrl(cs.page.img);
        const style  = cssRect(cs.fitsinto);

        // get slide container and set geomety attibutes
        const e = getElem(cs.imgId);
        setCSS(e, style);

        if (cs.slideType === "movie" ) {

            const cls = "movie gif " + cs.resizeAlg;
            const v   = newElem("video", mkImgId(cs.imgId), {}, cls);

            v.width   = cs.fitsinto.geo.w;
            v.heigth  = cs.fitsinto.geo.h;

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
        else if ( cs.slideType === "gif" ) {

            const css = cssSize(cs.fitsinto.geo);
            const cls = "movie gif " + cs.resizeAlg;
            const v2  = newImgElem(cs.imgId, css, cls);

            v2.src    = url;
            e.appendChild(v2);
        }
        animTransMedia(getTrans())(k);
    }
    return doit;
}

function buildBlogSlide() {

    function doit(k) {
        const txt    = getSlideBlog();
        const style  = cssRect(cs.screen);
        const style2 = cssGeo(cs.screenGeo, { overflow: "auto" });

        trc(1, "buildBlogPage: " + txt);

        // get slide container and set geomety attibutes
        const e = getElem(cs.imgId);
        setCSS(e, style);

        // build blog contents div
        const b  = newElem("div", cs.imgId + "-blog", style2, "blog");
        b.innerHTML = txt;
        e.appendChild(b);

        animTransBlog(getTrans())(k);
    }
    return doit;
}

function buildCollectionSlide() {
    function doit(k) {
        const colDescr = cs.page.colDescr;
        const colReq   = colDescr.eReq;
        const colMeta  = colDescr.eMeta;
        const colBlog  = getSlideBlog();
        const navIcons = cs.page.navIcons;
        const c1Icon   = cs.page.c1Icon;
        const colIcons = cs.page.contIcons;
        const iconReq  = { rType:    "icon",
                           rPathPos: colReq.rPathPos
                         };
        const style    = cssRect(cs.screen, { overflow: "auto" });

        // get element, clear contents and set style attributes
        const e = clearDomElem(cs.imgId);
        setCSS(e, style);

        const c = buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons, colBlog);
        e.appendChild(c);
        allImagesLoaded(c)(() => {
            animTransCollection(getTrans())(k);
        });
    }
    return doit;
}

function loadImgCache() {

    function doit(k) {
        const imgReq  = cs.page.imgReq;

        let reqGeo    = bestFitToScreenGeo();

        if ( cs.resizeAlg === "zoom"
             ||
             cs.resizeAlg === "tinyimg"
             ||
             cs.resizeAlg === "magnify"
           ) {
            reqGeo = readGeo("org");
        }
        if ( cs.resizeAlg === "panorama") {
            reqGeo = roundGeo(cs.panoramaS.geo);
        }

        cs.urlImg = imgReqToUrl(imgReq, reqGeo);

        trc(1, "loadImgCache: urlImg=" + cs.urlImg);

        if ( cs.urlImg === (ls.urlImg || "") ) {
            trc(1, `loadImgCache: already cached ${cs.imgId}` );
            switchResizeImg()(k);
        }
        else {
            function k1() {
                trc(1, `onload loadImgCache: ${cs.imgId}` );

                switchResizeImg()(k);
            };

            var picCache = new Image();
            picCache.onload = k1;
            picCache.src    = cs.urlImg;   // the .onload handler is triggered here
        }
    }
    return doit;
}

// hack for waiting until all images are loaded

function allImagesLoaded(e) {
    function doit(k) {
        const images        = e.getElementsByTagName("img");
        const noOfImages    = images.length;
        var   alreadyLoaded = 0;
        trc(1, "allImagesLoaded: # images found: " + noOfImages);

        for (const img of images) {
            const src   = img.getAttribute("src");
            const img1  = new Image();  // create dummy image and set onload callback
            img1.onload = imgLoaded;
            img1.src    = src;
        }

        function imgLoaded() {
            alreadyLoaded++;
            trc(1, "imgLoaded: " + alreadyLoaded);

            if (alreadyLoaded === noOfImages) {
                trc(1, "allImagesLoaded: terminated");
                k();
            }
        };

    }
    return doit;
}

function switchResizeImg() {

    function doit(k) {
        if ( cs.resizeAlg === "zoom") {
            // addZoomImg()(k);
            addZoomImg()(k);
        }
        else if ( cs.resizeAlg === "panorama" ) {
            addPanoramaImg()(k);
        }
        else if ( cs.resizeAlg === "fill" ) {
            addFillImg()(k);
        }
        else if ( cs.resizeAlg === "fitsinto" ) {
            addFitIntoImg()(k);
        }
        else if ( cs.resizeAlg === "tinyimg" ) {
            addTinyImg()(k);
        }
        else if ( cs.resizeAlg === "magnify" ) {
            addMagnifiedImg()(k);
        }
        else {
            trc(1, "switchSlide stop continuation: illegal resizeAlg " + cs.resizeAlg);
        }
    }
    return doit;
}

function addMoveScale(addHandler) {

    function doit(k) {
        const style  = {overflow: "hidden"};
        const style2 = cssRect( cs.sameAsLast
                                ? cs.moveScaleTrans.start
                                : cs.rect,
                                {overflow: "hidden"}
                              );

        addImgToDom(style, style2, addHandler);

        if ( cs.sameAsLast ) {
            const d = zoomDist( cs.moveScaleTrans.start,
                                cs.moveScaleTrans.finish
                              );
            const t = Math.sqrt(d) * cs.zoomDur;
            const a = moveAndScale(cs.moveScaleTrans)(t);
            trc(1, `addMoveScale: anim=${JSON.stringify(a)}`);
            animTransZoom(a)(k);
        }
        else {
            animTransMedia(getTrans())(k); // img displayed the 1. time
        }
    };
    return doit;
}

function addResize(resizeAlg) {

    function addHandler(i) {

        function resizeHandler(e) {
            stopShow();

            const pos = mkGeo(e.offsetX, e.offsetY);
            const off = screenOffsetToRelOffset(pos, cs.rect);
            const alg = ( ( e.altKey && cs.resizeAlg === "zoom" )
                          ? "zoom"
                          : resizeAlg
                        );
            trc(1, "resizeHandler: alg =" + resizeAlg + ", offset = " + showGeo(off));
            thisSlideWith(alg, off);
        }

        i.addEventListener("dblclick", resizeHandler);
    }
    return addHandler;
}

function addZoomImg()      { return addMoveScale(addResize("default")); }
function addTinyImg()      { return addMoveScale(addResize("magnify")); }
function addMagnifiedImg() { return addMoveScale(addResize("tinyimg")); }
function addFillImg()      { return addMoveScale(addResize("zoom"));    }
function addFitIntoImg()   { return addMoveScale(addResize("zoom"));    }

function addPanoramaImg() {

    function doit(k) {
        // compute animation duration

        const dist   = subGeo(cs.panoramaS.off, cs.panoramaF.off);
        const move   = Math.max(dist.w, dist.h);
        const dur    = 10 * Math.abs(move);        // 10 * number of pixels to move

        const style  = { overflow: "hidden" };
        let   style2 = cssRect(cs.panoramaS, { position: "absolute" });

        const tr     = mkTrans(cs.panoramaS, cs.panoramaF);
        const a      = panorama(tr)(dur);

        addImgToDom(style, style2, () => {});
        animTransPanorama(a, getTrans())(k);
    }
    return doit;
}

function addImgToDom(style, style2, addHandler) {
    const style1 = cssRect(mkRect(cs.screenGeo, nullGeo), style);

    const e = getElem(cs.imgId);
    setCSS(e, style1);

    const i = newImgElem(cs.imgId, style2, "img " + cs.resizeAlg);
    addHandler(i);
    i.src   = cs.urlImg;

    e.appendChild(i);
}

// --------------------
//
// run an animation to show current slide element

function animElement2( getE,     // element getter fct
                       a,        // animation
                       doS,      // configure elem before anim start
                       doE,      // configure elem after anim has finished
                       storeA    // store the animation obj in cs/ls
                     ) {
    storeA = storeA || ((a1) => {});

    function doit(k) {
        const e = getE();
        if ( e ) {
            doS(e);
            trc(1, "animElement2: " + e.id);

            function k1() {
                doE(e);
                k();
            }

            anim(a, storeA)(e, k1);
        }
        else {
            trc(1, "animElement2: no element found");
            k();
        }
    }
    return doit;
}

function animElement1(getE, a, cssS, cssE, storeA) {
    return animElement2( getE,
                         a,
                         (e) => { setCSS(e, cssS); },
                         (e) => { setCSS(e, cssE); },
                         storeA,
                       );
}

function animElement(getE, a, storeA) {
    return animElement1( getE,
                         a,
                         { display: "block" },
                         {},
                         storeA,
                       );
}

function animCurrent(a) {
    return animElement( () => { return getElem(cs.imgId); },
                        a,
                        setFadeInAnim
                      );
}

function animCurrentImg(a) {
    return animElement( () => { return getElem(mkImgId(cs.imgId)); },
                        a,
                        setFadeInAnim
                      );
}

// run an animation to hide last slide element and cleanup element

function animLast(a) {
    const getE = ()  => { return getElem(ls.imgId) || null; };

    return animElement2( ()  => { return getElem(ls.imgId) || null; },   // get elem of last pic
                         a,
                         (e) => { setCSS(e, { "z-index": -1 }); },       // push it down the render stack
                         (e) => { clearDomElem(getE()); },               // throw it away
                         setFadeOutAnim
                       );
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
//
// transition to a collection (cs.slideType === "json")

function animTransCollection(trans) {

    function doit(k) {
        const fi = fadein (trans.fadeIn);
        const fo = fadeout(trans.fadeOut);
        const mo = trans.mode;
        var   tr;

        if ( mo === "crossfade"
             ||
             ( mo === "default"
               &&
               isColSlide(ls)               // last slide was a collection
             )
           ) {
            tr = transCrossFade(fo, fi);
        }
        else if ( mo === "fadeoutin"
                  ||
                  ( mo === "default"
                    &&
                    isSlide(ls)             // last slide is an img/movie/blog
                  )
                ) {
            tr = transFadeOutIn(fo, fi);
        }
        else {
            tr = animCurrent(fi);         // initial transition
        }
        tr(k);
    }
    return doit;
}

// transition to image or movie

function animTransMedia(trans) {

    function doit(k) {
        const fi  = fadein  (trans.fadeIn);
        const fo  = fadeout (trans.fadeOut);
        const fi1 = fadein1 (trans.fadeIn);
        const fo1 = fadeout1(trans.fadeOut);
        const mo  = trans.mode;
        var   tr;

        if ( mo === "crossfade"
             ||
             ( mo === "default"
               &&
               isMediaSlide(ls)            // default for img -> img: crossfade
             )
           ) {
            if ( similarGeoOnScreen() ) {
                // similar image geometies: smoother crossfade

                trc(1, "animTransMedia: smoother transition");
                tr = transCrossFade(fo1, fi1);
            } else {
                tr = transCrossFade(fo, fi);
            }
        }
        else if ( mo === "fadeoutin" ) {
            tr = transFadeOutIn(fo, fi);
        }
        else { // default
            tr = transCrossFade(fo, fi);
        }
        tr(k);
    }
    return doit;
}

function animFitFill() {
    function doit(k) {
        const tr = mkTrans(cs.fitsinto, cs.fill);
        const a  = moveAndScale(tr)(cs.zoomDur);
        animCurrentImg(a)(k);
    }
    return doit;
}

function animTransZoom(zoomAnim) {

    function doit(k) {
        const tr1 = transCrossFade(noAnim(600), fadein(500));
        const tr2 = animCurrentImg(zoomAnim);                  // zoom transition
        comp(tr1, tr2)(k);
    }
    return doit;
}

function animTransPanorama(panoAnim, trans) {

    function doit(k) {
        const fi  = fadein  (trans.fadeIn);
        const fo  = fadeout (trans.fadeIn);

        const tr1 = transCrossFade(fo, fi);
        const tr2 = animCurrentImg(panoAnim);
        comp(tr1, tr2)(k);
    }
    return doit;
}

// transition to blog entry

function animTransBlog(trans) {

    function doit(k) {
        const fi  = fadein  (trans.fadeIn);
        const fo  = fadeout (trans.fadeOut);
        const mo  = trans.mode;
        var   tr;

        if ( mo === "crossfade" ) {
            tr = transCrossFade(fo, fi);
        }
        else {
            tr = transFadeOutIn(fo, fi);  // col/blog -> img
        }

        tr(k);
    }
    return doit;
}

// --------------------
//
// slide predicates

function isColSlide(s) {
    s = s || cs;
    const t = s.slideType || "";
    return t === "json";
}

function isMediaSlide(s) {
    s = s || cs;
    const t = s.slideType || "";
    return [ "img",
             "imgfx",
             "icon",
             "iconp",
             "gif",
             "movie",
           ].includes(t);
}

function isImgSlide(s) {
    s = s || cs;
    const t = s.slideType || "";
    return [ "img",
             "imgfx",
             "icon",
             "iconp",
           ].includes(t);
}

function isMovieSlide(s) {
    s = s || cs;
    const t = s.slideType || "";
    return [ "gif",
             "movie",
           ].includes(t);
}

function isBlogSlide(s) {
    s = s || cs;
    const t = s.slideType || "";
    return t === "page";
}

function isSlide(s) {
    s = s || cs;
    const t = s.slideType || "";
    return t != "";
}

function isPanoSlide() {
    if ( isImgSlide() ) {
        const geo = getOrgGeo();
        return isPano(geo);
    }
}

function checkResizeAlg(alg) {
    if ( [ "fill",
           "fitsinto",
           "tinyimg",
           "magnify",
           "panorama",
           "zoom",
           "default"
         ].includes(alg) ) {
        return alg;
    }
    return "default";
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

// ----------------------------------------
//
// from https://stackoverflow.com/questions/3354239/hiding-the-mouse-cursor-when-idle-using-javascript
// 4. answer

var HideCursor = {
    INI: {
        MOUSE_IDLE: 3000
    },
    hideMouse: function() {
        trc(1, "hide mouse cursor");
        $("#imageTab").css('cursor', 'none');
        $("#imageTab").on("mousemove", HideCursor.waitThenHideMouse);
    },
    waitThenHideMouse: function() {
        $("#imageTab").css('cursor', 'default');
        $("#imageTab").off("mousemove", HideCursor.waitThenHideMouse);
        setTimeout(HideCursor.hideMouse, HideCursor.INI.MOUSE_IDLE);
    },
    showMouse: function() {
        $("#imageTab").off("mousemove", HideCursor.waitThenHideMouse);
        $("#imageTab").css('cursor', 'default');
    },
};

// ----------------------------------------
