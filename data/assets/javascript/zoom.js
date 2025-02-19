/* zoom.js single page slideshow with zoom */

function trc (t, Text) {
    if ( t > 0 ) {
        console.log(Text);
    }
}

/* ---------------------------------------- */
/* id's */

const version      = "PhotoShow (zoom.html) 0.5.11.0 2025-02-11";

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

const videoAttrs  = { controls: "",   // default video attributes
                      autoplay: null,
                      muted:    null,
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

function maxGeo(g1, g2) {
    if (typeof g2 === "number") {
        return maxGeo(g1, { w : g2, h : g2} )
    } else {
        return { w : Math.max(g1.w, g2.w),
                 h : Math.max(g1.h, g2.h)
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

function absGeo(g) {
    return { w : Math.abs(g.w),
             h : Math.abs(g.h)
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

    res.left     = opx.w;
    res.top      = opx.h;
    res.position = "absolute";
    res.overflow = ov || "auto";

    return res;
}

function fitsInto(g1, g2) {
    return g1.w <= g2.w && g1.h <= g2.h;
}

function lessThan(g1, g2) {
    return g1.w < g2.w && g1.h < g2.h;
}

function similarGeo(g1, g2, d) {
    d = d || 10;
    const g = absGeo(subGeo(g1, g2));
    return g.w <= d && g.h <= d;
}

function isPano(g) {
    const ar = aspectRatio(g);
    return fitsInto(cs.screenGeo, g)
        && ( ar >= 2 || ar <= 0.5 );
}

function isTiny(g) {
    return lessThan(g, cs.screenGeo);
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

function toPx(obj) {
    const res = {};
    for (k in obj) {
        res[k] = obj[k] + "px";
    }
    return res;
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

function resizeToHeight(s, d) {
    return resizeGeo(s, {w: infiniteWidth, h: d.h});
}

function resizeToWidth(s, d) {
    return resizeGeo(s, {w: d.w, h: infiniteWidth});
}

function resizeToPano(g) {
    return maxGeo( resizeToHeight(g, cs.screenGeo),
                   resizeToWidth (g, cs.screenGeo)
                 );
}

function resizeToScreen(g) {
    return resize(g, cs.screenGeo);
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
        setCSS(imgTab, {width : w1 + "px", height : h1 + "px"});
    }
    return theScreenGeo;
}

function placeOnScreen(geo) {
    return halfGeo(subGeo(cs.screenGeo, geo));
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
        if (g.w >= s.w && g.h >= s.h)
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
    if (w <= 1400)               // cannon beamer geo 1400x1050
        return readGeo("140x105");
    if (w <= 2560)
        return readGeo("160x120"); // iMac 27''

    return readGeo("256x144");     // eizo 4k display
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
    if ( typeof e === "string" ) {
        return clearDomElem(getElem(e));
    } else {
        const id = e.id;
        const tn = e.tagName;
        const p  = e.parentElement;
        cancelAnims(e);
        e.remove();

        const ne = newElem(tn, id, {}, "hiddenImage");
        p.insertBefore(ne, p.children[0]);
        return ne;
    }
}

function cancelAnims(e) {
    const as = e.getAnimations({ subtree: true });
    as.forEach((a) => {
        trc(1, "animation canceled");
        a.cancel();
    });
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
          + (defaultTransDur === 0
             ? "aus"
             : defaultTransDur + " sec."
            );
    showStatus(msg);
}

/* ---------------------------------------- */
/* initialization */

function initShow() {
    const g = pxGeo(screenGeo());
    console.log("initShow: screen geo=" + showGeo(g));

    // initHandlers(); // TODO cleanup

    setCSS(imgTab, {width : g.w, height : g.h});

    clearDomElem(img1Id);
    clearDomElem(img2Id);

    showPath(pathCollections());
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

    const numCols = Math.max(div(cs.screenGeo.w - 2 * padding, iconGeoB.w + gap));

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

function getOrgGeo() {
    if ( isMediaSlide() ) {
        return readGeo(cs.page.oirGeo[0]);
    }
    return null;
}

function getSlideMeta() {
    if ( isColSlide() ) {
        return cs.page.colDescr.eMeta;
    }
    return cs.page.img[2];
}

function getSlideBlog() {
    const md = getSlideMeta();   // new blog access: blog is part of metadata
    const bt = md["Descr:Blog"];
    if (bt) {
        return bt;
    } // else {return "";}         // TODO cleanup, when server has been updated
    return cs.page.blogCont;      // old blog access: blog is in blogCont field;
}

function getNavReq(nav) {
    if ( isColSlide() ) {
        return cs.page.navIcons[nav].eReq;
    }
    return cs.page.imgNavRefs[nav];
}

function getChild0Req() {
    if ( isColSlide() ) {
        const req = cs.page.contIcons[0].eReq;
        if ( req ) { return req; }
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
    const pPos  = cs.slideReq.pPos;
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

// ------------------------------------------------------------
//
// event handlers for image animations

function toggleFullSlide() {
    if ( isImgSlide() ) {
        if ( cs.resizeAlg != "fullsize" ) {
            thisSlideWith("fullsize");
        } else {
            thisSlideWith("default");
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

    if ( isKey(e, 100, "d")
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
        toggleFullSlide();
        return false;
    }
    if ( isKey(e, 97, "a") ) {
        stopSlideShow();
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

function buildInfo() {
    const md = getSlideMeta();
    const it = getElem(infoDescr.tab);
    buildMetaInfo(it, md);
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
// slideshow stuff

var slideShow      = false;
var slideShowTimer;
var slideShowType  = "";

const slideShowDefaultSpeed = 5000;  // default: 5 sec in milliseconds
var   slideShowSpeed = slideShowDefaultSpeed;  // default: 5 sec in milliseconds

function slideDur() {
    const md = getSlideMeta();
    const d  = md["Descr:Duration"];
    let   t  = 1; // seconds
    if (d) {
        t = d * 1;         // convert to number
        if (!t) { t = 1;}  // no conv: reset to defaoult
    }
    return t * slideShowSpeed;
}

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
          (slideShowType === "allColls")
          ? nextReqGlobal()
          : nextReqLocal();

    if (! req) {
        stopSlideShow();
        gotoPar();
    }
    else {
        // install timer
        const ms = slideDur();
        slideShowTimer = setTimeout(advanceSlideShow, ms);
        trc(1, "advanceSlideShow timer set msec: " + ms + " (" + slideShowType + ")");

        // show slide
        showNextSlide(req);
    }
}

function stopSlideShow() {
    trc(1, "stopSlideShow");
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
    trc(1, "startSlideShow");
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

function showVersion() {
    showStatus(version);
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
                     animElement(getStatus, noAnim8(dur)),
                     toggleOverlay(statusDescr),
                   ])
            );
    }
}

function toggleOverlay(o) {

    function doit(k) {
        const ie = getElem(o.id);
        cancelAnims(ie);

        function getO() { return ie; }

        // fadein
        var a    = fadein8(500);
        var css1 = { display: "block", opacity: 0.0 }
        var css2 = { }

        if ( o.visible ) {
            // fadeout
            var a = fadeout8(500);
            var css1 = { }
            var css2 = { display: "none", opacity: '' }
        }

        o.visible = ! o.visible;

        animElement1(getO, a, css1, css2)(k);

    }
    return doit;
}

function toggleInfo() { runC(toggleOverlay(infoDescr)); }
function toggleHelp() { runC(toggleOverlay(helpDescr)); }

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

function zoomIn1(geo0, off0, geo1, off1) {
    geo0 = pxGeo(geo0);
    off0 = pxGeo(off0);
    geo1 = pxGeo(geo1);
    off1 = pxGeo(off1);

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

function zoomOut1(geo0, off0, geo1, off1) {
    return zoomIn1(geo1, off1, geo0, off0);
}

// --------------------

function panorama(dr, dr1, offset) {
    // dr:  left or bottom      // horizontal / vertical pano
    // dr1: top  or left

    function doit(dur) {
        var kf0 = { offset: 0.0 };   kf0[dr] =         "0px";
        var kf1 = { offset: 1.0 };   kf1[dr] = offset + "px";

        const kf = [kf0, kf1];

        const t = {
            duration:   dur,
            easing:     'ease-in-out',
            delay:      1000,
            direction:  "alternate",
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

// run an animation a on element e and continue with k

function anim(a) {

    function doit(e, k) {
        trc(1, `anim start: id=${e.id}: a=${JSON.stringify(a)}`);

        var animation;

        function k1() {
            trc(1, `anim end: id=${e.id}`);

            // Commit animation state to style attribute
            animation.commitStyles();

            // Cancel the animation
            animation.cancel();

            // trc(1, "continuation called");
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

// ----------------------------------------

function thisSlideWith(resizeAlg, zoomPos) {
    resizeAlg = checkResizeAlg(resizeAlg);

    const geo =
          ( resizeAlg === "fullscreen"
            ||
            resizeAlg === "zoom"
            ||
            resizeAlg === "tinyimg"
          )
          ? readGeo("org")
          : bestFitToScreenGeo();
    gotoUrl(jsonReqToUrl1(cs.slideReq, geo), resizeAlg, zoomPos);
}

function gotoUrl(url, resizeAlg, zoomPos) {
    runC(gotoSlide(url, resizeAlg, zoomPos));
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
           transDur    : 0,
           screenGeo   : {},
           zoomPos     : {},
           zoomPos     : {},
           zoomDur     : 2000,
           magDur      : 1000,
         };

var ls = {};

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
                   transDur    : defaultTransDur * 1000,     // transition dur in msec
                   slideType   : req.rType,
                   slideReq    : req,
                   screenGeo   : screenGeo(),
                 };

            if ( isMediaSlide() ) {
                cs.orgGeo    = readGeo(cs.page.oirGeo[0]);              // size of original media

                cs.reqGeo    = readGeo(cs.page.oirGeo[2]);
                if ( isOrgGeo(readGeo(cs.page.oirGeo[1])) ) {      // size scaled down copy to fill the screen
                    cs.reqGeo = cs.orgGeo;                         // hack: server delivers wrong geo
                }

                // normalize resize algs

                cs.isLargeImg = lessThan(cs.screenGeo, cs.orgGeo);
                if ( ! cs.isLargeImg
                     &&
                     ( cs.resizeAlg === "fullsize"
                       ||
                       cs.resizeAlg === "zoom"
                     )
                   ) {
                    cs.resizeAlg = "default";
                }

                cs.isPanoImg = isPano(cs.orgGeo);
                if ( ! cs.isPanoImg
                     &&
                     cs.resizeAlg === "panorama"
                   ) {
                    cs.resizeAlg = "default";
                }

                cs.isTinyImg = lessThan(cs.orgGeo, cs.screenGeo);
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

                if ( cs.isPanoImg ) {
                    cs.panoGeo = resizeToPano(cs.orgGeo);
                }

                // last rule
                if ( cs.resizeAlg === "default" ) {
                    cs.resizeAlg = "downsize";
                }

                cs.fitGeo    = resizeGeo(cs.orgGeo, cs.screenGeo); // size scaled down to fit into screen

                cs.zoomPos   = zoomPos   || halfGeo(cs.screenGeo);      // default zoom position
                cs.zoomDur   = 4000;                                    // default zoom duration 4 sec

                cs.magDur    = 1000;                                    // mduration of magnifying tiny img

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

        const offset = pxGeo(placeOnScreen(cs.fitGeo));
        const g      = pxGeo(cs.fitGeo);
        const style  = { width  : g.w,
                         height : g.h,
                         left   : offset.w,
                         top    : offset.h
                       };

        // get slide container and set geomety attibutes
        const e = getElem(cs.imgId);
        setCSS(e, style);

        if (cs.slideType === "movie" ) {

            const cls = "movie gif " + cs.resizeAlg;
            const v   = newElem("video", mkImgId(cs.imgId), {}, cls);

            v.width   = cs.fitGeo.w;
            v.heigth  = cs.fitGeo.h;

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

            const css = { width:  g.w, height: g.h };
            const cls = "movie gif " + cs.resizeAlg;
            const v2  = newImgElem(cs.imgId, css, cls);

            v2.src    = url;
            e.appendChild(v2);
        }
        animTransMedia(cs.transDur)(k);
    }
    return doit;
}

function buildBlogSlide() {

    function doit(k) {
        const geo  = pxGeo(cs.screenGeo);
        const txt  = getSlideBlog();

        trc(1, "buildBlogPage: " + txt);

        // get slide container and set geomety attibutes
        const e = getElem(cs.imgId);
        setCSS(e, { width:  geo.w,
                    height: geo.h,
                    top:    "0px",
                    left:   "0px"
                  });

        // build blog contents div
        const b  = newElem("div", cs.imgId + "-blog",
                           { width:    geo.w,
                             height:   geo.h,
                             overflow: "auto"
                           },
                           "blog"
                          );
        b.innerHTML = txt;
        e.appendChild(b);

        animTransBlog(cs.transDur)(k);
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
        const iconReq  = { rType: "icon",
                           rPathPos: colReq.rPathPos
                         };
        const g        = pxGeo(cs.screenGeo);

        // get element, clear contents and set style attributes
        const e = clearDomElem(cs.imgId);
        setCSS(e, { width:    g.w,
                    height:   g.h,
                    left:     "0px",
                    top:      "0px",
                    overflow: "auto"
                  });

        e.appendChild(buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons, colBlog));

        animTransCollection(cs.transDur)(k);
    }
    return doit;
}

function loadImgCache() {

    function doit(k) {
        const imgReq  = cs.page.imgReq;

        let reqGeo    = bestFitToScreenGeo();

        if ( cs.resizeAlg === "fullsize"
             ||
             cs.resizeAlg === "zoom"
             ||
             cs.resizeAlg === "tinyimg"
             ||
             cs.resizeAlg === "magnify"
           ) {
            reqGeo = readGeo("org");
        }
        if ( cs.resizeAlg === "panorama") {
            reqGeo = cs.panoGeo;
        }

        cs.urlImg = imgReqToUrl(imgReq, reqGeo);

        trc(1, "loadImgCache: urlImg=" + cs.urlImg +
            ", reqGeo=" + showGeo(reqGeo)
           );

        function k1() {
            trc(1, `onload loadImgCache: ${cs.imgId}` );

            switchResizeImg()(k);
        };

        var picCache = new Image();
        picCache.onload = k1;
        picCache.src    = cs.urlImg;   // the .onload handler is triggered here
    }
    return doit;
}

function switchResizeImg() {

    function doit(k) {
        if ( cs.resizeAlg === "zoom") {
            addZoomImg()(k);
        }
        else if ( cs.resizeAlg === "panorama" ) {
            addPanoramaImg()(k);
        }
        else if ( cs.resizeAlg === "fullsize" ) {
            addFullImg()(k);
        }
        else if ( cs.resizeAlg === "downsize" ) {
            addZoomableImg()(k);
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

function addPanoramaImg() {

    function doit(k) {
        const geo    = cs.panoGeo;
        const isH    = isHorizontal(geo);
        const offset = isH ? cs.screenGeo.w - geo.w : cs.screenGeo.h - geo.h;
        const dr     = isH ? "left" : "bottom";
        const dr1    = isH ? "top"  : "left";

        const ar     = aspectRatio(geo);
        const dur    = 10 * Math.abs(offset); // 10 * number of pixels to move

        const style  = styleGeo(cs.screenGeo, nullGeo, "hidden");
        let   style2 = { position: "absolute" };
        style2[dr]   = "0px";
        style2[dr1]  = "0px";

        const a      = panorama(dr, dr1, offset)(dur);

        addImgToDom(style, style2, () => {});
        animTransPanorama(a, cs.transDur)(k);
    }
    return doit;
}

function addZoomImg() {

    function doit(k) {
        const geo        = cs.orgGeo;
        const imgGeo     = cs.fitGeo;
        const offset     = placeOnScreen(imgGeo);
        const style      = styleGeo(cs.screenGeo, nullGeo, "hidden");

        const viewCenter = halfGeo(imgGeo);
        const viewScale  = divGeo(imgGeo, geo);

        const orgOff     = placeOnScreen(geo);
        const orgScale   = oneGeo;

        const clickDisp  = subGeo(viewCenter, cs.zoomPos);
        const clickScale = divGeo(geo, imgGeo);
        const clickOff   = addGeo(orgOff, mulGeo(clickDisp, clickScale));

        // const style2     = styleGeo(imgGeo, clickOff, "hidden");
        const style2     = styleGeo(imgGeo, offset, "hidden");
        style2.position  = "absolute";

        trc(1, `addZoomImg: ${cs.urlImg}, ${showGeo(geo)}, ${showGeo(cs.zoomPos)}`);

        const a =  zoomIn1(imgGeo, offset, geo, clickOff)(cs.zoomDur);

        trc(1, `addZoomImg: anim=${JSON.stringify(a)}`);

        addImgToDom(style, style2, () => {});

        animTransZoom(a)(k);
    };
    return doit;
}

function addTinyMagImg(geo0, geo, resizeAlg) {

    function doit(k) {
        const toggle  = cs.urlImg === ls.urlImg;

        const offset0 = placeOnScreen(geo0);
        const offset  = placeOnScreen(geo);

        const style   = styleGeo(cs.screenGeo, nullGeo);
        const style2  =
              toggle
              ? styleGeo(geo0, offset0)
              : styleGeo(geo,   offset);

        trc(1, `addTinyMagImg: ${cs.urlImg}, ${toggle}, ${showGeo(geo0)}, ${showGeo(geo)}`);

        function resizeHandler(e) {
            trc(1, "resizeHandler: alg =" + resizeAlg);
            thisSlideWith(resizeAlg);
        }

        function addHandler(i) {
            i.addEventListener("dblclick", resizeHandler);
        }

        addImgToDom(style, style2, addHandler);

        if ( toggle ) {
            const a = zoomIn1(geo0, offset0, geo, offset)(cs.magDur);
            animTransZoom(a)(k);            // toggle tiny/magnified
        }
        else {
            animTransMedia(cs.transDur)(k); // img displayed the 1. time
        }
    }
    return doit;
}

function addTinyImg()      { return addTinyMagImg(cs.fitGeo, cs.orgGeo, "magnify"); }
function addMagnifiedImg() { return addTinyMagImg(cs.orgGeo, cs.fitGeo, "tinyimg"); }

function addFullImg() {

    function doit(k) {
        const geo    = cs.orgGeo;
        const style  = styleGeo(cs.screenGeo, nullGeo);

        // const offset = halfGeo(subGeo(cs.screenGeo, geo)); // nice try
        // const style2 = styleGeo(geo, offset);     // nice try, does not work
        const style2 = styleSize(geo);
        style2.position  = "absolute";

        addImgToDom(style, style2, () => {});

        animTransMedia(cs.transDur)(k);
    }
    return doit;
}

function addZoomableImg() {

    function doit(k) {
        const imgGeo = cs.fitGeo;
        const offset = placeOnScreen(imgGeo);
        const style  = styleGeo(imgGeo, offset, "hidden");
        const style2 = styleSize(imgGeo);

        function initZoom(e) {
            trc(1, "initZoom: start zooming image with id=" + cs.imgId );

            const pos = { w: e.offsetX,
                          h: e.offsetY
                        };
            trc(1, "pos=" + showGeo(pos));
            thisSlideWith("zoom", pos);
        }

        function addHandler(i) {
            i.addEventListener("dblclick", initZoom);
        }

        addImgToDom(style, style2, addHandler);

        animTransMedia(cs.transDur)(k);
    }
    return doit;
}

function addImgToDom(style, style2, addHandler) {

    const e = getElem(cs.imgId);
    setCSS(e, style);

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
                       doE       // configure elem after anim has finished
                     ) {

    function doit(k) {
        const e = getE();
        if ( e ) {
            doS(e);
            trc(1, "animElement2: " + e.id + ", " + JSON.stringify(a));

            function k1() {
                doE(e);
                k();
            }

            anim(a)(e, k1);
        }
        else {
            trc(1, "animElement2: no element found");
            k();
        }
    }
    return doit;
}

function animElement1(getE, a, cssS, cssE) {
    return animElement2(getE, a,
                        (e) => { setCSS(e, cssS); },
                        (e) => { setCSS(e, cssE); }
                       );
}

function animElement(getE, a) {
    return animElement1(getE, a, { display: "block" }, {});
}

function animCurrent(a) {
    return animElement( () => { return getElem(cs.imgId); }, a);
}

function animCurrentImg(a) {
    return animElement( () => { return getElem(mkImgId(cs.imgId)); }, a );
}

// run an animation to hide last slide element and cleanup element

function animLast(a) {
    return animElement2( ()  => { return getElem(ls.imgId) || null; },   // get elem of last pic
                         a,
                         (e) => { setCSS(e, { "z-index": -1 }); },       // push it down the render stack
                         (e) => { clearDomElem(e); }                     // throw it away
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

function animTransCollection(dur) {

    function doit(k) {
        var tr = animCurrent(fadein(dur));    // initial transition

        if ( isColSlide(ls) ) {
            tr = transCrossFade(fadeout(dur), fadein(dur));
        }
        else if ( isSlide(ls) ) {
            tr = transFadeOutIn(fadeout(dur), fadein(dur));
        }
        tr(k);
    }
    return doit;
}

// transition to image or movie

function animTransMedia(dur) {

    function doit(k) {
        // trc(1, "animTransMedia: dur=" + dur);
        var tr = transCrossFade(fadeout(dur), fadein(dur));  // img -> img

        if ( isMediaSlide(ls) ) {
            if ( similarGeoOnScreen() ) {
                // similar image geometies
                // smoother crossfade

                trc(1, "animTransMedia: smoother transition");
                tr = transCrossFade(fadeout1(dur), fadein1(dur));
            }
        }
        else {
            tr = transCrossFade(fadeout(dur), fadein(dur));
        }
        tr(k);
    }
    return doit;
}

function animTransZoom(zoomAnim) {

    function doit(k) {
        const tr1 = transCrossFade(noAnim(500), fadein(500));  // transition to fullsize image slide
        const tr2 = animCurrentImg(zoomAnim);                  // zoom transition
        comp(tr1, tr2)(k);
    }
    return doit;
}

function animTransMagnify(zoomAnim) {

    function doit(k) {
        // const tr1 = transCrossFade(noAnim(500), fadein(500));  // transition to fullsize image slide
        const tr2 = animCurrentImg(zoomAnim);                  // zoom transition
        tr2(k);
        // comp(tr1, tr2)(k);
    }
    return doit;
}

function animTransPanorama(panoAnim, dur) {

    function doit(k) {
        const tr1 = transCrossFade(fadeout(dur), fadein(dur));
        const tr2 = animCurrentImg(panoAnim);
        comp(tr1, tr2)(k);
    }
    return doit;
}

// transition to blog entry

function animTransBlog(dur) {

    function doit(k) {
        const tr = transFadeOutIn(fadeout(dur), fadein(dur));  // col/blog -> img
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
    if ( [ "downsize",
           "fullsize",
           "zoom",
           "tinyimg",
           "magnify",
           "panorama",
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

// --------------------
//
// some simple tests

const c0 = "/docs/json/1600x1200/archive/collections/albums.json";
const c1 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder.json";
const u1 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder/pic-0000.json";
const u2 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder/pic-0001.json";
const u3 = "/docs/json/1600x1200/archive/collections/albums/EinPaarBilder/pic-0002.json";

// ----------------------------------------
