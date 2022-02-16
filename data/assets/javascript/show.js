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
const info    = "info";
const infoTab = "info-table";
const help    = "help";

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
    return fitsInto(screenGeo(), g)
        && (g.w / g.h >= 2);
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

function resizeToScreenHeight(g) {
    return resizeToHeight(g, screenGeo());
}

function screenGeo() {
    return { w : screen.availWidth  / 2,  // just for testing
             h : screen.availHeight / 2
           };
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
        return readGeo("40x105");

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

function editUrl(req) {
    return 'javascript:openEditUrl('
        + qt(req.rPathPos[0])
        + ","
        + req.rPathPos[1]
        + ');' ;
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
        throw ("getElem: no elem found for " + id);
    }
    return e;
}

function clearCont(e) {
    e.innerHTML = '';
    return e;
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

/* ---------------------------------------- */

function showElem(id) {
    setCSS(id, "display", "block");
}

function hideElem(id) {
    setCSS(id, "display", "none");
}

function showImg2() {
    hideElem(img1);
    showElem(img2);
}

function isHidden(id) {
    const s = getStyle(id, "display");
    return ! s || s === "none";
}

function changeElems(id1, id2) {
    hideElem(id2);
    showElem(id1);
}

function toggleElems(id1, id2) {
    if (isHidden(id1)) {
        changeElems(id1,id2);
    } else {
        changeElems(id2,id1);
    }
}

function toggleImg12()  { toggleElems(img1,   img2);   }

function nextImgId() {
    return isHidden(img1) ? img1 : img2;
}

function currImgId() {
    return isHidden(img1) ? img2 : img1;
}

/* ---------------------------------------- */
/* global state */

var nextPage;
var currPage;

var picCache = new Image();

/* ---------------------------------------- */
/* initialization */

function initShow() {
    const g = toPx(screenGeo());
    console.log("initShow: screen geo=" + showGeo(g));

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
    const e = clearCont(getElem(id));
    setCSS(e, style);

    const i = newElem("img", id + "-img",
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
    const e = clearCont(getElem(id));
    setCSS(e, style);

    const i = newElem("img", id + "-img",
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
    const leftOffset = scrGeo.w - imgGeo.w;

    // add keyframe style
    const s = clearCont(getElem("panorama-css"));
    const kf  = "pano-move-right";
    const css = "@keyframes " + kf + " {"
          + "   0% {left: 0px}"
          + "  45% {left: " + leftOffset + "px}"
          + "  55% {left: " + leftOffset + "px}"
          + " 100% {left: 0px}"
          + "}";
    s.appendChild(newText(css));

    const style = { width    : scrGeo.w + "px",
                    height   : scrGeo.h + "px",
                    left     : "0px",
                    top      : "0px",
                    overflow : "hidden"
                  };
    const e = clearCont(getElem(id));
    setCSS(e, style);

    const d = 2.5 * 7.0 * (imgGeo.w / imgGeo.h);
    const i = newElem("img", id + "-img",
                      { position:                    "absolute",
                        left:                        "0px",
                        top:                         "0px",
                        "animation-name":            kf,
                        "animation-duration":        d + "s",
                        "animation-delay":           "2s",
                        "animation-iteration-count": 1,
                        "animation-direction":       "alternate", // redundant due to animation-iteration-count: 1
                        "animation-timing-function": "ease-in-out",
                        "animation-play-state":      "paused"
                      },
                      "img panorama"
                     );
    // i.onload = "javascript:togglePanoAnimation()";
    i.addEventListener("load", togglePanoAnimation);
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
        loadImg1(nextImgId(), imgUrl, imgGeo, resizeAlg);
        toggleImg12();
    };
    picCache.src = imgUrl; // the .onload handler is triggered here
}

function showImg(page)          { showImg1(page, "no-magnify"); }
function showMagnifiedImg(page) { showImg1(page, "magnify");    }
function showFullSizeImg(page)  { showImg1(page, "fullsize");   }
function showPanoramaImg(page)  { showImg1(page, "panorama");   }

function isFullImg() {
}
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
    const e = clearCont(getElem(id));
    setCSS(e, style);

    // build new contents
    const id1   = id + "-movie";

    if (rType === "movie") {
        const v    = newElem("video", id1, {}, "movie video " + resizeAlg);
        v.autoplay = "autoplay";
        v.muted    = "muted";
        v.width    = movGeo.w;
        v.heigth   = movGeo.h;

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
        const v2 = newElem("img", id1,
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

    trc(1, "showMovie: url=" + movUrl + ", geo=" + showGeo(movGeo));

    loadMovie(nextImgId(), movUrl, movGeo, movReq.rType, resizeAlg);
    toggleImg12();
}

function showMovie(page)          { showMovie1(page, "no-magnify"); }
function showMagnifiedMovie(page) { showMovie1(page,    "magnify"); }

// ----------------------------------------

function showBlog(page) {
    const req = page.imgReq;
    const geo = toPx(screenGeo());
    const txt = page.blogCont;
    const id  = nextImgId();

    trc(1, "showBlog: " + txt);

    // get element, clear contents and set style attributes
    const e  = clearCont(getElem(id));
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

    toggleImg12();
}

// ----------------------------------------

function showCol(page) {
    const colDescr = page.colDescr;
    const colReq   = colDescr.eReq;
    const colMeta  = colDescr.eMeta;
    const navIcons = page.navIcons;
    const c1Icon   = page.c1Icon;
    const colIcons = page.contIcons;
    const iconReq  = { rType: "icon",
                       rPathPos: colReq.rPathPos
                     };
    const g        = toPx(screenGeo());

    const e = clearCont(getElem(nextImgId()));
    setCSS(e, { width:  g.w,
                height: g.h,
                left:   "0px",
                top:    "0px"
              });
    e.appendChild(buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons));

    toggleImg12();
}

function buildCollection(colReq, iconReq, colMeta, navIcons, c1Icon, colIcons) {
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

    const cssBorder = border + "px solid #444";

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
                const r = newElem("div", "collection-header-icon",
                                  { width:  iG.w,
                                    heigth: iG.h,
                                    border: cssBorder
                                  });

                const a = newElem("a");
                a.href  = editUrl(colReq);
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
            const r = newElem("div");

            if ( isHeader ) {
                const t1 = colMeta["Descr:Title"];
                const t2 = colMeta["Descr:Subtitle"];
                const t3 = colMeta["Descr:Comment"];

                r.id = "collection-header-title";

                if (t1) {
                    r.appendChild(buildLine("title", t1));
                }
                if (t2) {
                    r.appendChild(buildLine("subtitle", t2));
                }
                if (t3) {
                    r.appendChild(buildLine("comment", t3));
                }
            }
            return r;
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
                          isHeader ? "collection-header" : "collection-footer",
                          { display: "grid",
                            "grid-template-columns": iGB.w + " auto " + navGeo.w + "px",
                            "grid-column-gap": "1em"
                          });
        h.appendChild(buildHeadIcon());
        h.appendChild(buildHeadLine());
        h.appendChild(buildNav());
        return h;
    }

    function buildColContents() {
        const r = newElem("div", "collection-contents",
                          { display: "grid",
                            "grid-template-columns": replicate(numCols, " " + iGB.w),
                            "grid-auto-rows": iGB.h,
                            "grid-gap":       gap + "px",
                          });


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

    const c = newElem("div", "collection",
                      { padding:      padding + "px",
                        "min-height": scrGeo.h + "px"
                      },
                      "col"
                     );
    c.appendChild(buildColHeaderFooter(true));
    c.appendChild(buildColContents());
    c.appendChild(buildColHeaderFooter(false));
    return c;
}

// ----------------------------------------

function showPage(page) {
    trc(1, "showPage:" + page);

    // store page as new currPage
    currPage = page;

    buildInfo();
    setPageTitle();

    const rty = isColPage() ? page.colDescr.eReq.rType : page.imgReq.rType;

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
    } else {
        trc(1, "showNextPage: req is empty");
    }
}

function showErr(err) {
    trc(1, "showErr:" + err);
    trc(1, "not yet handled");
}

// ----------------------------------------
// predicates for currPage

function isColPage() {
    return ! (currPage.imgReq);
}

function isImgPage() {
    return ! isColPage();
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
            const orgGeo = currPage.oirGeo[0];
            return lessThan(orgGeo, screenGeo());
        }
    }
    return false;
}

// ----------------------------------------
// predicates for current visible part of DOM

function isPic() {
    const e = getElem(currImgId() + "-img");
    return e && e.classList.contains("img");
}

function isMovie() {
    const e = getElem(currImgId() + "-movie");
    return e && e.classList.contains("movie");
}

function isFullImg() {
    const e = getElem(currImgId() + "-img");
    return e && e.classList.contains("fullsize");
}

function isMagnifiedImg() {
    const e = getElem(currImgId() + "-img") || getElem(currImgId + "-movie");
    return e && e.classList.contains("magnify");
}

function isPanoImg() {
    const e = getElem(currImgId() + "-img");
    return e && e.classList.contains("panorama");
}

// ----------------------------------------
// event handler

function gotoPrev() {
    const req = isColPage() ? currPage.navIcons.prev.eReq : currPage.imgNavRefs.prev;
    showNextPage(req);
}

function gotoNext() {
    const req = isColPage() ? currPage.navIcons.next.eReq : currPage.imgNavRefs.next;
    showNextPage(req);
}

function gotoPar() {
    const req = isColPage() ? currPage.navIcons.par.eReq : currPage.imgNavRefs.par;
    showNextPage(req);
}

// for slideshows
function goForward() {
    const req = isColPage() ? currPage.navIcons.fwrd.eReq : currPage.imgNavRefs.fwrd;
    showNextPage(req);
}

function gotoChild(i) {
    if (isColPage()) {
        showNextPage(currPage.navIcons.contIcons[i].eReq);
    }
}

// reload
function stayHere() {
    const req = isColPage() ? currPage.colDescr.eReq : currPage.imgReq;
    showNextPage(req);
}

// call catalog edit

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
    const md = isColPage() ? currPage.colDescr.eMeta : currPage.img[2];
    const it = getElem(infoTab);
    buildMetaInfo(it, md);
}

function showInfo() {
    hideHelp();
    showElem(info);
}

function hideInfo() {
    hideElem(info);
}

function toggleInfo() {
    if (isHidden(info)) {
        showInfo();
    } else {
        hideInfo(info);
    }
}

function showHelp() {
    hideInfo();
    showElem(help);
}

function hideHelp() {
    hideElem(help);
}

function toggleHelp() {
    if (isHidden(help)) {
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
        const i = getElem(currImgId() + "-img");
        const s = i.style.animationPlayState;
        setCSS(i, { animationPlayState: (s === "paused" ? "running" : "paused")
                  });
    }
}

// ----------------------------------------

function setPageTitle() {
    let txt = "";
    if ( isColPage() ) {
        const cd = currPage.colDescr;
        const md = cd.eMeta;
        const rq = cd.rPathPos;
        txt = md["Descr:Title"] || baseName(rq);
    } else {
        txt = baseName(currPage.img[0]);
    }
    const e = getElem(title);
    clearCont(e).appendChild(newText(txt));
}


// ----------------------------------------

const u1 = "/docs/iconp/900x600/archive/collections/photos/tests/pic-0012.jpg";
const u2 = "/docs/img/160x120/archive/collections/photos/tests/pic-0003.jpg";

function ttt() {
    loadImg(img1, u1, readGeo("900x600"));
}
function sss() {
    loadImg(img2, u2, readGeo("160x120"));
}




/* ---------------------------------------- */

const req1 = {
    "rType": "json",
    "rPathPos": [
        "/archive/collections/albums/2021",
        null
    ]
};

const imgPage =
      {
          "imgReq": {
              "rType": "img",
              "rPathPos": [
                  "/archive/collections/albums/EinPaarBilder",
                  1
              ]
          },
          "oirGeo": [
              "3894x2592",   // org size
              "1280x800",    // requested size
              "1201x800"     // scaled down size
          ],
          "img": [
              "/archive/photos/2001/xxx-Stockente-1",
              "xxx-Stockente-1.png",
              {
                  "Composite:ImageSize": "3894x2592",
                  "Composite:Megapixels": "10.1",
                  "Descr:Duration": "1.0",
                  "Descr:Title": "xxx-Stockente-1.png",
                  "Descr:Title1": "xxx-Stockente-1.png",
                  "File:FileSize": "13 MB",
                  "File:MimeType": "image/png",
                  "File:Name": "xxx-Stockente-1.png",
                  "File:RefImg": "/photos/2001/xxx-Stockente-1.png",
                  "File:RefJpg": "/docs/img/1280x800/archive/collections/albums/EinPaarBilder/pic-0001.jpg",
                  "File:RefMedia": "/docs/img/1280x800/archive/collections/albums/EinPaarBilder/pic-0001.jpg",
                  "File:TimeStamp": "1575482988",
                  "Img:EXIFUpdate": "1610735082",
                  "Descr:Rating": "5"
              }
          ],
          "imgNavRefs": {
              "prev": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      0
                  ]
              },
              "next": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              },
              "par": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      null
                  ]
              },
              "fwrd": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              }
          },
          "imgNavImgs": {
              "prev": {
                  "rType": "img",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      0
                  ]
              },
              "next": {
                  "rType": "img",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              },
              "par": null,
              "fwrd": {
                  "rType": "img",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              }
          },
          "blogCont": "",
          "now": "2022-02-03T15:11:47"
      };

const movPage = {
  "imgReq": {
    "rType": "movie",
    "rPathPos": [
      "/archive/collections/photos/2018/2018-09-14.iPhone",
      0
    ]
  },
  "img": [
    "/archive/photos/2018/2018-09-14.iPhone/IMG_0009",
    "IMG_0009.mp4",
    {
      "Composite:ImageSize": "608x1080",
      "Composite:Megapixels": "0.657",
      "Descr:Duration": "1.0",
      "Descr:Title": "IMG_0009.mp4",
      "File:FileSize": "3.3 MB",
      "File:MimeType": "video/mp4",
      "File:Name": "IMG_0009.mp4",
      "File:RefImg": "/photos/2018/2018-09-14.iPhone/IMG_0009.mp4",
      "File:RefJpg": "/archive/photos/2018/2018-09-14.iPhone/IMG_0009.mp4",
      "File:RefMedia": "/archive/photos/2018/2018-09-14.iPhone/IMG_0009.mp4",
      "File:TimeStamp": "1592056240",
      "Img:EXIFUpdate": "1610735084",
      "QuickTime:Duration": "14.44 s",
      "QuickTime:ImageHeight": "1080",
      "QuickTime:ImageWidth": "608",
      "QuickTime:VideoFrameRate": "29.97"
    }
  ],
  "imgNavRefs": {
    "prev": null,
    "next": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    },
    "par": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        null
      ]
    },
    "fwrd": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    }
  },
  "imgNavImgs": {
    "prev": null,
    "next": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    },
    "par": null,
    "fwrd": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    }
  },
  "oirGeo": [
    "608x1080",
    "1x1",
    "0x1"
  ],
  "blogCont": "",
  "now": "2022-02-03T18:16:57"
}

const gifPage = {
  "imgReq": {
    "rType": "gif",
    "rPathPos": [
      "/archive/collections/photos/tests",
      0
    ]
  },
  "oirGeo": [
    "200x35",
    "1x1",
    "1x0"
  ],
  "img": [
    "/archive/photos/tests/hund",
    "hund.gif",
    {
      "Composite:ImageSize": "200x35",
      "Composite:Megapixels": "7.0e-3",
      "Descr:Duration": "1.0",
      "Descr:Title": "hund.gif",
      "File:FileSize": "13 kB",
      "File:MimeType": "video/x-gif",
      "File:Name": "hund.gif",
      "File:RefImg": "/photos/tests/hund.gif",
      "File:RefJpg": "/archive/photos/tests/hund.gif",
      "File:RefMedia": "/archive/photos/tests/hund.gif",
      "File:TimeStamp": "1611057556",
      "GIF:AnimationIterations": "1000",
      "GIF:Duration": "1.00 s",
      "GIF:FrameCount": "35",
      "Img:EXIFUpdate": "1611165598"
    }
  ],
  "imgNavRefs": {
    "prev": null,
    "next": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/tests",
        1
      ]
    },
    "par": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/tests",
        null
      ]
    },
    "fwrd": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/tests",
        1
      ]
    }
  },
  "imgNavImgs": {
    "prev": null,
    "next": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/tests",
        1
      ]
    },
    "par": null,
    "fwrd": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/tests",
        1
      ]
    }
  },
  "blogCont": "",
  "now": "2022-02-04T09:49:48"
};

const blogPage = {
  "imgReq": {
    "rType": "page",
    "rPathPos": [
      "/archive/collections/photos/2001",
      9
    ]
  },
  "oirGeo": [
    "1x1",
    "1x1",
    "1x1"
  ],
  "img": [
    "/archive/photos/2001/index",
    "index.md",
    {
      "Descr:Duration": "1.0",
      "Descr:Title": "index.md",
      "File:MimeType": "text/x-markdown",
      "File:Name": "index.md",
      "File:RefImg": "/photos/2001/index.md",
      "File:RefJpg": "/docs/page/1x1/archive/collections/photos/2001/pic-0009.html",
      "File:RefMedia": "/docs/page/1x1/archive/collections/photos/2001/pic-0009.html",
      "File:TimeStamp": "1536329082",
      "Img:EXIFUpdate": "1610735082"
    }
  ],
  "imgNavRefs": {
    "prev": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2001",
        8
      ]
    },
    "next": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2001",
        10
      ]
    },
    "par": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2001",
        null
      ]
    },
    "fwrd": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2001",
        10
      ]
    }
  },
  "imgNavImgs": {
    "prev": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2001",
        8
      ]
    },
    "next": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2001",
        10
      ]
    },
    "par": null,
    "fwrd": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2001",
        10
      ]
    }
  },
  "blogCont": "<h2 id=\"ein-paar-bunte-blumen\">ein paar bunte Blumen</h2>\n<p>allerdings nur 3 Stück aber im Augenblick werden die nicht\nimportiert</p>\n<p>inzwischen werden auch wieder .png und .tiff importiert Alles wird\nimmer besser</p>\n<p>bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla\nbla bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br>\nbla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla<br> bla bla bla bla bla bla\nbla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla\nbla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla\nbla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla\nbla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla\nbla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla\nbla bla bla bla bla bla bla bla bla bla</p>\n",
  "now": "2022-02-04T12:08:56"
};


// ----------------------------------------
// build metadata table

const metaInfo = {
    "Descr:Title":                  "Titel",
    "Descr:Subtitle":               "Untertitel",
    "Descr:TitleEnglish":           "Titel engl.",
    "Descr:TitleLatin":             "Titel lat.",
    "Descr:Comment":                "Kommentar (Aufnahme)",
    "Descr:CommentImg":             "Kommentar (Kopie)",
    "Exif:CreateDate":              "Aufnahmedatum",
    "Descr:Address":                "Adresse",
    "Descr:Location":               "Ort",
    // "Descr:GPSPosition":            "Position",
    "Descr:GPSPositionDeg":         "Position in Grad",
    "Descr:GPSAltitude":            "Höhe",
    "Descr:Web":                    "Web",
    "Descr:Wikipedia":              "Wikipedia",
    "Descr:Keywords":               "Schlüsselwörter",
    "Exif:Model":                   "Kamera",
    "Composite:LensSpec":           "Objektiv",
    "Composite:LensID":             "Objektiv Typ",
    "Exif:FocalLength":             "Brennweite",
    "Exif:FocalLengthIn35mmFormat": "Brennweite in 35mm",
    "Exif:ExposureTime":            "Belichtungszeit",
    "Exif:FNumber":                 "Blende",
    "Exif:ExposureCompensation":    "Belichtungskorrektur",
    "Exif:ISO":                     "ISO",
    "Exif:ExposureMode":            "Belichtungsmessung",
    "Exif:ExposureProgram":         "Aufnahmebetriebsart",
    "MakerNotes:FocusDistance":     "Entfernung",
    "Composite:DOF":                "Tiefenschärfe",
    "Composite:FOV":                "Sichtfeld",
    "Composite:HyperfocalDistance": "Hyperfokale Distanz",
    "MakerNotes:ShootingMode":      "Aufnahmemodus",
    "Exif:WhiteBalance":            "Weißabgleich",
    "MakerNotes:ShutterCount":      "Aufnahmezähler",
    "Composite:ImageSize":          "Geometrie",
    "File:RefRaw":                  "Raw-Datei",
    "File:RefImg":                  "Bilddatei",
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
    // TODO not yet complete
    "Descr:GPSPositionDeg":    fmtGPS,
    "Composite:Megapixels": fmtMPX,
    "Descr:Rating": fmtRating
};

function lookupFmt(k) {
    return metaFmt[k] || newText;
}

function fmtMPX(t) {
    return newText("" + (1 * t));
}

var gpsUrl;

function fmtGPS(t) {
    const deg = String.fromCharCode(176);

    const a   = newElem("a");
    const txt = newText(t.replace(" deg", deg));

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
        if (err.status == 500) {
            const msg = err.responseJSON || err.responseText;
            processErr(msg);
        }
        else if (err.status == 404) {
            processErr("archive entry not found: " + url);
        }
        else {
            processErr("server error: " + err.status + " when loading json page");
        }
    }).always(processNext);
}

// ----------------------------------------
