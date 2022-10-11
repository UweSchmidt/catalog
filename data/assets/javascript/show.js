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

function jsonReqToUrl1(frameGeo, req) {
    const req1 = mkRequest('json',
                           req.rPathPos,
                           showGeo(bestFitToGeo(frameGeo)));
    return reqToUrl(req1);
}

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

function loadImg(frameGeo, id, url, geo, resizeAlg) {
    const imgGeo = fitToFrameGeo(frameGeo, geo, resizeAlg);
    const o      = toPx(placeOnFrame(frameGeo, imgGeo));
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
// --------------------

function loadFullImg(frameGeo, id, url, imgGeo) {
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

function loadPanoramaImg(frameGeo, id, url, imgGeo) {

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

function loadImgFG1(frameGeo, id, req, geo, resizeAlg) {
    if (resizeAlg === "fullsize") {
        loadFullImg(frameGeo, id, req, geo);
    }
    else if (resizeAlg === "panorama") {
        loadPanoramaImg(frameGeo, id, req, geo);
    }
    else if (resizeAlg === "zoom") {
        loadZoomableImg(frameGeo, id, req, geo);
    }
    else {
        loadImg(frameGeo, id, req, geo, resizeAlg);
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

function loadMovie1(frameGeo, id, url, geo, rType, resizeAlg) {
    const movGeo  = fitToFrameGeo(frameGeo, geo, resizeAlg);
    const off     = toPx(placeOnFrame(frameGeo, movGeo));
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

function showMovieAlg1(frameGeo, page, resizeAlg) {
    const movReq = page.imgReq;
    const movGeo = readGeo(page.oirGeo[0]);
    const movUrl = toMediaUrl(page.img);
    const id     = nextImgId();

    trc(1, "showMovieAlg1: url=" + movUrl + ", geo=" + showGeo(movGeo));

    loadMovie1(frameGeo, id, movUrl, movGeo, movReq.rType, resizeAlg);
    toggleImg12(id);
}

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
    const id = nextImgId();
    showColId(id, frameGeo, page);
    toggleImg12(id);
}

function showColId(id, frameGeo, page) {
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
    const e        = clearCont(id);
    setCSS(e, { width:    g.x,
                height:   g.y,
                left:     "0px",
                top:      "0px",
                overflow: "auto"
              });
    e.appendChild(
        buildCollection(frameGeo,
                        colReq, iconReq,
                        colMeta,
                        navIcons, c1Icon, colIcons, colBlog
                       )
    );
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
                a.href  = toHref(jsonReqToUrl1(frameGeo, req));
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
            a.href   = toHref(jsonReqToUrl1(frameGeo, ir));
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
