// ----------------------------------------
//
// zoomable and movable image (experimental)

// --------------------

var defaultZoomDur       = 1.5;  // sec

var zoomState = { id       : "",
                  idImg    : "",
                  frameGeo : zeroV2,  // current frame geo
                  orgGeo   : zeroV2,  // original geometry of image
                  curGeo   : zeroV2,  // geometry of image displayed on screen
                  curOff   : zeroV2,  // left and top coords of displayed image
                  scale    : 1,       // resize factor of image
                  shift    : zeroV2,  // shift org image from center
                                 // {x: 0.2, y: 0.1}:
                                 // center moves 20% of width to the right
                                 // and 10% of height to the bottom
                  cssId    : ""
                };

function loadZoomableImg(frameGeo, id, url, geo, scale, shift, cssId) {
    cssId = cssId || "image-zoom-css";

    initZoomState(frameGeo, id, cssId);
    initZoomGeo(frameGeo, geo);
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

function initZoomState(frameGeo, id, cssId) {
    const g0 = readGeo(currPage.oirGeo[0]);
    zoomState = { id      : id,
                  idImg   : mkImgId(id),
                  frameGeo: frameGeo,
                  orgGeo  : g0,
                  curGeo  : g0,
                  curOff  : placeOnFrame(frameGeo, g0, zeroV2),
                  shift   : zeroV2,
                  scale   : 1,
                  cssId   : cssId,
                };
}

function initZoomGeo(frameGeo, geo) {
    zoomState.curGeo = geo;
    zoomState.curOff = placeOnFrame(frameGeo, geo, zoomState.shift);
    zoomState.scale = geo.x / zoomState.orgGeo.x;
}

function zoomTo(scale, shift) {
    const sc = scale || zoomState.scale;
    const sh = shift || zoomState.shift;

    const g0 = zoomState.orgGeo;
    const g1 = mulV2(g0, sc);

    const s1 = mulV2(mulV2(sh, g0), sc);
    const o1 = placeOnFrame(zoomState.frameGeo, g1, s1);

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

    const css = clearCont(zoomState.cssId);
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

// ----------------------------------------
