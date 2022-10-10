// --------------------
//
// screen and picture geometry ops
//

// --------------------

// "1x1" as geo for server gets image with original geometry

const geoOrg = oneV2;

// geo's of copies to be cached on server side

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

// --------------------
// compute icon geometry for different screen sizes

function bestFitIconGeo(frameGeo) {
    if (frameGeo.x <= 1280)
        return V2(120, 90);
    if (frameGeo.x <= 1400)
        return V2(140, 105);

    return V2(160, 120);
}

// --------------------
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

// --------------------
// compute left/top offset, when geo is placed centered
// into frame with, optionally shifted by an offset shift0

function placeOnFrame(frameGeo, geo, shift0) {
    const shift   = shift0 || zeroV2;
    const leftTop = scaleV2(subV2(frameGeo, geo), 0.5);
    const res     = addV2(leftTop, shift);
    trc(1,`placeOnFramw: ${showGeo(leftTop)} ${showGeo(res)}`);
    return res;
}

// --------------------

// panorama: horizontal: w >= 2 * h
//           vertical:   h >= 2 * w

function isPano(g) {
    const ar = aspectRatioV2(g);
    return ( // fitsIntoV2(imgTabGeo(), g)
             // &&
             ( ar >= 2 || ar <= 0.5 )
           );
}

// width > height
function isHorizontal(g) {
    return aspectRatioV2(g) > 1;
}

// height > width
function isVertical(g) {
    return aspectRatioV2(g) > 1;
}

// isSquare
function isVertical(g) {
    return aspectRatioV2(g) === 1;
}

// --------------------
//
// Geo to/from String ops

// parse Geo from String
function readGeo(txt) {
    const a = txt.split('x');
    return V2(1 * a[0], 1 * a[1]);
}

// Geo to String
function showGeo(geo) {
    return "" + geo.x + "x" + geo.y;
}

// Number or Geo to CSS pixel format
function toPx(val) {

    // convert to CSS px units
    function px(n) {
        return Math.round(n) + "px";
    }

    if (typeof val === "number") {
        return px(v);
    }
    const res = {};
    for (k in val) {
        res[k] = px(Math.round(val[k]));
    }
    return res;
}

// --------------------

// scale s such that it is
// the larges geo with same aspect ratio as s
// that fits into g

function fitIntoGeo(s, g) {
    const ar = minV2(divV2(g, s));
    return mulV2(s, ar);
}

// scale s such that is is
// the smallest geo with same aspect ratio as s
// in which g fits into

function fillGeo(s, g) {
    const ar = maxV2(divV2(g, s));
    return mulV2(s, ar);
}

// scale s such that it has the same height as g
// and aspect ratio remain

function sameHeightGeo(s, g) {
    const ar = divV2(g, s).y;
    return mulV2(s, ar);
}

// scale s such that it has the same width as g
// and aspect ratio remains

function sameWidthGeo(s, g) {
    const ar = divV2(g, s).x;
    return mulV2(s, ar);
}

// --------------------
