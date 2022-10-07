// --------------------
//
// screen and picture geometry ops
//

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

function sameHeigthGeo(s, g) {
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
