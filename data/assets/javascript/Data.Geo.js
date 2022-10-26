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

function bestFitToGeo (reqGeo, orgGeo) {
    for (let v of serverSupportedGeos) {
        const g = readGeo(v);
        if (fitsIntoV2(reqGeo, g))
            return g;
    }

    // optimize resizing for large pictures like panoramas
    const sizeReqGeo = areaV2(reqGeo);
    const sizeOrgGeo = areaV2(orgGeo);
    return ( 4 * sizeReqGeo < sizeOrgGeo )
        ? roundV2(reqGeo)  // request a scaled down copy
        : geoOrg;          // request original size image
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
    const leftTop = floorV2(mulV2(subV2(frameGeo, geo), 0.5));
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
    const ar = minCompV2(divV2(g, s));
    return mulV2(s, ar);
}

// scale s such that is is
// the smallest geo with same aspect ratio as s
// in which g fits into

function fillGeo(s, g) {
    const ar = maxCompV2(divV2(g, s));
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

// don't resize geometry
function realSize(s, g) {
    return s;
}
// --------------------

function offsetC (o) { return o; }
function offsetN (o) { return V2(o.x, 0);}
function offsetNW(o) { return V2(0, 0); }
function offsetW (o) { return V2(0, o.y);}
function offsetSW(o) { return V2(0, o.y * 2);}
function offsetS (o) { return V2(o.x, o.y * 2);}
function offsetSE(o) { return V2(o.x * 2, o.y * 2);}
function offsetE (o) { return V2(o.x * 2, o.y);}
function offsetNE(o) { return V2(o.x * 2, 0);}

// --------------------

function resizeAlg(name) {
    switch ( name ) {
    case 'fit'        :
    case 'fitInto'    : return fitIntoGeo;
    case 'fill'       : return fillGeo;
    case 'fitheight'  :
    case 'sameHeight' : return sameHeightGeo;
    case 'fitwidth'   :
    case 'sameWidth'  : return sameWidthGeo;
    case 'fix'        :
    case 'abs'        :
    default           : return realSize;
    }
}

function offsetAlg(name) {
    switch ( name ) {
    case 'n' :
    case 'N' : return offsetN;
    case 'nw':
    case 'NW': return offsetNW;
    case 'w' :
    case 'W' : return offsetW;
    case 'sw':
    case 'SW': return offsetSW;
    case 's' :
    case 'S' : return offsetS;
    case 'se':
    case 'SE': return offsetSE;
    case 'e' :
    case 'E' : return offsetE;
    case 'ne':
    case 'NE': return offsetNE;
    case 'C' :
    case 'c' :
    case 'center':
    default: return offsetC;
    }
}

// --------------------
//
// resize and place an image into a frame
//
// frameGeo   : the size of the stage
// imgGeo     : size of the meda (img, movie, ...)
//
// alg        : resize algorithms
// fitInto    : largest geo with whole image fits into frame
// fill       : smallest geo with whole frame is covered by image
// sameHeight : image has same height as frame
// sameWidth  : image has same width as frame
// fix        : default, image isn't resized
//
// scale      : afterwards image may be scaled by a factor
//
// dir        : alignment
// center     : default, image is centered on stage
// N, NW, W, SW, S, SE, E, NE
//            : orientation: NW = top left corner
//
// shift      : afterwards shift the image by given amount
//              reltive to size of the frame
//              shift = (0.1,0.2) -> 10% of frame width to the right
//                                   20% of frame height to the bottom

function placeImg(frameGeo, imgGeo, alg, scale, dir, shift) {
    const sc  = scale || V2(1.0,1.0);
    const g1  = resizeAlg(alg)(imgGeo, frameGeo);
    const geo = mulV2(g1, sc);

    const sh  = mulV2(shift || V2(0,0), frameGeo);
    const o1  = mulV2(subV2(frameGeo, geo), V2(0.5));
    const o2  = offsetAlg(dir)(o1);
    const off = addV2(o2, sh);
    const res =  { geo: geo,
                   off: off
                 };

    trc(1,`placeImg: ${showGeo(res.geo)}, ${showGeo(res.off)}`);
    return res;
}

function placeMedia(frameGeo, imgGeo) {
    function place(gs) {
        const alg   = gs.alg   || 'fitInto';
        const sc    = gs.scale || V2(1.0,1.0);
        const scale = typeof sc === 'Number'
                    ? V2(sc, sc)
                    : sc;
        const dir   = gs.dir   || 'center';
        const shift = gs.shift || V2(0,0);
        return placeImg(frameGeo, imgGeo, alg, scale, dir, shift);
    }
    return place;
}

function placeFrame(frameGeo, gs) {
    return placeMedia(frameGeo, frameGeo)(gs);
}

function maxGeo(frameGeo, imgGeo, geos) {
    const place = placeMedia(frameGeo, imgGeo);
    let res = V2(0,0);
    for (const g of geos) {
        res = maxV2(res, place(g).geo);
    }
    trc(1,`maxGeo: res=${showGeo(res)}`);
    return res;
}

function showGo(go) {
    return `${showGeo(go.geo)},${showGeo(go.off)}`;
}

// ----------------------------------------
