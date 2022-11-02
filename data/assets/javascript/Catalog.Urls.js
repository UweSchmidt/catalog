// ----------------------------------------
//
// URL functions for accessing catalog server

// import Data.V2
// import Data.Geo

// --------------------
// request ops

function mkRequest(rType, rPathPos, rGeo) {
    return { rType:    rType,
             rPathPos: rPathPos,
             geo:      rGeo || "org",
           };
}

function nullReq(req) {
    return (! req);
}

function isColReq(req) {
    const pos = req.rPathPos[1];
    return (typeof pos != 'number');
}

function isPicReq(req) {
    return ['img', 'imgfx', 'icon', 'iconp'].includes(req.rType);
}

function isMovieReq(req) {
    return ['movie', 'gif'].includes(req.rType);
}

function isBlogReq(req) {
    return req.rType === 'page';
}

function rExt(rType) {
    switch ( rType ) {
    case 'json':
        return '.json';

    case 'img':
    case 'imgfx':
    case 'icon':
    case 'iconp':
        return '.jpg';

    case 'movie':
        return '.mp4';

    case 'gif':
        return '.gif';

    default:
        return '';
    }
}

// --------------------
// URL ops

function mkJsonUrl(path, geo) {
    const rPathPos = pathToPathPos(path);
    const jReq     = mkRequest('json', rPathPos, geo);
    const jUrl     = jsonReqToUrl(jReq);
    return reqToUrl(jReq);
}

function imgReqToUrl(req, imgGeo) {
    req.geo = eqV2(imgGeo, oneV2) ? "org" : showGeo(imgGeo);
    return reqToUrl(req);
}

function reqToUrl(req) {
    const ty = req.rType;
    const ge = req.geo;
    const pp = rPathPosToUrl(req.rPathPos);
    const ex = rExt(ty);

    return `/docs/${ty}/${ge}${pp}${ex}`;
}

function toHref(url) {
    return `javascript:gotoUrl('${url}');`;
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

function sPathToPathPos(path0) {
    return pathToPathPos(pathCollections() + path0);
}

// ----------------------------------------
