// ----------------------------------------
//
// 2-dim Vector Library
//
// vector implemented as object with fields "x" and "y"

// constructor

function V2(x, y) {
    if ( x === undefined) {
        return {x: 0, y: 0};
    }
    if ( y === undefined ) {
        return {x: x, y: x};
    }
    return {x: x, y: y};
}

function toV2(...args) {
    trc2("toV2:", args);
    const args2 = map(toNum)(args);
    trc2("toV2:", args2);
    return V2(...args2);
}

const zeroV2 = V2();
const oneV2  = V2(1);

// to string

function showV2(p) {
    return `(${p.x},${p.y})`;
}

function mapV2(f, p) {
    return V2(f(p.x), f(p.y));
}

function zipV2(f, p1, p2) {
    return V2(f(p1.x, p2.x), f(p1.y, p2.y));
}

// arithmetic

function addV2(p1, p2) {
    if (typeof p2 === "number") {
        return { x: p1.x + p2,   // vector + scalar
                 y: p1.y + p2
               };

    } else {
        return { x: p1.x + p2.x,
                 y: p1.y + p2.y
               };
    }
}

function mulV2(p1, p2) {
    if (typeof p2 === "number") {
        return { x: p1.x * p2,   // vector * scalar
                 y: p1.y * p2
               };

    } else {
        return { x: p1.x * p2.x,
                 y: p1.y * p2.y
               };
    }
}

function divV2(p1, p2) {
    if (typeof p2 === "number") {
        return V2(p1.x / p2,   // vector / scalar
                  p1.y / p2
                 );

    } else {
        return V2(p1.x / p2.x,
                  p1.y / p2.y
                 );
    }
}

function subV2(p1, p2) {
    if (typeof p2 === "number") {
        return V2(p1.x - p2,   // vector - scalar
                  p1.y - p2
                 );

    } else {
        return V2(p1.x - p2.x,
                  p1.y - p2.y
                 );
    }
}

function maxV2(p1, p2) {
    return V2(Math.max(p1.x, p2.x),
              Math.max(p1.y, p2.y)
             );
}

function minV2(p1, p2) {
    return V2(Math.min(p1.x, p2.x),
              Math.min(p1.y, p2.y)
             );
}

function eqV2(p1, p2) {
    return p1.x === p2.x && p1.y === p2.y;
}

function neqV2(p1, p2) {
    return ! eqV2(p1, p2);
}

function nullV2(p) {
    return p.x === 0 && p.y === 0;
}

function roundV2(p) {
    return V2(Math.round(p.x),
              Math.round(p.y)
             );
}

function ceilV2(p) {
    return V2(Math.ceil(p.x),
              Math.ceil(p.y)
             );
}

function floorV2(p) {
    return V2(Math.floor(p.x),
              Math.floor(p.y)
             );
}

function maxCompV2(p) {
    return Math.max(p.x, p.y);
}

function minCompV2(p) {
    return Math.min(p.x, p.y);
}

function aspectRatioV2(p) {
    return p.x / p.y;
}

function areaV2(p) {
    return p.x * p.y;
}

function fitsIntoV2(p1, p2) {
    return p1.x <= p2.x && p1.y <= p2.y;
}

function lessThanV2(p1, p2) {
    return p1.x < p2.x && p1.y < p2.y;
}

// ----------------------------------------
