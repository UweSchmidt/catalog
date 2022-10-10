// ----------------------------------------
//
// animation control

// ----------------------------------------
//
// naming convention
//
// fadein-<cls>, visible-<cls>, fadeout-<cls>, hidden-<cls>
// with
// cls=name of animation class

function initShowHandlers() {
    initAnimHandlers(imageShowAnims);
}

function initAnimHandlers(xs) {
    while (xs.length >= 2) {
        const id  = xs.shift();
        const cls = xs.shift();
        initAnimHandler(id, cls);
    }
}

function initAnimHandler(id, cls) {
    const e = getElem(id);
    e.classList.add(`hidden-${cls}`);
    e.addEventListener("animationend",
                       function(ev) {
                           handleAnim(id, cls, ev);
                       }
                      );
}

function handleAnim(id, cls, ev) {
    const n = ev.animationName;
    trc(1, `handleAnim: ${id}-${cls} anim-name: ${n}`);
    if ( n === `kf-fadein-${cls}` || n === `kf-fadeout-${cls}` ) {
        ev.stopPropagation();
        const e = getElem(id);
        nextAnimClass(e, `fadein-${cls}`, `visible-${cls}`);
        nextAnimClass(e, `fadeout-${cls}`, `hidden-${cls}`);
    }
}

function nextAnimClass(e, cur, nxt) {
    const cs = e.classList;
    if (cs.contains(cur)) {
        cs.remove(cur);
        cs.add(nxt);
        trc(1, "nextAnim: cur=" + cur +", nxt=" + nxt + ", cs=" + cs.toString());
        return true;
    }
    return false;
}

function hideAnim(id, cls) {
    trc(1, `hideAnim: ${id} ${cls}`);
    const e = getElem(id);
    nextAnimClass(e, `fadein-${cls}`,  `fadeout-${cls}`);
    nextAnimClass(e, `visible-${cls}`, `fadeout-${cls}`);
}

function showAnim(id, cls) {
    trc(1, `showAnim: ${id} ${cls}`);
    const e = getElem(id);
    nextAnimClass(e, `fadeout-${cls}`, `fadein-${cls}`);
    nextAnimClass(e, `hidden-${cls}`,  `fadein-${cls}`);
}

function isHiddenAnim(id, cls) {
    trc(1, `isHiddenAnim: ${id} ${cls}`);
    const cs = getElem(id).classList;
    return cs.contains(`hidden-${cls}`) || cs.contains(`fadeout-${cls}`);
}

// ----------------------------------------
