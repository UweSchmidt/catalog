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

// import DOM.Manipulate

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
// simplest transition: exchange images without animation

function cut(id1, id2, dur, cls) {
    cls = cls || 'image';
    trc(1, "cut: " + id1);
    const e1 = getElem(id1);
    const e2 = getElem(id2);

    nextAnimClass(e2, `visible-${cls}`, `hidden-${cls}`);
    nextAnimClass(e1, `hidden-${cls}`, `visible-${cls}`);
    clearImageElem(e2);
}

// ----------------------------------------
// crossfade images

function crossFade(id1, id2, dur, cls) {
    const e1 = getElem(id1);
    const e2 = getElem(id2);
    trc(1, `crossFade: ${id1}, ${dur}sec`);

    setAnimDur(e2, dur);
    nextAnimClass(e2, `visible-${cls}`, `fadeout-${cls}`)
        || nextAnimClass(e2, `fadein-${cls}`, `fadeout-${cls}`);

    setAnimDur(e1, dur);
    nextAnimClass(e1, `hidden-${cls}`, `fadein-${cls}`);
}

// ----------------------------------------
// fadeout fadein

function fadeOutIn(id1, id2, dur, cls) {
    cls = cls || 'image';
    const e1 = getElem(id1);
    const e2 = getElem(id2);
    cls = cls || 'image';
    trc(1, `fadeOutIn: ${id1}, ${dur}sec`);

    setAnimDur(e2, dur);
    nextAnimClass(e2, `visible-${cls}`, `fadeout-${cls}`)
        || nextAnimClass(e2, `fadein-${cls}`, `fadeout-${cls}`);

    // setCSS(e1, {opacity: 0});
    setAnimDur(e1, dur, dur);
    nextAnimClass(e1, `hidden-${cls}`, `fadein-${cls}`);
}

// ----------------------------------------

function clearImageElem(e) {
    clearCont(e);
    clearAnimDur(e);
}

// ----------------------------------------
