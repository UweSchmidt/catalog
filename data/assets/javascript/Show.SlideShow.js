// ----------------------------------------
//
// slideshow stuff

const slideShowState = {
    running     : false,
    timer       : undefined,
    type        : "",
    speed       : 5000,
    defaultSpeed: 5000,  // 5 sec.

};

function slideDur() {
    const md = getPageMeta();
    const d  = md["Descr:Duration"];
    let   t  = 1; // seconds
    if (d) {
        t = d * 1;         // convert to number
        if (!t) { t = 1;}  // no conv: reset to default
    }
    return t * slideShowState.speed;
}

function advanceSlideShow() {
    trc(1, "advance SlideShow");
    const hasNext = ( slideShowState.type == "allColls")
          ? goForward()
          : ( isColPage()
              ? gotoChild(0)
              : gotoNext()
            );
    if (! hasNext) {
        stopSlideShow();
        gotoPar();
    } else {
        const ms = slideDur();
        slideShowState.timer = setTimeout(advanceSlideShow, ms);
        trc(1, "advanceSlideShow timer set msec: " + ms + " (" + slideShowState.type + ")");
    }
}

function stopSlideShow() {
    if (slideShowState.running) {
        if (slideShowState.timer != undefined) {
            clearTimeout(slideShowState.timer);
            trc(1, "timer cleared");
        }
        slideShowState.running      = false;
        slideShowState.type  = "";
        slideShowState.timer = undefined;
        statusBar.show("Automatischer Bildwechsel beendet");
    }
}

function startSlideShow() {
    if (! slideShowState.running) {
        slideShowState.running = true;
        statusBar.show("Automatischer Bildwechsel gestartet");
        advanceSlideShow();
    }
}

function startStopSlideShow(stype) {
    slideShowState.type = stype;
    toggleSlideShow();
}

function toggleSlideShow() {
    if (slideShowState.running) {
        stopSlideShow();
    } else {
        startSlideShow();
    }
}

function resetSpeedSlideShow() {
    slideShowState.speed = slideShowState.defaultSpeed;
    showDur();
}

function slowDownSlideShow() {
    slideShowState.speed = slideShowState.speed * 1.2;
    showDur();
}

function speedUpSlideShow() {
    slideShowState.speed = Math.max(slideShowState.speed / 1.2, 2500);
    showDur();
}

function showDur() {
    const s =  Math.round(slideShowState.speed / 100) / 10;
    statusBar.show('Automatischer Bildwechsel nach ' + s + " sec.");
}

// ----------------------------------------
