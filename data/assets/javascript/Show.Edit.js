// ----------------------------------------
//
// editable zoom images (experimental)

// ----------------------------------------

// import DOM.Manipulate
// import Show.Zoom

// ----------------------------------------

var editMode  = false; // true;

function showHideEditTab () {
    if ( layout.theEditTabIsVisible ) {
        setCSS( "editTab",
                { width: toPx(layout.theScreenGeo.x - layout.theImgTabGeo.x),
                  visibility: "visible",
                }
              );
    }
    else {
        setCSS( "editTab", { visibility: "hidden" });
    }
}

function toggleEditTab () {
    if ( ! layout.theEditTabIsVisible ) {
        layout.theEditTabIsVisible = true;
        layout.theEditTabWidth     = 0.2;
        layout.theImgTabIsCentered = false;
    }
    else {
        layout.theEditTabIsVisible = false;
        layout.theEditTabWidth     = 0.0;
        layout.theImgTabIsCentered = true;
    }
    setSizeImgTab();
    showHideEditTab();
    stayHere();
}


function editModeOn() {
    if ( ! editMode ) {
        editMode = true;
        trc(1, "editModeOn");
    }
}

function editModeOff() {
    if ( editMode ) {
        editMode = false;
        trc(1, "editModeOff");
    }
}

function editKeyPressed(e) {
    const handled = handleKeyPressed(e);
    if ( handled ) {
        e.stopPropagation();
    }
    else {
        keyPressed(e);
    }
}

function handleKeyPressed(e) {
    if ( ! editMode ) {
        return false;
    }
    if ( ! isZoomImg() ) {
        return false;
    }
    const k = e.key;
    trc(1,`handleKeyPressed: ${k}`);

    // img resize

    if ( k === "+" || k === "*" ) {
        blowupImg(k === "*");
        return true;
    }
    if ( k === "-" || k === "_") {
        shrinkImg(k === "_");
        return true;
    }

    if ( k === "0") {
        zoomImgFit();
        return true;
    }

    if ( k === "1") {
        zoomImgFill();
        return true;
    }
    if ( k === "2") {
        zoomImg1();
        return true;
    }

    // img move
    if ( k === "l" || k === "L") {
        backwardImg(k == "L");
        return true;
    }
    if ( k === "r" || k === "R") {
        forwardImg(k == "R");
        return true;
    }
    if ( k === "u" || k === "U") {
        upwardImg(k == "U");
        return true;
    }
    if ( k === "d" || k === "D") {
        downwardImg(k == "D");
        return true;
    }

    return false;
}

function zmstep(smallStep) { return smallStep ? 1.01 : 1.1; }

function blowupImg(smallStep) {
    const s = zmstep(smallStep);
    zoomImg(zoomState.scale * s);
}
function shrinkImg(smallStep) {
    const s = zmstep(smallStep);
    zoomImg(zoomState.scale / s);
}

function mvstep(smallStep) { return smallStep ? 0.005 : 0.05; }

function forwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(s, 0)));
}
function backwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(0 - s, 0)));
}
function upwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(0, 0 - s)));
}
function downwardImg(smallStep) {
    const s = mvstep(smallStep);
    zoomImg(null, addV2(zoomState.shift, V2(0, s)));
}

// ----------------------------------------
