/* Navigation in picture pages */

function trc (t, Text) {
  if ( t > 0 ) {
      console.log(Text);
      //    alert(Text);
    /* nice try
       if (window.statusbar && window.statusbar.visible == true) {
       window.status = Text;
       window.defaultStatus = Text;
       } else {
       alert(Text);
    }
    */
  }
}

// ----------------------------------------

/* global state, these variables will be threaded through URL search string */

var isPicture = true;
var slideShow = 0;
var speed     = 1000;

function initState() {
    var s = window.location.search;
    s = s.slice(1);
    var params = s.split("+");

    /* set default param values */
    slideShow     = 0;
    slideShowType = "";

    for (var i = 0; i < params.length; ++i) {
        var kv = params[i].split("=");
        var k = kv[0];
        var v = kv[1];
        if ( k == "slideShow" ) { slideShow     = parseInt(v); }
        if ( k == "speed" )     { speed         = parseInt(v); }
        if ( k == "stype" )     { slideShowType = v; }
    }
    trc(0, "init: " + "slideShow=" + slideShow + " (" + slideShowType + ")");
}

function stateToString () {
    var res = "?";
    res += "slideShow=" + slideShow;
    res += "+speed=" + speed;
    if (slideShow > 0) {
        res += "+stype=" + slideShowType;
    }
    return res;
}

// ----------------------------------------

var theNextPage = '';
var theNextPic  = new Image();

function changeToNextPage() {
  changePage(theNextPage);
}

function changePicPage(url,imgurl) {
  if (imgurl == '') {
    changePage(url);
  } else {
    /* load picture in advance and change to next page after picture is loaded */
    theNextPage = url;
    theNextPic.onload = changeToNextPage;
    theNextPic.src    = imgurl;
  }
}

function changePage(url) {
  trc(0, "changePage: " + "url=" + url);
  if (url != "") {
    window.location.href = url + stateToString();
  }
}

// open the collection in collection editor

var picno = -1;

function openEditPage() {
    var path = getPath(thisp);
    var url  = "edit-4.5.0.html?path=" + path + "&picno=" + picno;
    trc(1, "openEditPage: url=" + url);
    window.open(url, "_blank");
}

// compute collection path

function getPath(p) {
    var ps = p.split("/");

    ps.shift();
    ps.shift();
    ps.shift();
    ps.shift();
    ps.unshift("");    // remove "/doc/page/<w>x<<h>"

    var ns = ps.pop().split(".");
    ns.pop();
    var n  = ns.join(".");
    ps.push(n);       // remove ".html" extension

    var re = /^pic-\d{4,}$/;
    var pn = ps.pop();
    if (re.test(pn)) {
        picno = parseInt(pn.slice(4)); // set global var picno and drop last path name
    } else {
        ps.push(pn);  // restore last name in path
    }

    p = ps.join("/");
    trc(1, "getPath: path=" + p + " picno=" + picno);
    return p;
}

// ----------------------------------------

// variable take one of "pic-org", "pic-scaled", "pic-pano"
// pic-scaled indicates the image scaled down to display size is shown

var picstate = "pic-scaled";

function toggleOriginalPicture() {
    console.log("toggleOriginalImage");

    // no ref to full size picture
    if (orgimg === "") {
        return;
    }

    // turn off full size view
    if (picstate === "pic-org") {
        switchToScaledPicture();
        return;
    }

    // turn off other views (pano)
    if (picstate !== "pic-scaled") {
        switchToScaledPicture();
    }

    // load full size and switch to full size view
    var dv  = $('#pic-org');
    var img = $('#pic-org img');
    if (! img.attr('src') ) {
        // load image on demand
        img.attr('src', orgimg);
        console.log(img.attr('src'));
        img.on('load', function(){
            // image ready: make it visible
            toggleOriginalPicture();
        });
    } else {
        // make image visible
        console.log( "make org picture visible");
        hideDiv('#' + picstate);
        showDiv('#pic-org');
        picstate="pic-org";
    }
}

function cssKeyFrames(isHorizontalPano, pos) {
    var dir = isHorizontalPano ? "left" : "bottom";
    var keyframes
            = "@keyframes moveRightOrUp {\
                 0% {" + dir + ": 0px;}\
                40% {" + dir + ": " + pos + "px;}\
                60% {" + dir + ": " + pos + "px;}\
               100% {" + dir + ": 0px;}\
               }";

    $("<style>")
        .prop("type", "text/css")
        .html(keyframes)
        .appendTo("head");
}

function togglePanoAnimation() {
    // no ref to panorama picture
    if (panoimg === ""
        ||
        picstate !== "pic-pano"
       ) {
        return;
    }
    var img = $('#pic-pano img');
    var as  = img.css('animation-play-state');
    console.log("animation-play-state=" + as);
    if (as === 'running') {
        as = 'paused';
    } else {
        as = 'running';
    }
    img.css('animation-play-state', as);
}

function togglePanoramaPicture() {
    // no ref to panorama picture
    if (panoimg === "") {
        return;
    }

    // turn off panorama view
    if (picstate === "pic-pano") {
        switchToScaledPicture();
        return;
    }

    // turn off other views
    if (picstate !== "pic-scaled") {
        switchToScaledPicture();
    }

    // load pano and switch to pano view
    var dv  = $('#pic-pano');
    var img = $('#pic-pano img');
    if (! img.attr('src') ) {
        // load image on demand
        img.attr('src', panoimg);
        console.log(img.attr('src'));
        img.on('load', function(){
            // add @keyframes for animation
            var pw = img[0].naturalWidth;
            var ph = img[0].naturalHeight;
            var isHorizontalPano = pw > ph;
            console.log("PanoGeo=" + pw + "x" + ph);

            var sw = dv.width();
            var sh = dv.height();
            console.log("DisplayGeo=" + sw + "x" + sh);


            var du = 2.5 *  // 40% move to right, 20% pause, 40% move back
                     7.0 *  // anim duration is 7 sec
                            // scale anim duration with aspect ratio
                     (isHorizontalPano ? pw / ph : ph / pw);
            console.log("AnimDuration=" + du);
            img.css('animation-duration', "" + du + "s");

            if ( isHorizontalPano ) {
                cssKeyFrames(isHorizontalPano, sw - pw);
                img.css('top',   "0px");
            } else {
                cssKeyFrames(isHorizontalPano, sh - ph);
                img.css('bottom', "0px");
            }

            // image ready: make it visible
            togglePanoramaPicture();
        });
    } else {
        // make image visible
        console.log("make pano picture visible");
        hideDiv('#' + picstate);
        showDiv('#pic-pano');
        picstate="pic-pano";
        console.log("start animation");
        img.css('animation-play-state', 'running');
    }
}

function hideDiv(s) {
    $(s).css('display', 'none');
}

function showDiv(s) {
    $(s).css('display', 'block');
}

function switchToScaledPicture() {
    if (picstate === "pic-scaled") {return;}
    hideDiv('#' + picstate);
    showDiv('#pic-scaled');
    picstate="pic-scaled";
}

function toggleMute() {
    var video = $('#pic-movie');
    var mv    = ! video.prop('muted');

    video.prop('muted', mv);
    console.log("toggle video muted, value is now " + mv);
}

function toggleControls() {
    var video = $('#pic-movie');
    var mv    = ! video.prop('controls');

    video.prop('controls', mv);
    console.log("toggle video controls, value is now " + mv);
}

// ----------------------------------------

var slideShowTimer;
var slideShowType = "";

function advanceSlideShow() {
    trc(1, "advance SlideShow");
    if ( slideShowType == "allColls") {
        fwrdPage();
    } else {
        nextPage();
    }
}

function stopSlideShow () {
    if (typeof slideShowTimer != "undefined") {
        window.clearTimeout(slideShowTimer);
        trc(1, "timer cleared");
    }
    slideShow     = 0;
    slideShowType = "";
    trc(1, "slideShow stopped");
}

function startSlideShow () {
    var d = 5000;
    if ( typeof duration == "number" ) {
        d = duration;
    }
    d = (d * speed) / 1000;
    slideShowTimer = window.setTimeout(advanceSlideShow, d);
    slideShow = 1;
    trc(1, "SlideShow started with msec: " + d + " (" + slideShowType + ")");
}

function startStopSlideShow(stype) {
    duration = 1000;
    slideShowType=stype;
    toggleSlideShow();
}

function toggleSlideShow() {
  if (slideShow > 0) {
    stopSlideShow();
  } else {
    startSlideShow();
  }
}

function initSpeedSlideShow() {
  speed = 1000;
}

function slowDownSlideShow() {
  speed = Math.round(speed * 1.2);
}

function speedUpSlideShow() {
  speed = Math.round(speed / 1.2);
  if ( speed < 50 ) { speed = 50; }
}

function initSlideShow () {
    if ( isPicture || slideShowType == "allColls" ) {
        if (slideShow > 0) {
            slideShow = 0;
        } else {
            slideShow = 1;
        }
        toggleSlideShow();
    } else {
        stopSlideShow();
    }
}

var opacity         = "0.8";
var backgroundColor = "#666666";

var titleVisible = false;
var infoVisible  = false;
var helpVisible  = false;

function toggleTitle() {
    if (titleVisible) {
        hideTitleElement();
    } else {
        showTitleElement();
    }
}

function showTitle() {
    /* showTitleElement(); /* disable mouse over effects */
}

function hideTitle() {
    /* hideTitleElement(); /* disable mouse over effects */
}

function showTitleElement() {
    if (! titleVisible) {
        stopSlideShow();
        hideInfoElement();
        hideHelpElement();
        $('#title-area-line')
            .css('visibility',      "visible")
            .css('opacity',         opacity)
            .css('backgroundColor', backgroundColor);

        titleVisible = true;
    }
}

function hideTitleElement() {
    if (titleVisible) {
        $('#title-area-line')
            .css('visibility',      '')
            .css('opacity',         '')
            .css('backgroundColor', '');

        titleVisible = false;
    }
}

function toggleInfo() {
    if (infoVisible) {
        hideInfoElement();
    } else {
        showInfoElement();
    }
}

function showInfoElement() {
    if (! infoVisible) {
        stopSlideShow();
        hideTitleElement();
        hideHelpElement();
        $('#info-area-content')
            .css('visibility',      "visible")
            .css('opacity',         opacity)
            .css('backgroundColor', backgroundColor);

        infoVisible = true;
    }
}

function hideInfoElement() {
    if (infoVisible) {
        $('#info-area-content')
            .css('visibility',      '')
            .css('opacity',         '')
            .css('backgroundColor', '');

        infoVisible = false;
    }
}

function toggleHelp() {
    if (helpVisible) {
        hideHelpElement();
    } else {
        showHelpElement();
    }
}

function showHelpElement() {
    if (! helpVisible) {
        stopSlideShow();
        hideTitleElement();
        hideInfoElement();
        $('#help-area-content')
            .css('visibility',      "visible")
            .css('opacity',         opacity)
            .css('backgroundColor', backgroundColor);

        helpVisible = true;
    }
}

function hideHelpElement() {
    if (helpVisible) {
        $('#help-area-content')
            .css('visibility',      '')
            .css('opacity',         '')
            .css('backgroundColor', '');

        helpVisible = false;
    }
}

function isKey(e, c, s) {
    if ((e.keyCode == 0
         ||
         e.keyCode == e.which
        )
        &&
        e.which == c
       ) { return true;}
    return false;
}

function keyCodeToString(e, c) {
    if ((e.keyCode == 0
         ||
         e.keyCode == e.which
        )
        &&
        e.which == c
       ) { return String.fromCharCode(c); }
    return "";
}

function keyUp(e) {
    if (! e)
        e = window.event;

    console.log("keyUp: KeyCode=" + e.keyCode + " which=" + e.which);

    if ( (e.keyCode == 39)   /* right arrow */
         ||
         (e.keyCode == 34)   /* page down, presenter: right arrow */
       ) {
        stopSlideShow();
        nextPage();
        return false;
    }
    if ( (e.keyCode == 37)   /* left arrow */
         ||
         (e.keyCode == 33)   /* page up, presenter: left arrow */
         ||
         (e.keyCode == 8)    /* backspace*/
       ) {
        stopSlideShow();
        prevPage();
        return false;
    }
    if ( (e.keyCode == 27)     /* escape, presenter: left screen icon */
         ||                    /* presenter: left screen: 116, 27, 116, 27, ... */
         (e.keyCode == 116)    /* F5, presenter: left screen icon */
         ||
         (e.keyCode == 38)     /* up arrow */
       ) {
        stopSlideShow();
        parentPage();
        return false;
    }
    if ( (e.keyCode == 40)     /* down arrow */
         ||
         (e.keyCode == 190)    /* '.' , presenter right screen icon */
       ) {
        stopSlideShow();
        child1Page();
        return false;
    }
}

function keyPressed (e) {
    if (! e)
        e = window.event;

    console.log("keyPressed: KeyCode=" + e.keyCode + " which=" + e.which);

    if ( isKey(e, 32, " ")
         ||
         isKey(e, 62, ">")
         ||
         isKey(e, 110, "n")
       ) {
        stopSlideShow();
        nextPage();
        return false;
    }

    if ( isKey(e, 60, "<")
         ||
         isKey(e, 112, "p")
       ) {
        stopSlideShow();
        prevPage();
        return false;
    }

    if ( isKey(e, 94, "^")
         ||
         isKey(e, 117, "u")
       ) {
        stopSlideShow();
        parentPage();
        return false;
    }

    if ( isKey(e, 118, "v")
         ||
         isKey(e, 100, "d")
       ) {
        child1Page();
        return false;
    }

    if ( isKey(e, 115, "s") ) {
        startStopSlideShow("thisColl");
        return false;
    }

    if ( isKey(e,  83, "S") ) {
        startStopSlideShow("allColls");
        return false;
    }

    if ( isKey(e, 116, "t") ) {
        toggleTitle();
        return false;
    }

    if ( isKey(e, 105, "i") ) {
        toggleInfo();
        return false;
    }

    if ( isKey(e, 43, "+") ) {
        speedUpSlideShow();
        return false;
    }

    if ( isKey(e, 45, "-") ) {
        slowDownSlideShow();
        return false;
    }

    if ( isKey(e, 48, "0") ) {
        initSpeedSlideShow();
        return false;
    }

    if ( isKey(e, 102, "f") ) {
        toggleOriginalPicture();
        return false;
    }

    if ( isKey(e, 97, "a") ) {
        togglePanoramaPicture();
        return false;
    }

    if ( isKey(e, 113, "q") ) {
        togglePanoAnimation();
        return false;
    }

    if ( isKey(e, 101, "e") ) {
        openEditPage();
        return false;
    }

    if ( isKey(e, 109, "m") ) {
        toggleMute();
        return false;
    }

    if ( isKey(e, 99, "c") ) {
        toggleControls();
        return false;
    }

    s = keyCodeToString(e.keyCode);
    if ( s != "" ) {
        toggleHelp();
        return false;
    }


    return true;
}

function nextPage() {
    if ( nextp != '' ) {
        changePicPage(nextp,nextimg);
    } else {
        parentPage();
    }
}

function fwrdPage() {
    if ( fwrdp != '' ) {
        nextp   = fwrdp;
        nextimg = fwrdimg;
        nextPage();
    }
    else {
        stopSlideShow();
    }
}

function prevPage() {
    if ( prevp != '' ) {
        changePicPage(prevp,previmg);
    } else {
        parentPage();
    }
}

function parentPage() {
    if ( parentp != '' ) {
        changePage(parentp);
    }
}

function thisPage() {
    changePage(thisp);
}

function child1Page() {
    changePage(childp);
}

function childPage(ref) {
    changePage(ref);
}

function initPicture() {
    isPicture = true;
    initState();
    initSlideShow();
}

function initMovie() {
    initPicture();
}

function initAlbum() {
    isPicture = false;
    initState();
    initSlideShow();
}

document.onkeypress = keyPressed;
document.onkeyup    = keyUp;
