// ----------------------------------------
//
// convenience functions for DOM manipulation


function newText(txt) {
    return document.createTextNode(txt);
}

function newElem0(tag) {
    return document.createElement(tag);
}

function getElem(id) {
    const e = document.getElementById(id);
    if (! e) {
        trc(1, "getElem: warning: no elem found for " + id);
    }
    return e;
}

// clear content of an element
// e may be an id of an element or the element itself

function clearCont(e) {
    if (typeof e === "string") {
        const e1 = getElem(e);
        if (e1 != null) {
            clearCont(e1);
        }
        return e1;
    } else {
        e.innerHTML = '';
        trc(1, "clearCont:" + JSON.stringify(e));
        return e;
    }
}

// set CSS attribute(s) in an element (identified by an id)

function setCSS(e, attrs, val) {
    if (typeof e === "string") {  // e is an id
        setCSS(getElem(e), attrs, val);
    }
    else if (typeof attrs == "string"
             &&
             typeof val === "string"
            ) {
        e.style[attrs] = val;
    }
    else if (typeof e === "object"
             &&
             typeof attrs === "object"
            ) {
        for (let a in attrs) {    // e is an element
            e.style[a] = "" + attrs[a];
        }
        return e;
    }
    else {
        throw ("setCSS: illegal arg types: (" +
               typeof e + "," +
               typeof attrs + "," +
               typeof val + ")");
    }
}

// set the size of an element and the position in parent element
// relative to left top corner

function setGeoCSS(id, p, o) {
    const px  = toPx(p);
    const css = { width : px.x,
                  height: px.y };
    if( o ) {
        ox = toPx(o);
        css.left = ox.x;
        css.top  = ox.y;
    }
    setCSS(id, css);
}

// ----------------------------------------

function setAnimDur(e, dur, delay) {
    const del = delay || 0;
    setCSS(e, {"animation-duration": dur + "s",
               "animation-delay":    del + "s"
              });
}

function clearAnimDur(e) {
    setCSS(e, {"animation-duration": null,
               "animation-delay":    null,
              });
}

// ----------------------------------------

// create an element with id attr, style attributes, and class attribues
function newElem(tag, x2, x3, x4) {
    let id = "";
    if (typeof x2 === "string") {  // elem id found
        id = x2;
        x2 = x3;
        x3 = x4;
    }
    let css = {};
    if (typeof x2 === "object") {  // style obj found
        css = x2;
        x2  = x3;
    }
    let cls = "";
    if (typeof x2 === "string") {  // css class found
        cls = x2;
    }

    const e = newElem0(tag);
    setCSS(e, css);                // set style
    if (id) {
        e.id = id;                 // set id
    }
    if (cls) {
        const clss = cls.split(" ");
        for (let i = 0; i < clss.length; i++ ) {
            const c = clss[i];
            if (c) {
                e.classList.add(c);
            }
        }
    }
    return e;
}

// clear elem contents and set size to 100%

function clearBlock(id) {
    const e      = clearCont(id);
    const dstyle = { width    : "100%",
                     height   : "100%"
                   };
    setCSS(e, dstyle);
    return e;
}

// --------------------

function setContents(id, cont) {
    document.getElementById(id).innerHTML = cont;
}

function setAttr(id, attr, val) {
    document.getElementById(id)[attr] = val;
}

function setStyles(id, attrs) {
    const e = document.getElementById(id);
    for (let a in attrs) {
        e.style[a] = attrs[a];
    }
}

function setStyle(id, attr, val) {
    document.getElementById(id).style[attr] = val;
}

function getStyle(id, attr) {
    return document.getElementById(id).style[attr];
}


// ----------------------------------------

function newImgElem(id, css, cls) {
    return newElem4("img", id, css, cls);
}

function newMovElem(id, css, cls) {
    return newElem4("video", id, css, cls);
}

function newBlogElem(id, css, cls) {
    return newElem4("div", id, css, cls);
}

function newElem4(elem, id, css, cls) {
    return newElem(elem, mkImgId(id), css, cls);
}

// ----------------------------------------
