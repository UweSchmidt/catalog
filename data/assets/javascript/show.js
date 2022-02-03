/* show.js single page slideshow */

function trc (t, Text) {
    if ( t > 0 ) {
        console.log(Text);
    }
}

/* ---------------------------------------- */
/* id's */

const imgTab ="imageTab";
const colTab ="collectionTab";
const img1   = "image1";
const img2   = "image2";

/* ---------------------------------------- */
/* basic operations */

function div(x, y) {
    return Math.floor(x/y);
}

/* ---------------------------------------- */
/* geometry ops */

function readGeo(txt) {
    if (txt === "org")
        return { w : 1, h : 1 };

    const a = txt.split('x');
    return { w : 1 * a[0],
             h : 1 * a[1]
           };
}

function showGeo(geo) {
    if (geo.w === 1 && geo.h === 1)
        return "org";
    return "" + geo.w + "x" + geo.h;
}

function toPx(obj) {
    const res = {};
    for (k in obj) {
        res[k] = obj[k] + "px";
    }
    return res;
}

function shrinkGeo(s, d) {
    if (s.w <= d.w && s.h <= d.h)
        return s;
    else
        return resizeGeo(s, d);
}

function resizeGeo(s, d) {
    if (s.w * d.h >= d.w * s.h)
        return { w : d.w,
                 h : div(d.w * s.h, s.w)
               };
    else
        return { w : div(d.h * s.w, s.h),
                 h : d.h
               };
}

function screenGeo() {
    return { w : screen.availWidth  / 2,  // just for testing
             h : screen.availHeight / 2
           };
}

function fitToScreenGeo(geo) {
    return shrinkGeo(geo, screenGeo());
}

function placeOnScreen(geo) {
    const sg = screenGeo();
    return { x : div(sg.w - geo.w, 2),
             y : div(sg.h - geo.h, 2)
           };
}

const geoOrg = readGeo("org");

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

function bestFitToScreenGeo () {
    const s = screenGeo();
    for (let v of serverSupportedGeos) {
        const g = readGeo(v);
        if (g.w >= s.w && g.h >= s.h)
            return g;
    }
    return geoOrg;
}

/* ---------------------------------------- */
/* urls */

function imgReqToUrl(imgReq) {
    const pp = rPathPosToUrl(imgReq.rPathPos);
    return "/docs/"
        + imgReq.rType
        + "/"
        + showGeo(bestFitToScreenGeo())
        + pp
        + ".jpg";
}

function rPathPosToUrl(rPathPos) {
    const pos = rPathPos[1];
    return rPathPos[0] + posToUrl(rPathPos[1]);
}

function posToUrl(pos) {
    if (pos) {
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

/* ---------------------------------------- */

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

/* ---------------------------------------- */

function showElem(id) {
    setStyle(id, "display", "block");
}

function hideElem(id) {
    setStyle(id, "display", "none");
}

function showColTab() {
    hideElem(imgTab);
    showElem(colTab);
}

function showImg2() {
    hideElem(img1);
    showElem(img2);
}

function isHidden(id) {
    return getStyle(id, "display") === "none";
}

function changeElems(id1, id2) {
    hideElem(id2);
    showElem(id1);
}

function toggleElems(id1, id2) {
    if (isHidden(id1)) {
        changeElems(id1,id2);
    } else {
        changeElems(id2,id1);
    }
}

function toggleImg12()  { toggleElems(img1,   img2);   }
function toggleImgCol() { toggleElems(imgTab, colTab); }

function nextImgId() {
    return isHidden(img1) ? img1 : img2;
}

/* ---------------------------------------- */
/* global state */

var lastPage;
var currPage;

var picCache = new Image();

/* ---------------------------------------- */
/* initialization */

function initShow() {
    const g = toPx(screenGeo());
    console.log("initShow: screen geo=" + showGeo(g));

    setStyles(imgTab, {width : g.w, height : g.h});
    setStyles(colTab, {width : g.w});
}

// ----------------------------------------
// display an ordinary image

function loadImg(id, url, geo) {
    const imgGeo = fitToScreenGeo(geo);
    const offset = toPx(placeOnScreen(imgGeo));
    const g      = toPx(imgGeo);
    const style  = { width  : g.w,
                     height : g.h,
                     left   : offset.x,
                     top    : offset.y
                   };
    setStyles(id, style);

    const id1   = id + "-img";
    const ielem = "<image id='" + id1 + "'>";
    setContents(id, ielem);
    const g1  = { width  : g.w,
                  height : g.h
                };
    setAttr(id1, "src", url);
    setStyles(id1, g1);
}

function showImg(page) {
    const imgReq = page.imgReq;
    const imgGeo = readGeo(page.oirGeo[0]);
    const imgUrl = imgReqToUrl(imgReq);

    trc(1, "showImg: url=" + imgUrl + ", geo=" + showGeo(imgGeo));

    picCache.onload = () => {
        loadImg(nextImgId(), imgUrl, imgGeo);
        toggleImg12();
    };
    picCache.src = imgUrl; // the .onload handler is triggered here
}

// ----------------------------------------

function loadMovie(id, url, geo) {
    const movGeo = fitToScreenGeo(geo);
    const offset = toPx(placeOnScreen(movGeo));
    const g      = toPx(movGeo);
    const style  = { width  : g.w,
                     height : g.h,
                     left   : offset.x,
                     top    : offset.y
                   };
    setStyles(id, style);

    const id1   = id + "-img";
    const ielem = "<video id='" + id1 + "' autoplay muted>";
    setContents(id, ielem),

    setAttr(id1, "width",  movGeo.w);
    setAttr(id1, "height", movGeo.h);
    const selem = "<source src='" + url + "' type='video/mp4'>";
    const warn  = "<span> your browser does not support HTML5 video</span>";
    setContents(id1, selem + warn);
}

function showMovie(page) {
    const movReq = page.imgReq;
    const movGeo = readGeo(page.oirGeo[0]);
    const movUrl = toMediaUrl(page.img);

    trc(1, "showMovie: url=" + movUrl + ", geo=" + showGeo(movGeo));

    loadMovie(nextImgId(), movUrl, movGeo);
    toggleImg12();
}

function showBlog(page) {
    trc(1, "showBlog: not yet implemented");
}

function showCol(page) {
    trc(1, "showCol: not yet implemented");
}

function showPage(page) {
    const rty = page.imgReq.rType;
    if (rty == "col") {
        showCol(page);
    }
    else if (rty === "movie") {
        showMovie(page);
    }
    else if (rty === "blog") {
        showBlog(page);
    }
    else if (rty ) {
        showImg(page);
    }
    else {
        trc(1, "illegal rType " + rty);
    }

}

/* ---------------------------------------- */

const u1 = "/docs/iconp/900x600/archive/collections/photos/tests/pic-0012.jpg";
const u2 = "/docs/img/160x120/archive/collections/photos/tests/pic-0003.jpg";

function ttt() {
    loadImg(img1, u1, readGeo("900x600"));
}
function sss() {
    loadImg(img2, u2, readGeo("160x120"));
}




/* ---------------------------------------- */

const imgPage =
      {
          "imgReq": {
              "rType": "img",
              "rPathPos": [
                  "/archive/collections/albums/EinPaarBilder",
                  1
              ]
          },
          "oirGeo": [
              "3894x2592",   // org size
              "1280x800",    // requested size
              "1201x800"     // scaled down size
          ],
          "img": [
              "/archive/photos/2001/xxx-Stockente-1",
              "xxx-Stockente-1.png",
              {
                  "Composite:ImageSize": "3894x2592",
                  "Composite:Megapixels": "10.1",
                  "Descr:Duration": "1.0",
                  "Descr:Title": "xxx-Stockente-1.png",
                  "File:FileSize": "13 MB",
                  "File:MimeType": "image/png",
                  "File:Name": "xxx-Stockente-1.png",
                  "File:RefImg": "/photos/2001/xxx-Stockente-1.png",
                  "File:RefJpg": "/docs/img/1280x800/archive/collections/albums/EinPaarBilder/pic-0001.jpg",
                  "File:RefMedia": "/docs/img/1280x800/archive/collections/albums/EinPaarBilder/pic-0001.jpg",
                  "File:TimeStamp": "1575482988",
                  "Img:EXIFUpdate": "1610735082"
              }
          ],
          "imgNavRefs": {
              "prev": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      0
                  ]
              },
              "next": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              },
              "par": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      null
                  ]
              },
              "fwrd": {
                  "rType": "json",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              }
          },
          "imgNavImgs": {
              "prev": {
                  "rType": "img",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      0
                  ]
              },
              "next": {
                  "rType": "img",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              },
              "par": null,
              "fwrd": {
                  "rType": "img",
                  "rPathPos": [
                      "/archive/collections/albums/EinPaarBilder",
                      2
                  ]
              }
          },
          "blogCont": "",
          "now": "2022-02-03T15:11:47"
      };

const movPage = {
  "imgReq": {
    "rType": "movie",
    "rPathPos": [
      "/archive/collections/photos/2018/2018-09-14.iPhone",
      0
    ]
  },
  "img": [
    "/archive/photos/2018/2018-09-14.iPhone/IMG_0009",
    "IMG_0009.mp4",
    {
      "Composite:ImageSize": "608x1080",
      "Composite:Megapixels": "0.657",
      "Descr:Duration": "1.0",
      "Descr:Title": "IMG_0009.mp4",
      "File:FileSize": "3.3 MB",
      "File:MimeType": "video/mp4",
      "File:Name": "IMG_0009.mp4",
      "File:RefImg": "/photos/2018/2018-09-14.iPhone/IMG_0009.mp4",
      "File:RefJpg": "/archive/photos/2018/2018-09-14.iPhone/IMG_0009.mp4",
      "File:RefMedia": "/archive/photos/2018/2018-09-14.iPhone/IMG_0009.mp4",
      "File:TimeStamp": "1592056240",
      "Img:EXIFUpdate": "1610735084",
      "QuickTime:Duration": "14.44 s",
      "QuickTime:ImageHeight": "1080",
      "QuickTime:ImageWidth": "608",
      "QuickTime:VideoFrameRate": "29.97"
    }
  ],
  "imgNavRefs": {
    "prev": null,
    "next": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    },
    "par": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        null
      ]
    },
    "fwrd": {
      "rType": "json",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    }
  },
  "imgNavImgs": {
    "prev": null,
    "next": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    },
    "par": null,
    "fwrd": {
      "rType": "img",
      "rPathPos": [
        "/archive/collections/photos/2018/2018-09-14.iPhone",
        1
      ]
    }
  },
  "oirGeo": [
    "608x1080",
    "1x1",
    "0x1"
  ],
  "blogCont": "",
  "now": "2022-02-03T18:16:57"
}
