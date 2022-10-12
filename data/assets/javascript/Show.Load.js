// ----------------------------------------
//
// load media and build DOM elements

// import Prelude
// import Data.V2
// import Data.Geo
// import Catalog.Urls
// import DOM.Manipulate

// ----------------------------------------
// page access functions

function getPageType(page) {
    return getPageReq(page).rType;
}

function getPageReq(page) {
    return isColPage(page)
        ? page.colDescr.eReq
        : page.imgReq;
}

function getPageMeta(page) {
    return isColPage(page)
        ? page.colDescr.eMeta
        : page.img[2];
}

function getPageBlog(page) {
    const md = getPageMeta(page);   // new blog access: blog is part of metadata
    const bt = md["Descr:Blog"];
    if (bt) {
        return bt;
    } // else {return "";}         // TODO cleanup, when server has been updated
    return page.blogCont;      // old blog access: blog is in blogCont field;
}

function getNavReq(nav, page) {
    return isColPage(page)
        ? page.navIcons[nav].eReq
        : page.imgNavRefs[nav];
}

function isColPage(page) {
    return ! page.imgReq;
}

function isImgPage(page) {
    return ! isColPage(page);
}

// --------------------

function jsonReqToUrl1(frameGeo, req) {
    const req1 = mkRequest('json',
                           req.rPathPos,
                           showGeo(bestFitToGeo(frameGeo)));
    return reqToUrl(req1);
}

// --------------------
// function for open catalog edit in new tab

function openEditPage(path, pos) {
    var url = "edit-4.5.0.html"
        + "?path=" + path
        + ( pos
            ? "&picno=" + picno
            : ""
          );
    trc(1, "openEditPage: url=" + url);
    window.open(url, "_blank");
}

// ----------------------------------------
// build an image element for a given resizeAlg

function loadAnImg(id, frameGeo, req, geo, resizeAlg) {
    if (resizeAlg === "fullsize") {
        loadFullImg(id, frameGeo, req, geo);
    }
    else if (resizeAlg === "panorama") {
        loadPanoramaImg(id, frameGeo, req, geo);
    }
    else if (resizeAlg === "zoom") {
        loadZoomableImg(frameGeo, id, req, geo);
    }
    else {
        loadImg(id, frameGeo, req, geo, resizeAlg);
    }
}

// ----------------------------------------
// build ordinary image page

function loadImg(id, frameGeo, url, geo, resizeAlg) {
    const imgGeo = fitToFrameGeo(frameGeo, geo, resizeAlg);
    const o      = toPx(placeOnFrame(frameGeo, imgGeo));
    const g      = toPx(imgGeo);

    const istyle = { width    : g.x,
                     height   : g.y,
                     left     : o.x,
                     top      : o.y,
                     position : "absolute",
                     overflow : "hidden"
                   };
    // get element, clear contents and set style attributes
    const e = clearBlock(id);
    const i = newImgElem(id, istyle, "img " + resizeAlg);
    i.src   = url;
    e.appendChild(i);
}

// ----------------------------------------
// build an element for a scrollable image in original resolution

function loadFullImg(id, frameGeo, url, imgGeo) {
    const off    = toPx(zeroV2);
    const g      = toPx(imgGeo);

    const istyle = { width    : g.x,
                     height   : g.y,
                     left     : off.x,
                     top      : off.y,
                     position : "absolute",
                     overflow : "auto"     // !!! image becomes scrollable
                   };
    // get element, clear contents and set style attributes
    const e = clearBlock(id);
    const i = newImgElem(id, istyle, "img fullsize");
    i.src   = url;
    e.appendChild(i);
}


// ----------------------------------------
// build element for panorama image

function loadPanoramaImg(id, frameGeo, url, imgGeo) {

    const isH    = isHorizontal(imgGeo);
    const ar     = aspectRatioV2(imgGeo);
    const offset = isH ? frameGeo.x - imgGeo.x : frameGeo.y - imgGeo.y;
    const d = 2.5 * 7.0 * Math.max(ar, 1/ar);

    // add keyframe style
    const s = clearCont(panoCss);

    const kf  = "pano-move";
    const dr  = isH ? "left" : "bottom";
    const dr1 = isH ? "top"  : "left";

    // css for animated panoramas
    const cssKeyFrames = `
          @keyframes ${kf} {
                0% {${dr}: 0px}
               45% {${dr}: ${offset}px}
               55% {${dr}: ${offset}px}
              100% {${dr}: 0px}
          }
          `;

    const cssPanoClass = `
          img.panorama {
              position: absolute;
              ${dr}:     0px;
              ${dr1}:    0px;
              animation-name:            ${kf};
              animation-duration:        ${d}s;
              animation-delay:           2s;
              animation-iteration-count: 1;
              animation-timing-function: ease-in-out;
              animation-play-state:      paused;
          }
          `;

    s.appendChild(newText(cssKeyFrames + cssPanoClass));

    function animationEndFunction() {
        trc(1, "animation of panorama has finished");
    }

    const e = clearBlock(id);
    const i = newImgElem(id, {}, "img panorama");
    i.addEventListener("load", togglePanoAnimation);
    i.addEventListener("animationend", animationEndFunction);
    i.src = url;
    e.appendChild(i);
}

// ----------------------------------------
// build movie page

function loadMovie(id, frameGeo, page, resizeAlg) {
    const movReq = page.imgReq;
    const movGeo = readGeo(page.oirGeo[0]);
    const movUrl = toMediaUrl(page.img);

    trc(1, "loadMovie: url=" + movUrl + ", geo=" + showGeo(movGeo));

    loadMovie1(id, frameGeo, movUrl, movGeo, movReq.rType, resizeAlg);
}

function loadMovie1(id, frameGeo, url, geo, rType, resizeAlg) {
    const movGeo  = fitToFrameGeo(frameGeo, geo, resizeAlg);
    const off     = toPx(placeOnFrame(frameGeo, movGeo));
    const g       = toPx(movGeo);
    const istyle  = { width    : g.x,
                      height   : g.y,
                      left     : off.x,
                      top      : off.y,
                      position : "absolute",
                      overflow : "hidden"
                   };

    const e = clearBlock(id);

    // build video/img element

    if (rType === "movie") {
        const v    = newMovElem(id, istyle, "movie video " + resizeAlg);
        v.width    = movGeo.x;
        v.height   = movGeo.y;
        if (videoAttrs.autoplay != null) {
            v.autoplay = "autoplay";
        }
        if (videoAttrs.controls != null) {
            v.controls = "controls";
        }
        if (videoAttrs.muted != null) {
            v.muted = "muted";
        }

        const s = newElem("source");
        s.src  = url;
        s.type = "video/mp4";

        const w = newElem("span")
                  .appendChild(
                      newText("your browser does not support HTML5 video"));
        v.appendChild(s).appendChild(w);

        // insert new content into element
        e.appendChild(v);

    }
    else if (rType === "gif") {
        const v2 = newImgElem(id,
                              { width:  g.x,
                                height: g.y
                              },
                              "movie gif " + resizeAlg
                             );
        v2.src   = url;

        e.appendChild(v2);
    }
}

// ----------------------------------------
// build a blog page

function loadBlog(id, frameGeo, page) {
    const req = page.imgReq;
    const geo = toPx(frameGeo);
    const txt = getPageBlog(page);

    trc(1, "loadBlog: " + txt);

    // get element, clear contents and set style attributes
    const e  = clearBlock(id);

    // build blog contents div
    const b  = newBlogElem(id,
                           { height:   "100%",
                             overflow: "auto"
                           },
                           "blog"
                          );
    b.innerHTML = txt;
    e.appendChild(b);
}

// ----------------------------------------
// build collection page

function loadCollection(id, frameGeo, page) {
    const colDescr = page.colDescr;
    const colReq   = colDescr.eReq;
    const colMeta  = colDescr.eMeta;
    const colBlog  = getPageBlog(page);
    const navIcons = page.navIcons;
    const c1Icon   = page.c1Icon;
    const colIcons = page.contIcons;
    const iconReq  = { rType: "icon",
                       rPathPos: colReq.rPathPos
                     };
    const g        = toPx(frameGeo);
    const e        = clearCont(id);
    setCSS(e, { width:    g.x,
                height:   g.y,
                left:     "0px",
                top:      "0px",
                overflow: "auto"
              });
    e.appendChild(
        buildCollection(frameGeo,
                        colReq, iconReq,
                        colMeta,
                        navIcons, c1Icon, colIcons, colBlog
                       )
    );
}

function buildCollection(frameGeo,
                         colReq, iconReq,
                         colMeta,
                         navIcons, c1Icon, colIcons, colBlog,
                        ) {
    // geometry for navigation grid
    const gap      = 4;
    const padding  = 10;
    const border   = 1;
    const border2  = 2 * border;
    const gridGeo  = V2(3, 2);

    // geometry for icons with/without border
    // in full and half size

    const iconGeo  = bestFitIconGeo(frameGeo); // the geometry of nav icons
    const reqGeo   = bestFitToGeo(iconGeo);  // the geometry of the requested icons (maybe larger than iconGeo)

    // icon geo with border
    const iconGeoB = addV2(iconGeo, border2);

    const i2h      = div(iconGeo.y - border2 - gap, 2);
    const i2w      = div(3 * i2h, 2);
    const ico2Geo  = V2(i2w, i2h);
    const ico2GeoB = addV2(ico2Geo,border2);

    const gapGeo   = mulV2(subV2(gridGeo, 1), gap);
    const navGeo   = addV2(mulV2(ico2GeoB, gridGeo), gapGeo);

    const numCols = Math.max(div(frameGeo.x - 2 * padding, iconGeoB.x + gap));

    // geometry in px
    const iG       = toPx(iconGeo);
    const iGB      = toPx(iconGeoB);
    const i2G      = toPx(ico2Geo);
    const i2GB     = toPx(ico2GeoB);

    const cssBorderColor = "#444";
    const cssBorder      = border + "px solid " + cssBorderColor;

    trc(1, "iconGeo="  + showGeo(iconGeo));
    trc(1, "ico2Geo="  + showGeo(ico2Geo));
    trc(1, "iconGeoB=" + showGeo(iconGeoB));
    trc(1, "ico2GeoB=" + showGeo(ico2GeoB));
    trc(1, "navGeo="   + showGeo(navGeo));
    trc(1, "reqGeo="   + showGeo(reqGeo));
    trc(1, "numCols="  + numCols);

    function buildIcon(req) {
        const r = newElem("img",
                          { width:  "100%",
                            height: "100%"
                          });
        r.src = imgReqToUrl(iconReq, reqGeo);
        return r;
    }

    function toIconUrl(req) {
        const ireq = { rType: "icon", rPathPos: req.rPathPos};
        return imgReqToUrl(ireq, reqGeo);
    }

    function addBild(req, txt) {
        return txt.endsWith(" ")
            ? ( txt +
                ( isColReq(req)
                  ? "Album"
                  : "Bild"
                )
              )
            : txt;
    }

    function buildColHeaderFooter(isHeader) {

        function buildHeadIcon() {
            if ( isHeader ) {
                const r = newElem("div",
                                  { width:  iG.x,
                                    height: iG.y,
                                    border: cssBorder
                                  },
                                  "collection-header-icon");

                const a = newElem("a");
                const href = `javascript:openEditPage('${colReq.rPathPos[0]}')`;
                a.href  = href;     // "javascript:openEdit()";
                a.appendChild(buildIcon(iconReq));

                r.appendChild(a);
                return r;
            } else {
                return newElem("div");
            }
        }

        function buildLine(cls, txt) {
            const l = newElem("div", "", {}, cls);
            l.appendChild(newText(txt));
            return l;
        }

        function buildHeadLine() {
            if ( isHeader ) {
                const r  = newElem("div", {}, "collection-header-title");
                const t1 = colMeta["Descr:Title"];
                const t2 = colMeta["Descr:Subtitle"];
                const t3 = colMeta["Descr:Comment"];

                if (t1) {
                    r.appendChild(buildLine("title", t1));
                }
                if (t2) {
                    r.appendChild(buildLine("subtitle", t2));
                }
                if (t3) {
                    r.appendChild(buildLine("comment", t3));
                }
                return r;
            } else {
                return newElem("div");
            }
        }

        function buildNav() {

            function buildNavIcon(ico, tt0) {
                if (! ico) {
                    return newElem("div");
                }

                const req = ico.eReq;
                const md  = ico.eMeta;

                const r = newElem("div",
                                  { width:  i2G.x,
                                    height: i2G.y,
                                    border: cssBorder
                                  },
                                  "navicon"
                                 );
                if (! req) {
                    return r;
                }

                const a = newElem("a");
                a.href  = toHref(jsonReqToUrl1(frameGeo, req));
                a.title = md["Descr:Title"]
                       || addBild(req, tt0)
                       || "";

                const i = newElem("img",
                                  { width:  "100%",
                                    height: "100%"
                                  });
                i.src = toIconUrl(req, reqGeo);
                a.appendChild(i);

                r.appendChild(a);
                return r;
            }

            const r = newElem("div",
                              { display:                 "grid",
                                "grid-template-columns": replicate(3, " " + i2GB.x),
                                "grid-template-rows":    replicate(2, " " + i2GB.y),
                                "grid-gap":              gap + "px"
                              });

            const i1 = buildNavIcon();
            const i2 = buildNavIcon(navIcons.par,  "umfassendes Album");
            const i3 = buildNavIcon();
            const i4 = buildNavIcon(navIcons.prev, "voriges ");
            const i5 = buildNavIcon(c1Icon,        "erstes ");
            const i6 = buildNavIcon(navIcons.next, "nächstes ");


            r.appendChild(i1);
            r.appendChild(i2);
            r.appendChild(i3);
            r.appendChild(i4);
            r.appendChild(i5);
            r.appendChild(i6);
            return r;
        }

        const h = newElem("div",
                          { display: "grid",
                            "grid-template-columns": iGB.x + " auto " + navGeo.x + "px",
                            "grid-column-gap": "1em"
                          },
                          isHeader ? "collection-header" : "collection-footer");
        h.appendChild(buildHeadIcon());
        h.appendChild(buildHeadLine());
        h.appendChild(buildNav());
        return h;
    }

    function buildColContents() {
        const r = newElem("div",
                          { display: "grid",
                            "grid-template-columns": replicate(numCols, " " + iGB.x),
                            "grid-auto-rows": iGB.y,
                            "grid-gap":       gap + "px",
                          },
                          "collection-contents");


        for (let i = 0; i < colIcons.length; i++) {
            const ce = colIcons[i];

            // trc(1, "buildColContents: i=" + i + ", val=" + JSON.stringify(ce));

            const ir = ce.eReq;
            const md = ce.eMeta;
            const e  = newElem("div",
                               { width:  iG.x,
                                 height: iG.y,
                                 border: cssBorder
                               });

            const a  = newElem("a");
            a.href   = toHref(jsonReqToUrl1(frameGeo, ir));
            a.title  = md["Descr:Title"]
                    || addBild(ir, (i + 1) + ". ");

            const im = newElem("img",
                               { width:  "100%",
                                 height: "100%"
                               });
            im.src = toIconUrl(ir, reqGeo);
            a.appendChild(im);
            e.appendChild(a);
            r.appendChild(e);
        }
        return r;
    }

    function buildColBlog(){
        const r = newElem("div", {}, "collection-blog");
        r.innerHTML = colBlog;
        return r;
    }

    function ruler() {
        const r = newElem("div",
                          { "padding-left":     padding + "px",
                            "padding-right":    padding + "px",
                            "margin-top":       padding + "px",
                            "margin-bottom":    padding + "px",
                            height:             border2 + "px",
                            "background-color": cssBorderColor
                          },
                          "ruler"
                         );
        return r;
    }

    const c = newElem("div",
                      { padding:      padding + "px",
                        "min-height": frameGeo.y + "px"
                      },
                      "collection"
                     );
    c.appendChild(buildColHeaderFooter(true));
    c.appendChild(ruler());
    if (colBlog) {
        c.appendChild(buildColBlog());
        c.appendChild(ruler());
    }
    c.appendChild(buildColContents());
    c.appendChild(ruler());
    c.appendChild(buildColHeaderFooter(false));
    c.appendChild(ruler());
    return c;
}

// ----------------------------------------
