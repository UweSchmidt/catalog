// ----------------------------------------
//
// global state

var openCollections = {};

/*
// ----------------------------------------
//
// server version is initialised in document ready event handler
//
// communication with scotty server and servant server differ
// in the basic data transfer,

var serverVersion = { "server"  : "unknown",
                      "version" : "0.0.0.0",
                      "date"    : "2000-00-00"
                    };

function getServerVersion (cont) {
    $.getJSON( "/server.json", function( data ) {
        serverVersion = data;
        console.log(JSON.stringify(serverVersion));
        cont();
    });
}

function initSystemCollections() {
    getServerVersion(openSystemCollections());
}
*/

// ----------------------------------------
//
// version test on bootstrap 4.5.0

function is450() {
    return (typeof bootstrapVersion == "number" && bootstrapVersion == 450);
}

var hiddenclass  = "hidden";
var imgboxclass  = "img-box";
var carmarksel   = 'div.carousel-caption a.carousel-image-mark';
var carmarksel1  = carmarksel;

if ( is450() ) {
    hiddenclass = "hidden-bs450";
    imgboxclass = "img-box-bs450";
    carmarksel1 = carmarksel1 + ' > svg';
}

// in bootstrap 4.5.0 the dia-un/marked class must be at the "svg"-tag,
// not the surrounding "a"-tag

function diaStarSelector(j) {
    res = '[data-star="' + j + '"]';
    if ( is450() ) {
        res = res + ' > svg';
    }
    return res;
}

// ----------------------------------------
//
// undo history
// history is stored in server
// when opening undo history menue the whole history
// is read from server
// and the drop down menue is created on the fly

function setHistory() {
    // clear undo history
    $('#undoHistoryList')
        .empty();

    // load new history list from server
    getHistoryFromServer(addHistList);
}

// callback from getHistoryFromServer
function addHistList(hl) {
    while ( hl.length != 0 ) {
        var hd = hl.pop();
        addHistToMenue(hd);
    }
}

function addHistCmd(cname, modifyCmd) {
    console.log("addHistCmd " + cname);
    getHistIdFromServer(cname, function (hid) {
        console.log("addHistCmd: " + hid + ": " + cname);

        // continuation style is necessary to
        // synchronize server calls
        // for creating a change history entry
        // and the real catalog modifying operation
        modifyCmd();
    });
}

function resetHistTo(hid) {
    console.log("resetHistTo " + hid);

    resetHistInServer(hid, function () {
        console.log("reset history in server until id: " + hid);
        statusMsg("discard all changes up to " + hid);
        checkAllColAreThere(true, true);
    });
}

function addHistToMenue(hd) {
    console.log("addHistToMenue " + hd);
    var hid = hd[0];
    var hnm = hd[1];

    var undoId = "undo-" + hid;
    var he = '<a class="dropdown-item" id="' + undoId + '">'
        + hid + ': ' + hnm
        + '</a>';
    console.log(he);

    // add item to history menue
    $('#undoHistoryList')
        .prepend(he);

    // install handler for new history menue entry
    console.log('install handler at: #' + undoId);
    $('#' + undoId)
        .on('click',
            function () {
                resetHistTo(hid);
            });
}

/* event handler not in use
   substituted by remUndoUntilLastSave
   to shorten undo list until last
   "Save Archive" command
*/
function clearUndoHistory() {
    getHistoryFromServer(resetHistList);
}

function resetHistList(hl) {
    if ( hl.length > 0 ) {
        var h0 = hl[0];
        var hid = h0[0];
        dropHistAtInServer(hid);
        statusMsg("Undo history clearded");
    } else {
        statusMsg("Undo history was already empty");
    }
}

function remUndoUntilLastSave() {
    getHistoryFromServer(shortenHistList);
}

function shortenHistList(hl) {
    var hid = searchSaveCmd(hl);
    if (hid > 0) {
        dropHistAtInServer(hid);
        statusMsg("Undo history truncated until last catalog save");
    } else {
        statusMsg("No catalog save command found in undo history");
    }
}

function searchSaveCmd(hl) {
    for (i = 0; i < hl.length; ++i) {
        hentry = hl[i];
        hid    = hentry[0];
        hcmd   = hentry[1];
        pos = hcmd.indexOf("save catalog");
        if (pos == 0) {
            return hid;
        }
    }
    return 0;
}

// ----------------------------------------


// remove a collection
function remCol(path) {
    console.log('remCol: ' + path);
    openCollections[path] = undefined;
}

// insert a  collection
function insCol(path, col) {
    console.log('insCol: ' + path);
    console.log(col);
    openCollections[path] = col;
}

// update a collection and
// return whether col has changed
function updCol(path, col) {
    console.log('updCol: ' + path);
    var unchanged = eqCol(openCollections[path], col);
    if ( unchanged ) {
        console.log('nothing has changed');
    } else {
        console.log('something has changed');
        console.log(openCollections[path]);
        console.log(col);
        openCollections[path] = col;
    }
    return ! unchanged;
}

// compare collections
function eqCol(col, col1) {
    if (! col || ! col1) {
        return false;
    }
    var es  =  col.entries;
    var es1 = col1.entries;
    var ln  = es.length;
    if ( ln != es1.length ) {
        return false;
    }
    var i = 0;
    for (i = 0; i < ln; ++i) {
        var e  =  es[i];
        var e1 = es1[i];
        if (e.ColEntry != e1.ColEntry) {
            return false;
        }
        if (e.ref != e1.ref) {
            return false;
        }
        if (e.ColEntry === 'IMG'
            && e.part != e1.part) {
            return false;
        }
    }
    return true;
}

// ----------------------------------------
//
// dia button group handler

function diaButton(e) {
    e.preventDefault();
    statusClear();

    var res    = {};
    res.dia    = $(e.target).closest('div.dia');
    res.pos    = getEntryPos(res.dia);
    res.cid    = activeCollectionId();
    res.path   = collectionPath(res.cid);
    res.rating = getRatingVal(res.dia);
    // res.name = getDiaName(res.dia);
    console.log(res);
    return res;
}

function setDiaNo(dia, i) {
    $(dia).find('span.img-no').empty().append("" + (i + 1));
}

function setDiaImgName(dia, name) {
    $(dia)
        .find('span.img-part')
        .empty()
        .append(name);
}

function setDiaColName(dia, name) {
    $(dia)
        .find('span.col-part a')
        .empty()
        .append(name);
}

function setDiaColRef(dia, path) {
    $(dia)
        .find('span.col-part a')
        .attr('href', path);
}

function setDiaColAccess(dia, wr) {
    console.log('setDiacolaccess', wr);
    if (wr) {
        $(dia)
            .find('span.img-col-writeprotected')
            .addClass(hiddenclass);
    } else {
        $(dia)
            .find('span.img-col-writeprotected')
            .removeClass(hiddenclass);
    }
}

function getDiaColWriteProtected(dia) {
    var res = $(dia)
            .find('span.img-col-writeprotected')
            .hasClass(hiddenclass);
    console.log('getdiaColWriteAccess', res);
    return res;
}

// --------------------

function setDiaRating(dia, rating) {
    console.log("setDiaRating: " + rating);
    console.log(dia);
    var stars = $(dia).find('div.dia-stars');
    stars.attr('data-rating', rating);
    var j = 0;
    for (j = 0; j <= 5; ++j) {
        var cls = j <= rating ? "dia-marked" : "dia-unmarked";
        var ssl = diaStarSelector(j);
        stars.find(ssl)
            .removeClass("dia-marked")
            .removeClass("dia-unmarked")
            .addClass(cls);
    }
}

function setRatingInCollection(cid, i, rating) {
    console.log("setRating: " + cid + ", " + i + ", " + rating);
    var dia = getDia(cid, i);
    setDiaRating(dia, rating);
}

// used as callback for getRatingsFromServer
function setAllRatingsInCollection(cid, ratings) {
    var i = 0;
    for (i = 0; i < ratings.length; ++i) {
        setRatingInCollection(cid, i, ratings[i]);
    }
}

function setRatingsInCollection(cid, ixs, rating) {
    var i = 0;
    for (i = 0; i < ixs.length; ++i) {
        if ( ixs[i] >= 0 ) {
            setRatingInCollection(cid, i, rating);
        }
    }
    unmarkAll(cid);
}

// --------------------

function getDia(cid, i) {
    var sel = '#' + cid + " > div.dia:nth-child(" + (i + 1) + ")";
    console.log("getDia: sel: " + sel);
    return $(sel);
}

function isMarkedDia(dia) {
    if (dia) {
        return dia.hasClass('marked');
    }
    return null;
}

function isUnmarkedDia(dia) {
    if (dia) {
        return ! dia.hasClass('marked');
    }
    return null;
}

function toggleDiaMark(cid, i) {
    var dia = getDia(cid, i);
    if (dia) {
        dia = toggleOrSetMark(dia, 'toggle');
    }
    return dia;
}

// --------------------

function getDiaNo(dia) {
    var pos = $(dia)
            .find('span.img-no')
            .contents()
            .get(0)
            .textContent;
    pos = parseInt(pos) - 1;
    return pos;
}

function getDiaColRef(dia) {
    var res = $(dia)
            .find('span.col-part a')
            .attr('href');
    console.log('getDiaColRef: ', res);
    return res;
}

function getDiaName(dia) {
    var res;

    // img
    res = $(dia)
        .find('span.img-part')
        .contents()
        .get(0);
    if (res && res.length > 0) {
        return res.textContent;
    }

    // col
    res = $(dia)
        .find('div.dia-name a')
        .contents()
        .get(0);
    if (res && res.length > 0) {
        return res.textContent;
    }

    return '???';
}

function diaBtnRemove(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#RemoveButton').click();
}

function diaBtnMoveToClipboard(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#MoveToClipboardButton').click();
}

function diaBtnCopyToClipboard(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#CopyToClipboardButton').click();
}

function diaBtnRename(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#RenameCollectionButton0').click();
}

function diaBtnWriteProtected(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#WriteProtectedButton').click();
}

function diaBtnSort(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#SortButton').click();
}

function diaBtnView(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#PreviewButton').click();
}

function diaBtnMeta(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#ShowMetaDataButton').click();
}

function diaBtnTitle(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#MetaDataButton').click();
}

function diaBtnColimg(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#CollectionImgButton').click();
}

function diaBtnBlog(e) {
    var o  = diaButton(e);
    setEntryMark(o.dia);
    $('#BlogEditButton0').click();
}

// ----------------------------------------
//
// mark/unmark entries
// toggleSlideMark is the event handler

function toggleSlideMark(e) {
    e.preventDefault();
    statusClear();
    // console.log("mark image was clicked");
    // console.log(e);
    // console.log(e.shiftKey);
    // console.log(e.target);

    var dia      = $(e.target).closest("div.dia");
    var unmarked = dia.hasClass('unmarked');
    if (e. shiftKey) {
        toggleLeft(dia, unmarked);
    } else {
        toggleMark(dia);
    }
}

function toggleLeft(dia, unmarked) {
    var dno = getDiaNo(dia);
    var uml  = dia.hasClass('unmarked');
    // console.log('toggleLeft', dno, uml);

    if (unmarked === uml) {
        if (dno > 0) {
            var prev = dia.prev('div.dia');
            // console.log(prev);
            toggleLeft(prev, unmarked);
        }
        toggleMark(dia);
    }
}

function toggleMark(dia) {
    toggleOrSetMark(dia, 'toggle');
}

function setEntryMark(dia) {
    // the clear guaranties, this entry will be
    // the last marked entry
    // this becomes important when sorting is added to button groups
    toggleOrSetMark(dia, 'clear');
    toggleOrSetMark(dia, 'set');
}

function clearEntryMark(dia) {
    toggleOrSetMark(dia, 'clear');
}

function toggleOrSetMark(dia, fct) {
    console.log("toggleDiaMark: dia");
    console.log(dia);

    if ( dia.hasClass("unmarked") ) {
        if (fct === 'toggle' || fct === 'set') {
            // dia was unmarked, mark it, and existing curmark
            dia.removeClass("unmarked");
            clearCurMark(dia);

            var mx = getMarkedNo(getMarked()).length + 1;
            // console.log(mx);
            setMarkCount(dia, '' + mx);

            // set mark and curmark
            markThisAsCur(dia);
            dia.addClass("marked");
        }
    } else {
        if (fct === 'toggle' || fct === 'clear') {
            // dia was marked, so unmark it
            // save the mark no
            var markNo = dia.find("div.dia-mark")
                    .contents()
                    .get(0).textContent;
            // console.log("unmark dia " + markNo);

            // was marked, remove mark, and move curmark to last marked
            dia.removeClass("marked").addClass("unmarked");
            clearCurMark(dia);
            markLastAsCur(dia);

            // renumber mark counts
            removeMarkCount(dia);
            renumberMarkCount(parseInt(markNo));
        }
    }
    return dia;
}

function clearCurMark(dia) {
    var tabContent = $(dia).parent();
    // console.log('clearCurMark');
    // console.log(tabContent);
    var img = tabContent.find("img.curmarked");
    // console.log(img);
    $(img).removeClass("curmarked");
    return dia;
}

function removeMarkCount(dia) {
    dia.find("div.dia-mark").empty();
    return dia;
}

function renumberMarkCount(pos) {
    // console.log('renumberMarkCount ' + pos);
    var col = getMarked();
    var nos = getMarkedNo(col);
    col.each(function (i, e) {
        // console.log("renumber");
        // console.log(i);
        // console.log(e);
        var ecol = $(e);
        var mno = ecol.find("div.dia-mark")
                .contents()
                .get(0).textContent;
        var no  = parseInt(mno);
        // console.log(no);
        if (no > pos) {
            setMarkCount(ecol, '' + (no - 1));
        }
    });
}

function setMarkCount(dia, m) {
    // console.log('setMarkCount');
    // console.log(dia);
    // console.log(m);
    dia.find("div.dia-mark").empty().append(m);
}

function markThisAsCur(dia) {
    dia.find("img.dia-src").addClass("curmarked");
}

function markLastAsCur() {
    var col = getMarked();
    // console.log(col);
    var nos = getMarkedNo(col);
    // console.log(nos);
    var i = maxVal(nos);
    // console.log("marklastascur: " + i);
    if ( i >= 0 ) {
        var sel = "div.dia-mark:contains(" + i + ")";
        // console.log(sel);
        col.find(sel)
            .closest("div.dia")
            .find("img.dia-src")
            .addClass("curmarked");
    }
}

function setMark(dia, mark) {
    var mark2 = 'colmark';
    if ( mark === mark2 ) {
        mark2 = 'imgmark';
    }
    dia.removeClass(mark2)
        .addClass(mark);
}

function getMarked() {
    return activeCollection().children("div.marked");
}

function getMarkedNo(col) {
    return col
        .find("div.dia-mark")
        .contents()
        .toArray()
        .map(function (x) {return parseInt(x.textContent);});
}

// end marking functions
//
// ----------------------------------------

function maxVal(a) {
    if ( a.length == 0)
        return -1;
    return Math.max(...a);
}

// ----------------------------------------
//
// collection ids and paths

function activeCollection() {
    return $("#theCollections").children("div.active");
}

function activeCollectionId() {
    return activeCollection().attr('id');
}

function alwaysOpenCollectionId(cid) {
    return cid === idCollections()
        || cid === idClipboard();
}

function allCollectionIds() {
    // collect all collection ids
    var colIds = [];
    $("#theCollections div.tab-pane").each(function (i, e) {
        var id1 = $(e).attr('id');
        colIds.push(id1);
    });
    console.log(colIds);
    return colIds;
}

function collectionPath(cid) {
    return $('#' + cid).attr('data-path');
}

function collectionIsWriteProtected(cid) {
    var res = $('#' + cid).hasClass('no-write');
    console.log('collectionIsWriteProtected');
    console.log(res);
    return res;
}

function collectionIsSortProtected(cid) {
    var res = $('#' + cid).hasClass('no-sort');
    console.log('collectionIsSortProtected');
    console.log(res);
    return res;
}

function collectionIsFix(path) {
    console.log('collectionIsFix');
    console.log(path);
    return path === pathClipboard()
        || path === pathCollections();
}

function collectionIsGenerated(path) {
    console.log('collectionIsGenerated');
    console.log(path);
    return collectionIsFix(path)
        || isPathPrefix(pathPhotos(), path)
        || isPathPrefix(pathTimeline(), path)
        || isPathPrefix(pathImports(), path);
}

function collectionId(path) {
    var res = undefined;
    $("#theCollections div.tab-pane").each(function (i, e) {
        var id1 = $(e).attr('id');
        var dpath = $(e).attr('data-path');
        if ( path === dpath ) {
            res = id1;
        }
    });
    return res;
}

function allCollectionPaths() {
    // collect all collection paths (ObjId's on server)
    var colPaths = [];
    $("#theCollections div.tab-pane")
        .each(function (i, e) {
            var path1 = $(e).attr('data-path');
            colPaths.push(path1);
        });
    console.log(colPaths);
    return colPaths;
}

function isWriteProtectedCollection(md) {
    var acc = md["Descr:Access"];
    return acc && acc.search('no-write') >= 0;
}

function isNoDeleteCollection(md) {
    var acc = md["Descr:Access"];
    return acc && acc.search('no-delete') >= 0;
}

function isNotSortableCollection(md) {
    var acc = md["Descr:Access"];
    return acc && acc.search('no-sort') >= 0;
}

// ----------------------------------------

// set write protection flag for collection entries
// set ratings for collection entries

var contChain = () => {};

function showNewCollectionCont(cont) {
    return (path, colVal) => {
        console.log("showNewCollectionCont " + path);

        // init contChain
        contChain = () => {
            console.log("contChain finished");

            // reset contChain to noop
            contChain = () => {};

            console.log("contChain call global continuation");
            cont();
        };

    // show collection and build contChain
    // this is a normal function call
    showNewCollection(path, colVal);

    // call cont chain for setting write protection flags
    // this is an async function call
    contChain();
}
                                     }
function showNewCollection(path, colVal) {
    console.log("showNewCollection " + path);
    // insert into global state
    insCol(path, colVal);

    // compute colId and colName from path
    var o  = splitPath(path);
    var md = colVal.metadata;
    console.log(o);
    console.log(colVal);
    console.log(md);

    var io = isAlreadyOpen(path);

    if ( io[0] ) {
        // nothing to do, collection is already there
        // but we need the id attr value
        o.colId = io[1];
        console.log("collection already open");
        console.log(o.colId);

        // switch to the collection
        // it's already loaded
        setActiveTab(o.colId);
    } else {
        // create the tab

        o.colId   = path2id(o.path);
        console.log(o);

        // no-write collection ?
        var ro = isWriteProtectedCollection(md);
        var sr = isNotSortableCollection(md);
        var nd = isNoDeleteCollection(md);
        var gn = collectionIsGenerated(o.path);
        var fx = collectionIsFix(o.path);
        var ct = md["Descr:Title"];

        // add the tab panel
        var t = $('#prototype-tabpanel').children("div").clone();

        t.find('div.tab-panel').empty();
        t.attr('id', o.colId).attr('data-path', o.path);

        if ( ro ) { t.addClass("no-write");  }
        if ( sr ) { t.addClass("no-sort");   }
        if ( nd ) { t.addClass("no-delete"); }
        if ( gn ) { t.addClass("generated"); }
        $('#theCollections').append(t);

        // create a new tab from prototype
        var tb = $('#prototype-tab').find("li").clone();

        var tt = "path: " + o.path;
        if (ct) { tt = "title: " + ct + "\n" + tt; }
        if (ro) { tt = tt + "\naccess: no-write"; };

        tb.find('a')
            .attr('href', '#' + o.colId)
            .attr('aria-controls', o.colId)
            .attr('title', tt);
        tb.find('.coltab-name')
            .empty()
            .append(o.name);

        if ( fx ) {  // hide close button
            tb.find('.coltab-delete')
                .addClass("hidden-bs450");
        } else {
            tb.find('.coltab-delete')
                .find('button')
                .on('click', function (e) {
                    closeCollection(o.colId);
                });
        }

        $('#collectionTab').append(tb);
        markAccess(o.colId, ro);

        // make the collection visible
        setActiveTab(o.colId);

        // fill the collection
        insertEntries(o.colId, colVal.entries);
    }
}

// search collection for path
// return true/false and the id attr value

function isAlreadyOpen(path) {
    var b   = false;
    var pid = undefined;

    $('#theCollections > div[role=tabpanel]')
        .each(function (i, e) {
            var dp = $(e).attr('data-path');
            if ( dp === path ) {
                b = true;
                pid = $(e).attr('id');
            };
        });
    return [b, pid];
}

function setActiveTab(colId) {
    $('#collectionTab')
        .find('a[aria-controls=' + colId + ']')
        .trigger('click');
    document.querySelector("#edit-collections-container").scrollTop = 0;
}

function insertEntries(colId, entries) {
    console.log('insertEntries');
    console.log(colId);
    console.log(entries);
    var path = collectionPath(colId);

    var col = $('#' + colId);
    col.empty();
    entries.forEach(function (e, i) {
        insertEntry(colId, path, e, i);
    });

    var contChain1 = contChain;

    contChain = () => {
        console.log("setAllRatings for " + path);

        // insert the ratings
        // this async call must be  syncronized
        getAllRatingsFromServer(colId, contChain1);
    };

    // set handler for showing edit buttons
    col.find('div.dia')
        .hover(function () {
            $(this)
                .find('.dia-btn-group')
                .removeClass(hiddenclass);
        }, function () {
            $(this)
                .find('.dia-btn-group')
                .addClass(hiddenclass);
        });

    // install handler at rating stars
    col.find('div.dia div.dia-stars a.star')
        .on('click', function (e) {
            console.log('rating button');
            console.log(e.target); // target is glyphicon-star span elem
            var o = diaButton(e);
            var s = $(e.target)
                    .closest('a.star')
                    .attr('data-star');
            var newRating = parseInt(s) || 0;
            var curRating = o.rating;
            if ( curRating === 1 && newRating === 1) {
                // 1 star set and 1 star pressed
                // -> remove star
                newRating = 0;
            }
            setEntryMark(o.dia);
            setRating(newRating);
            getAllRatingsFromServer(colId, () => {});
        });

    // set handler for button groups
    col.find('div.dia button.dia-btn-remove')
        .on('click', diaBtnRemove);
    /*
    col.find('div.dia button.dia-btn-movefromclipboard')
        .on('click', diaBtnMoveFromClipboard);
    col.find('div.dia button.dia-btn-copyfromclipboard')
        .on('click', diaBtnCopyFromClipboard);
     */
    col.find('div.dia button.dia-btn-movetoclipboard')
        .on('click', diaBtnMoveToClipboard);
    col.find('div.dia button.dia-btn-copytoclipboard')
        .on('click', diaBtnCopyToClipboard);
    col.find('div.dia button.dia-btn-rename')
        .on('click', diaBtnRename);
    col.find('div.dia button.dia-btn-writeprotected')
        .on('click', diaBtnWriteProtected);
    col.find('div.dia button.dia-btn-sort')
        .on('click', diaBtnSort);
    col.find('div.dia button.dia-btn-view')
        .on('click', diaBtnView);
    col.find('div.dia button.dia-btn-meta')
        .on('click', diaBtnMeta);
    col.find('div.dia button.dia-btn-title')
        .on('click', diaBtnTitle);
    col.find('div.dia button.dia-btn-colimg')
        .on('click', diaBtnColimg);
    col.find('div.dia.data-md button.dia-btn-colblog')
        .on('click', diaBtnColimg);
    // all col dias and .md img dias get this handler
    col.find('div.dia.data-md button.dia-btn-blog')
        .on('click', diaBtnBlog);

    //collections don't have all buttons
    col.find('div.dia.colmark button.dia-btn-colimg')
        .addClass(hiddenclass);
    col.find('div.dia.colmark button.dia-btn-blog')
        .removeClass(hiddenclass)
        .attr('title', 'Edit blog text for this collection');
    col.find('div.dia.colmark button.dia-btn-view')
        .attr('title', 'Preview image and/or blog text for this collection');


    // hide write protect button for generated collections
    col.find('div.dia.colmark')
        .each(function (i, e) {
            var cname = getDiaName(e);
            var cpath = path + "/" + cname;
            console.log('hide lock button ', cpath);
            if (collectionIsGenerated(cpath)) {
                $(e).find('button.dia-btn-writeprotected')
                .addClass(hiddenclass);
            }
        });

    // images don't have all buttons
    col.find('div.dia.imgmark button.dia-btn-rename')
        .addClass(hiddenclass);
    col.find('div.dia.imgmark button.dia-btn-writeprotected')
        .addClass(hiddenclass);

    // .md text files don't have meta data
    col.find('div.dia.imgmark.data-md button.dia-btn-meta')
        .addClass(hiddenclass);
    col.find('div.dia.imgmark.data-md button.dia-btn-title')
        .addClass(hiddenclass);
    // redefine colimg button to set colblog entry
    col.find('div.dia.imgmark.data-md button.dia-btn-colimg')
        .attr('title', "Take this text as blog text for the current collection");
    col.find('div.dia.imgmark.data-md button.dia-btn-view')
        .attr('title', "Preview this blog text");
    col.find('div.dia.imgmark.data-md button.dia-btn-blog')
        .removeClass(hiddenclass);

    // clipboard has a special set of image buttons
    if (colId === idClipboard()) {
        col.find('div.dia button.dia-btn-movefromclipboard')
            .addClass(hiddenclass);
        col.find('div.dia button.dia-btn-copyfromclipboard')
            .addClass(hiddenclass);
        col.find('div.dia button.dia-btn-movetoclipboard')
            .addClass(hiddenclass);
        col.find('div.dia button.dia-btn-copytoclipboard')
            .addClass(hiddenclass);
    }

    // in clipboard and subcollections remove is possible
    // not in the rest of the collections
    // clipboard collections are never write protected
    if ( ! isPathPrefix(pathClipboard(), path) ) {
        col.find('div.dia button.dia-btn-remove')
            .addClass(hiddenclass);
    } else {
        col.find('div.dia button.dia-btn-writeprotected')
            .addClass(hiddenclass);
    }
}

function insertEntry(colId, colPath, entry, i) {
    var e = newEntry(colId, colPath, entry, i);
    $('#' + colId).append(e);
}

function newEntry(colId, colPath, entry, i) {
    console.log("newEntry: colId=" + colId);
    console.log(entry);
    var p = $("#prototype-dia").children("div").clone();

    setDiaNo(p, i);

    // old url scheme
    var sc = iconSize('');
    var mk = '';
    var tt = '';
    var ref = splitPath(entry.ref);

    if (entry.ColEntry === "IMG") {
        setDiaImgName(p, entry.part);
        // add the part extension as class to the entry
        // "data-jpg" for images,
        // "data-md" for blog entries (markdown text .md or .txt)
        // in preview modal box this info becomes important

        var ep = splitPath(entry.part);
        var ex = ep.ext;
        if (ep.ext == "txt" || ep.ext == "md") {
            ex = "data-md";
        } else {
            ex = "data-jpg";
        }
        p.addClass(ex);

        // if img is a video, add extra "data-mp4" class as marker
        // only supported in edit-4.5.0 and later

        if (ep.ext == "mp4" && is450()) {
            p.addClass("data-mp4");
        }

        sc = sc + ref.cpath + "/" + entry.part;
        mk = "imgmark";
        tt = "image: " + entry.ref;
    }
    if (entry.ColEntry === "COL") {
        setDiaColName(p, ref.name);
        // collections always have a .jpg as preview
        p.addClass('data-jpg')
         .addClass('data-md');


        // this ref is a dummy
        // the real ref of a collection is inserted later
        // by a server call
        // but we need a legal src ref

        sc = sc + "/assets/icons/generated/brokenImage.jpg";
        mk = "colmark";
        tt = "collection: " + entry.ref;
    }

    // check whether src ref has extension .jpg
    var scref = splitPath(sc);
    if (scref.ext !== "jpg" && scref.ext !== "JPG") {
        sc = sc + ".jpg";
    }
    // set img/col mark
    setMark(p, mk);

    if ( entry.ColEntry === "COL") {
        // if entry is a collection add open collection event handler
        p.find("div.dia-name a")
            .on('click', function (e) {
                e.preventDefault();
                openCollection(ref.path);
            });

        // getIsWriteable... is an async function
        // which must be syncronized

        // copy chain to local variable
        var contChain1 = contChain;

        // extend global contChain with server call
        contChain = () => {
            console.log("setDiaColAccess for " + ref.path);
            getIsWriteableFromServer(ref.path,
                                     function (ro) {
                                         setDiaColAccess(p, ro);
                                         contChain1();
                                     });

        };

    }

    // url for entry icon with new url scheme
    var newIconRef = iconRef(iconSize(), colPath, entry, i);

    console.log("new iconRef: " + newIconRef);
    p.find("img.dia-src")
        .attr('src', newIconRef);

    removeMarkCount(p);

    // set the icon title
    p.find("img.dia-src")
        .attr("title", tt);

    // add event handler for marking
    p.find("div.dia-img")
        .on('click', toggleSlideMark)
        .css('cursor','pointer');

    return p;
}

// ----------------------------------------

function checkAndRefreshCol(path, refresh, force) {
    console.log('colIsThere: ' + path);
    getIsColFromServer(path,
                       function (isThere) {
                           if (! isThere) {
                               var cid = collectionId(path);
                               removeCollectionFromDom(cid);
                               statusMsg('collection closed: ' + path);
                           } else {
                               if ( refresh ) {
                                   if ( force ) {
                                       remCol(path);
                                   }
                                   updateCollection(path);
                               }
                           }
                       });
}

function checkAllColAreThere(refresh, force) {
    var colPaths = allCollectionPaths();
    colPaths.forEach(function (path, i) {
        checkAndRefreshCol(path, refresh, force);
    });

}

// ----------------------------------------

function syncActiveCollection(sync) {
    var cid  = activeCollectionId();
    var path = collectionPath(cid);
    syncCollectionWithFilesystem(sync, path);
}

function syncCollectionWithFilesystem(sync, path) {
    console.log("syncCollectionWithFilesystem: " + path);

    if ( ! isPathPrefix(pathPhotos(), path) ) {
        statusError('collection to be synchronized must be a subcollection of '
                    + pathPhotos());
        return;
    }

    // start syncing on server side
    statusMsg('synchronizing collection with filesystem: ' + path);
    addHistCmd("sync collection " + splitName(path),
               function () {
                   modifyServer1(sync, path, [],
                                 function(log) {
                                     statusMsg('synchronizing collections on server side done');
                                     // TODO: show log file in modal box
                                     console.log(log);
                                     checkAllColAreThere(true, false);
                                 });
               });
}

function exifActiveCollection() {
    var cid  = activeCollectionId();
    var path = collectionPath(cid);
    exifCollectionWithFilesystem(path);
}

function exifCollectionWithFilesystem(path) {
    console.log("exifCollectionWithFilesystem: " + path);

    if ( ! isPathPrefix(pathPhotos(), path) ) {
        statusError('collection to be updated with exif must be a subcollection of '
                    + pathPhotos());
        return;
    }

    // start syncing on server side
    statusMsg('recomputing exif data for: ' + path);
    addHistCmd("sync exif data in " + splitName(path),
               function () {
                   modifyServer('syncExif', path, [true, true],
                                function(log) {
                                    statusMsg('recomputing exif data done');
                                    console.log(log);
                                });
               });
}

function checkArchiveConsistency() {
    console.log("checkArchiveConsistency");
    statusMsg('checking/repairing archive consistency');
    addHistCmd("check catalog",
               function () {
                   modifyServer1("checkArchive", pathArchive(), [],
                                 function(log) {
                                     statusMsg('checking archive on server side done');
                                     // TODO
                                     console.log(log);
                                 });
               });
}

// ----------------------------------------

// for remove, move and rename commands
// check whether subcollections are still there
// maybe sometimes there are subcollections,
// which were also moved or removed

function refreshCollection1(path, colVal) {
    refreshCollection(path, colVal);
    checkAllColAreThere(false, false);
}

function refreshCollection(path, colVal) {
    refreshCollection2(path,colVal, false);
}

function refreshCollectionF(path, colVal) {
    refreshCollection2(path,colVal, true);
}

function refreshCollection2(path, colVal, force) {
    var o = splitPath(path);
    console.log('refreshCollection: ' + force);
    console.log(o);
    console.log(colVal);

    // only if the collection content has changed
    // update the entries
    var changed = updCol(path, colVal);
    if ( changed || force) {
        var io = isAlreadyOpen(path);
        // check whether collection is already there
        if ( io[0]) {
            o.colId = io[1];
            insertEntries(o.colId, colVal.entries);
        }
    } else {
        unmarkAll(collectionId(path));
    }
}

// ----------------------------------------

// messages don't overwrite errors
function statusMsg(msg) {
    if (! $('#status-container')
         .hasClass('status-err-bg') ) {

        $('#status-message')
            .removeClass('error-msg')
            .addClass('status-msg')
            .empty()
            .append(msg);
    }
}

function statusError(msg) {
    $('#status-container')
        .addClass('status-err-bg');
    $('#status-message')
        .removeClass('status-msg')
        .addClass('error-msg')
        .empty()
        .append(msg);
}

// clear errors and messages
function statusClear() {
    console.log('statusClear');
    $('#status-container')
        .removeClass('status-err-bg');
    statusMsg('&nbsp;');
}

// ----------------------------------------
// string helper functions

function path2id(path) {
    // for html ids replace all none alphanum chars by "_"
    // else it isn't a valid id value

    var res = 'col-' + path.replace(/[^-_a-zA-Z0-9]/g,"_");
    console.log('path2id');
    console.log(path);
    console.log(res);

    return res;
}

// compute an object with path, name and cpath, and bname and ext
function splitPath(p) {
    if (typeof p === 'string') {
        return splitPath({'path' : p});
    }

    var a = p.path.split("/");
    p.name    = a.pop();
    // console.log(a);
    p.cpath   = a.join("/");
    // a.shift();
    // console.log(a);
    p.topname = a.shift(); // get the name of the top dir
    // console.log(a);
    a.unshift("");
    // console.log(a);
    p.cpath1  = a.join("/"); // remove the topdir

    var e = p.name.split(".");
    if (e.length < 2) {
        p.ext   = "";
        p.bname = p.name;
    } else {
        p.ext   = e.pop();
        p.bname = e.join(".");
    }
    return p;
}

function splitName(p) {
    return splitPath(p).name;
}

// check whether p1 is a path prefix of p2

function isPathPrefix(p1, p2) {
    var l1  = p1.length;
    var l2  = p2.length;
    var px2 = p2.substr(0, l1);
    if (p1 === p2) {
        return true;
    }
    if ( (l2 > l1) && (p1 === px2) && (p2.charAt(l1) === "/") ) {
        return true;
    }
    return false;
}

// for a collection path compute an array containing all
// ancestor collections with "/archive/colections" as 1. element
// and argument p as last element
//
// if path does not represent a collection the empty list is returned

function allAncestorCollections(p) {
    var p0 = pathCollections();
    var r  = [];
    if (isPathPrefix(p0, p)) {
        r.unshift(p);
        while (p != p0) {
            var o = splitPath(p);
            p = o.cpath;
            r.unshift(p);
        }
    }
    return r;
}

function addToList(xs, ys) {
    // console.log("addToList:");
    // console.log(xs);
    // console.log(ys);
    for (var i = 0; i < ys.length; ++ i) {
        var y = ys[i];
        if ( ! xs.includes(y) ) {
            xs.push(y);
        }
    }
    // console.log(xs);
    return xs;
}

function addToCols(xs, p) {
    return addToList(xs, allAncestorCollections(p));
}

var globalCollectionPath = "";
var globalPicNo = -1;

function getSearchPath() {
    var s = window.location.search;
    s = s.slice(1);
    var params = s.split("&");

    for (var i = 0; i < params.length; ++i) {
        var kv = params[i].split("=");
        var k  = kv[0];
        var v  = kv[1];
        if ( k == "path"  ) { globalCollectionPath = v; }
        if ( k == "picno" ) { globalPicNo = parseInt(v); }
    }
    console.log("getSearchPath: p=" + globalCollectionPath);
}

// ----------------------------------------

// top level commands, most with ajax calls

// initialize the collections in a fixed sequence,
// first the clipboard, so it's always the leftmost collection in the tab
// the root collection is not shown until the clipboard is there


function openAncestorCollections(p) {
    statusClear();
    openCols(allAncestorCollections(p));
}

function openCols(ps, ff) {
    console.log("openCols: ps=" + ps);
    if (ps.length != 0) {
        var p = ps.shift();  // split 1. element from path list
        getColFromServer(p,
                         (path, colVal) => {
                             showNewCollectionCont(
                                 () => { openCols(ps, ff)}
                             )(path, colVal);
                         }
                        );
    } else {
        if ( ff ) { ff(); }  // invoke final callback
    }
}

function markGlobalDia() {
    console.log("markGlobalDia: picno=" + globalPicNo);
    if ( globalPicNo >= 0 ) {
        markCurrDia(globalPicNo);
    }
}

function markCurrDia(i) {
    var cid = activeCollectionId();
    console.log("markCurrDia: cid=" + cid + ", picno=" + i);
    toggleDiaMark(cid, i);
}

function openSystemCollections() {
    var cs = [pathClipboard(),
              pathCollections()
             ];
    statusClear();
    getSearchPath();
    cs = addToCols(cs, globalCollectionPath);
    openCols(cs, markGlobalDia);
}

function openCollection(path) {
    statusClear();
    getColFromServer(path, showNewCollectionCont(() => {}));
}

function updateCollection(path) {
    getColFromServer(path, refreshCollection);
}

function closeCollection(cid) {
    statusClear();
    var cp   = collectionPath(cid);
    if ( alwaysOpenCollectionId(cid) ) {
        statusError("this collection must stay open: " + cp);
    } else {
        var cids = allCollectionIds();
        var ix   = cids.indexOf(cid);
        var next = '';

        if ( ix < 0) {
            return;
        }
        if ( ix === 0 ) {
            next = cids[1];
        } else {
            next = cids[ix - 1];
        }
        setActiveTab(next);
        removeCollectionFromDom(cid);
        statusMsg('collection closed: ' + cp);
    }
}

function removeCollectionFromDom(cid) {
    // remove from state
    remCol(collectionPath(cid));

    // remove the tab content
    $('#' + cid).remove();

    // remove the tab
    $('li > a[aria-controls=' + cid + ']').remove();
}

// ----------------------------------------
// mark functions

// process each dia in active collection
function eachADia(pred, fct) {
    eachDia(activeCollectionId(), pred, fct);
}

function eachDia(cid, pred, fct) {
    console.log('eachDia: ' + cid);
    $('#' + cid + ' > div.dia')
        .each(function (i, e) {
            if ( pred($(e)) ) {
                fct($(e));
            }
        });
}

function constTrue(dia) { return true; }

function hasRating(pred) {
    var f = function (dia) {
        var r = getRatingVal(dia);
        return pred(r);
    };
    return f;
}

function eqInt(i) {
    var p = function (j) {return j === i;}
    return p;
}

function geInt(i) {
    var p = function (j) {return j >= i;}
    return p;
}

function testDia(dia) {
    console.log(dia.find('img.dia-src').attr('title'));
}

function markAll(cid) {
    statusClear();
    console.log('mark all images in ' + cid);

    $('#' + cid + ' > div.unmarked')
        .each(function (i, e) {
            toggleMark($(e));
    });
}

function unmarkAll(cid) {
    // statusClear();
    console.log('unmark all images in ' + cid);

    var col = $('#' + cid);

    // remove all marked classes
    var ens = col.find('div.marked')
            .removeClass('marked')
            .addClass('unmarked');

    // remove all mark counts
    ens.find('div.dia-mark')
        .empty();

    // remove mark as current
    ens.find('img.curmarked')
        .removeClass('curmarked');
}

function getMarkedEntries(cid) {
    console.log('getMarkedEntries');
    var ixs = [];
    $('#' + cid + ' > div.dia > div.dia-top > div.dia-mark')
        .each(function(i, e) {
            // get the mark cnt
            var v = $(e).contents().get(0);
            var c = -1;
            if ( v ) {
                c = parseInt(v.textContent);
            }
            // console.log(v);
            // console.log(c);
            ixs.push(c);

        });
    console.log(ixs);
    return ixs;
}

function anyMarked(ixs) {
    var res = false;
    ixs.forEach(function (e, i) {
        if ( e >= 0 ) {
            res = true;
        }
    });
    return res;
}

function getLastMarkedEntry(cid) {
    var res = $('#' + cid)
            .find("img.curmarked")
            .closest('div.dia')
            .get(0);
    console.log('getLastmarkedEntry');
    console.log(res);
    return res;
}

function getColNames(cid) {
    var cnames = $('#' + cid + ' div.dia.colmark div.dia-name a')
            .contents()
            .get()
            .map(function (x) {return x.textContent;});
    console.log('getColNames');
    console.log(cnames);
    return cnames;
}

function getMarkedCollections(cid) {
    console.log('getMarkedCollections: ' + cid);
    var paths =[];
    $('#' + cid + ' div.dia.colmark.marked div.dia-img img')
        .each(function(i,e) {
            var t = $(e).attr('title');
            t = t.replace(/collection: /, '');
            console.log(t);
            paths.push(t);
        });
    console.log(paths);
    return paths;
}

function getOpenMarkedCollections(cid) {
    console.log("getOpenMarkedCollections: " + cid);
    var mcs = getMarkedCollections(cid);
    var ocs = allCollectionPaths();
    var res = [];
    mcs.forEach(function(e, i) {
        ocs.forEach(function(e2, i2) {
            if ( e === e2) {
                res.push(e2);
            }
        });
    });
    console.log('getOpenMarkedCollections');
    console.log(res);
    return res;
}

function getOpenMarkedSubCollections(cid) {
    console.log("getOpenMarkedSubCollections: " + cid);
    var mcs = getMarkedCollections(cid);
    var ocs = allCollectionPaths();
    var res = [];
    mcs.forEach(function(e, i) {
        ocs.forEach(function(e2, i2) {
            if ( isPathPrefix(e, e2) ) {
                res.push(e2);
            }
        });
    });
    console.log('getOpenMarkedSubCollections');
    console.log(res);
    return res;
}

function closeSubCollections(cid) {
    getOpenMarkedSubCollections(cid)
        .forEach(function(e, i) {
            var id = collectionId(e);
            var cp = collectionPath(id);
            removeCollectionFromDom(id);
            statusMsg('collection closed: ' + cp);
        });
    setActiveTab(cid);
}

// ----------------------------------------

function removeMarkedFromClipboard() {
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    statusClear();
    if ( ! isPathPrefix(pathClipboard(), cpath)) {
        statusError('removing images/collections only possible in clipboard collections');
        return;
    }
    removeMarkedFromCollection(cid);
}

function removeMarkedFromCollection(cid) {
    // if open collections are to be removed (or moved)
    // they must be closed first
    // get marked collections (not images)
    // get all paths of open collections
    // the intersection is the set of collections to be closed
    closeSubCollections(cid);

    var cpath = collectionPath(cid);
    if (! cpath ) {
        statusError('collection not found: ' + cid);
        return;
    }

    var ixs   = getMarkedEntries(cid);
    console.log('removeMarkedFromCollection');
    console.log(ixs);
    console.log(cpath);

    // remove on server and refresh collection
    removeFromColOnServer(cpath, ixs);
}

// ----------------------------------------

function moveAllFromClipboard(cid) {
    console.log('moveAllFromClipboard' + cid);
    markAll(idClipboard());
    moveMarkedFromClipboard(cid);
}

function copyAllFromClipboard(cid) {
    console.log('copyAllFromClipboard' + cid);
    markAll(idClipboard());
    copyMarkedFromClipboard(cid);
}

function moveAllToClipboard(cid) {
    console.log('moveAllToClipboard' + cid);
    markAll(cid);
    moveMarkedToClipboard(cid);
}

function copyAllToClipboard(cid) {
    console.log('copyAllToClipboard' + cid);
    markAll(cid);
    copyMarkedToClipboard(cid);
}

function moveMarkedFromClipboard(cid) {
    statusClear();
    var ixs = getMarkedEntries(idClipboard());
    var dpath = collectionPath(cid);
    var cpath = pathClipboard();
    console.log('moveMarkedFromClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    moveToColOnServer(cpath, dpath, ixs);
}

function copyMarkedFromClipboard(cid) {
    statusClear();
    var ixs = getMarkedEntries(idClipboard());
    var dpath = collectionPath(cid);
    var cpath = pathClipboard();
    console.log('copyMarkedFromClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    copyToColOnServer(cpath, dpath, ixs);
}

// ----------------------------------------

function moveMarkedToClipboard(cid) {
    statusClear();
    var ixs   = getMarkedEntries(cid);
    var cpath = collectionPath(cid);
    var dpath = pathClipboard();
    console.log('moveMarkedToClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    moveToColOnServer(cpath, dpath, ixs);
}

function copyMarkedToClipboard(cid) {
    statusClear();
    var ixs   = getMarkedEntries(cid);
    var cpath = collectionPath(cid);
    var dpath = pathClipboard();
    console.log('copyMarkedToClipboard');
    console.log(ixs);
    console.log(cpath);
    console.log(dpath);

    // do the work on server and refresh both collections
    copyToColOnServer(cpath, dpath, ixs);
}

// ----------------------------------------

function sortCollection(cid) {
    console.log('sortCollection: ' + cid);

    if (! checkSortable(cid)) {
        return;
    }

    var ixs  = getMarkedEntries(cid);
    if (! anyMarked(ixs)) {
        statusMsg('no marked images/collections found for sorting');
        return;
    }

    var path = collectionPath(cid);
    console.log(path);

    sortColOnServer(path, ixs);
}


function sortCollByDate(cid) {
    console.log('sortCollByDate: ' + cid);

    if (! checkSortable(cid)) {
        return;
    }

    var ixs  = getMarkedEntries(cid);
    if (! anyMarked(ixs)) {
        statusMsg('sort whole collection by date');
    }

    var path = collectionPath(cid);
    console.log(path);

    sortColByDateOnServer(path, ixs);
}

function checkSortable(cid) {
    var sr = collectionIsSortProtected(cid);
    console.log(sr);
    if ( sr ) {
        var path = collectionPath(cid);
        statusError('not sortable, the collection is sort protected: ' + path);
    }
    return ! sr;
}

// ----------------------------------------

function getEntryPos(img) {
    var pos = $(img)
            .find('span.img-no')
            .contents()
            .get(0)
            .textContent;
    pos = parseInt(pos) -1;
    console.log('getEntryPos');
    console.log(pos);
    return pos;
}

function getRatingVal(img) {
    var r = $(img)
            .find('div.dia-stars')
            .attr('data-rating');
    r = parseInt(r) || 0;
    console.log('getRatingVal: ' + r);
    return r;
}

function setCollectionImg(cid) {
    statusClear();
    console.log("setCollectionImg: " + cid);
    var path  = collectionPath(cid);
    var o     = splitPath(path);

    // first try current collection for a marked image
    // then lookup clipboard for a marked entry
    var scid  = cid;
    var spath = path;
    var simg  = getLastMarkedEntry(scid);
    if (! simg) {
        scid  = idClipboard();
        spath = pathClipboard();
        simg  = getLastMarkedEntry(scid);
    }
    if (! simg) {
        statusError('no marked image found');
        return;
    }
    if ( $(simg).hasClass('imgmark') ) {
        var pos = getDiaNo(simg);
        console.log('setCollectionImg:');
        console.log(pos);
        console.log(path);
        console.log(spath);
        console.log(o);

        if ( $(simg).hasClass('data-jpg') ) {
            // it's a .jpg image or a .mp4 movie
            // so take this image or movie icon as collection image
            addHistCmd("set col img for " + splitName(path),
                       function () {
                           modifyServer('colimg', path, [spath, pos],
                                        function () {
                                            var ppath = o.cpath;
                                            var pcol = isAlreadyOpen(ppath);
                                            statusMsg('collection image set in: ' + path);
                                            if ( pcol[0] ) {
                                                // parent collection open
                                                // refresh the parent collection
                                                // to show the new collection image
                                                remCol(ppath); // force refresh
                                                getColFromServer(ppath, refreshCollection);
                                            }
                                        });
                       });
        }
        else if ( $(simg).hasClass('data-md') ) {
            // it's a .md text file
            // so take this as the collection blog text
            addHistCmd("set col blog for " + splitName(path),
                       function () {
                           modifyServer('colblog', path, [spath, pos], noop);
                       });
        } else {
            statusError('not a .jpg image or a .md text file');
        }
    } else {
        statusError('marked entry is a collection, not an image');
    }
    // unmark last marked entry
    toggleMark($(simg));
}

// ----------------------------------------

function createCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var name  = $('#newCollectionName').val();
    name = name.replace(/[^-_.a-zA-Z0-9]/g,"");
    var cnames = getColNames(cid);
    var ix = cnames.indexOf(name);

    console.log('createCollection');
    console.log(cpath);
    console.log(name);
    console.log(cnames);

    if (name.length === 0) {
        statusError("no collection name given");
        return;
    }
    if ( ix >= 0 ) {
        statusError("collection name already exists: " + name);
        return;
    }
    createColOnServer(cpath, name, refreshCollection);
}

// ----------------------------------------

function writeProtectedCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var ixs   = getMarkedEntries(cid);
    var opcs  = getOpenMarkedCollections(cid);
    var dia   = getLastMarkedEntry(cid);
    var ro    = getDiaColWriteProtected(dia);
    console.log('writeProtectedCollection');
    console.log(cid);
    console.log(ixs);
    console.log(opcs);
    console.log(ro);

    changeWriteProtectedOnServer(cpath, ixs, ro, opcs);
}

function markWriteProtected(opcs, ro) {
    console.log('markWriteProtected', ro);
    console.log(opcs);

    opcs.forEach(function (e, i) {
        var cid = collectionId(e);
        markAccess(cid, ro);
    });
}

// the lock icon in tab list of open collections is set/removed
// the marker class no-write is added/removed in tab panel
function markAccess(cid, ro) {
    var c = $('#collectionTab')
            .find('[href="#' + cid + '"]')
            .find('.coltab-writeprotected');
    if (ro) {
        c.removeClass(hiddenclass);
        $('#' + cid).addClass('no-write');
    } else {
        c.addClass(hiddenclass);
        $('#' + cid).removeClass('no-write');
    }
}

// ----------------------------------------

// renameCollectionCheck does all the error checks
// if this is o.k. the #RenameCollectionButton is invoked
// and this one triggers the modal mox
// so error check and reporting can be done before the modal is shown

function renameCollectionCheck() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);

    if (collectionIsWriteProtected(cid)) {
        statusError('rename not allowed, collection is write protected: ' + cpath);
        return;
    }
    var img   = getLastMarkedEntry(cid);
    if (! img) {
        statusError("no collection marked in: " + cpath);
        return;
    }
    if ($(img).hasClass('imgmark')) {
        statusError("last marked isn't a collection in: " + cpath);
        // clearEntryMark($(img));
        return;
    }
    var iname = getDiaName(img);
    $('#RenameCollectionModalLabel')
        .empty()
        .append('Rename collection: ' + iname);
    $('#RenameCollectionButton').click();
}

function renameCollection() {
    statusClear();
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var img   = getLastMarkedEntry(cid);
    var iname = getDiaName(img);
    var path  = cpath + "/" + iname;
    console.log('renameCollection');
    console.log(path);

    var newname  = $('#RenameCollectionName').val();
    newname = newname.replace(/[^-_.a-zA-Z0-9]/g,"");

    console.log(newname);

    if (newname.length === 0) {
        statusError("no collection name given");
        return;
    }

    var cnames = getColNames(cid);
    var ix = cnames.indexOf(newname);

    console.log(cnames);
    console.log(ix);

    if ( ix >= 0 ) {
        statusError("collection name already exists: " + newname);
        return;
    }

    renameColOnServer(cpath, path, newname, refreshCollection1);
}

// ----------------------------------------
//
// global state for metadata clipboard

var metaDataClipboard= {};


// keys of editable metadata values

var metaDataKeys = [ "Title",
                     "Subtitle",
                     "Comment",
                     "CommentImg",
                     "GPSPosition",
                     "Location",
                     "Rating",
                     "Keywords",
                     "Web",
                     "Wikipedia",
                     "Audio",
                     "TitleEnglish",
                     "TitleLatin"
                   ];

function mdKey(k) { return 'Descr:'  + k; }
function mdKid(k) { return '#descr-' + k; }

function copyMetaData() {
    console.log('copyMetaData');

    metaDataClipboard = readMetaDataBox();

    statusMsg('Metadata copied to clipboard');
}

function pasteMetaData() {
    console.log('pasteMetaData');

    // read dialog box
    var md = readMetaDataBox();

    // merge clipboard values
    metaDataKeys.forEach(function(e) {
        var k = mdKey(e);
        var v = metaDataClipboard[k] || "";
        if (v && v.length > 0) {
            md[k] = v;
        }
    });

    // write new values back into dialog box
    writeMetaDataBox(md);

    statusMsg('Metadata from clipboard inserted');
}

// read attr values from dialog box for metadata

function readMetaDataBox() {
    var md = {};

    metaDataKeys.forEach(function (e) {
        var k = mdKey(e);
        var v = $(mdKid(e)).val();
        if (v && v.length > 0) {
            md[k] = v;
        }
    });

    console.log('readMetaDataBox');
    console.log(md);

    return md;
}

// write attr values into dialog box

function writeMetaDataBox(md) {
    console.log('writeMetaDataBox');
    console.log(md);

    metaDataKeys.forEach(function(e) {
        var v = md[mdKey(e)] || "";
        $(mdKid(e)).val(v);
    });
}


function clearMetaDataBox() {
    console.log('clearMetaDataBox');

    writeMetaDataBox({});
}

function setMetaData() {
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);

    /*
     // write protection is limited to collection entries,
     // collection image and meta data may be modified

    if (collectionIsWriteProtected(cid)) {
        statusError('can\'t set meta data, active collection is write protected: '
                    + cpath
                   );
        return;
    }
     */

    var metadata = normalizeAudioUrl(readMetaDataBox());

    /*
    if (jQuery.isEmptyObject(metadata)) {
        statusMsg('no meta data given');
        return;
    }
    */

    var ixs = getMarkedEntries(cid);

    console.log('setMetaData');
    console.log(metadata);
    console.log(ixs);

    setMetaOnServer(cpath,ixs,metadata);
}

function setRating(rating) {
    var cid   = activeCollectionId();
    var cpath = collectionPath(cid);
    var ixs   = getMarkedEntries(cid);

    if (! anyMarked(ixs)) {
        statusMsg('no marked images/collections found');
        return;
    }
    console.log('setRating: ' + rating);
    console.log(ixs);

    setRatingsOnServer(cid, cpath, ixs, rating);
}

function extternalUrl(s) {
    const re = /^https?:\/\//;
    return re.test(s);

}

function localAudioUrl(s) {
    const re = /^\/audio\//;
    return re.test(s);
}

function alreadyEncodedLocalAudioUrl(s) {
    const r=/^(%[A-F0-9]{2}|[-_.!()~*'A-Za-z0-9;\/?:@&=+$,#])*$/;
    return localAudioUrl(s) && r.test(s);
}

function normalizeAudioUrl(md) {
    let au = md["Descr:Audio"];
    if ( au ) {
        if ( (/^[.]\//).test(au) ) {
            au = au.substring(1);
        }
        if ( ! alreadyEncodedLocalAudioUrl(au) ) {
            md["Descr:Audio"] = encodeURI(au);
        }
    }
    return md;
}

// ----------------------------------------

// runs concurrently with metadata modal show event
function fillMetaData() {
    var o  = {};
    o.cid  = activeCollectionId();
    o.path = collectionPath(o.cid);
    o.dia  = getLastMarkedEntry(o.cid);

    if (o.dia) {
        o.pos  = getEntryPos(o.dia);
        o.name = getDiaName(o.dia);
    }
    else {
        o.pos  = -1;
        o.name = o.path;
    }
    fillMetaFromServer(o);

}

function fillMetaData1(md, args) {
    console.log('fillMetaData1');
    console.log(md);
    console.log(args);

    // insert fields into metadata edit form
    writeMetaDataBox(md);
}

// ----------------------------------------

// check whether there is a marked entry
// if not, it's a noop
// else the real getMetaData is performed

function getMetaData0() {
    console.log("getMetaData0");

    var cid  = activeCollectionId();
    var dia  = getLastMarkedEntry(cid);

    if (! dia) {
        statusError('no marked image/collection found');
        return ;
    }
    console.log("getMetaData0: click ShowMetaDataButton");
    $('#ShowMetaDataButton').click();
}

function getMetaData() {
    console.log("getMetaData");

    var o  = {};
    o.cid  = activeCollectionId();
    o.path = collectionPath(o.cid);
    o.dia  = getLastMarkedEntry(o.cid);

    if (o.dia) {
        o.pos  = getEntryPos(o.dia);
        o.name = o.path + "/" + getDiaName(o.dia);
    }
    else {
        o.pos  = -1;
        o.name = o.path;
    }
    getMetaFromServer(o);
}

function showMetaData(md, args) {
    console.log('showMetaData');
    console.log(md);
    console.log(args);

    $('#ShowMetaDataModalLabel')
        .empty()
        .append('Metadata: ' + args.name);

    var kvs = [];
    $.each(md, function (k, v) {
        kvs.push([k, v]);
    });
    kvs.sort(function (p1, p2) {
        var x = p1[0];
        var y = p2[0];
        if ( x > y ) { return +1; }
        if ( x < y ) { return -1; }
        return 0;
    });
    console.log(kvs);

    var mdt = $('#ShowMetaDataTable').empty();

    kvs.forEach(function (e, i) {
        mdt.append('<tr><th>' + e[0] + '</th><td>' + e[1] + '</td></tr>');
    });

    if (args.pos >= 0) {
        // clear mark for entry invoked
        clearEntryMark($(args.dia));
    }
}

function imageCarousel() {
    var args = {};
    args.cid  = activeCollectionId();
    args.path = collectionPath(args.cid);

    console.log("imageCarousel: get collection from server:" + args.path);
    getColFromServer(args.path,
                     function (path, colVal) {
                         buildImgCarousel(args, colVal)
                     });
}

function buildImgCarousel(args, colVal) {
    var md = colVal.metadata;

    console.log("buildImgCarousel: " + args.path);
    console.log(JSON.stringify(colVal));
    console.log(md);

    if (colVal.entries.length == 0) {
        statusError("empty collection: " + args.path);
        return;
    }

    var g = previewGeo();

    // build the carousel DOM by copying the prototype
    // and rename id an did refs
    var c   = $("#prototype-carousel")
            .children("div")
            .clone();
    c.attr('id', 'image-carousel');
    c.find('[data-target="#proto-carousel"]')
        .attr('data-target', "#image-carousel");
    c.find('[href="#proto-carousel"]')
        .attr('href', "#image-carousel");

    var cli = c.find('ol.carousel-indicators li')
            .clone()
            .removeClass('active');
    var cit = c.find('div.carousel-inner div.item')
            .clone()
            .removeClass('active');

    // add the appropriate geo class for carousel images to prototype
    cit.find('div.' + imgboxclass).addClass(imgboxclass + "-" + g.geo);

    // remove prototype list of indicators and list of items
    c.find('ol.carousel-indicators').empty();
    c.find('div.carousel-inner').empty();

    // insert images
    colVal.entries.forEach(function (e, i) {
        console.log("entry: " + i);
        console.log(JSON.stringify(e));

        // append to the list of carousel indicators
        var item = $(cli).clone().attr("data-slide-to", i);
        c.find('ol.carousel-indicators').append(item);

        // append carousel item to item list
        var iscol = e.ColEntry == "COL";
        var cimg = $(cit).clone();
        var eref = splitPath(e.ref);
        var capt = "."
                + (i + 1)
                + " "
                + (iscol ? "Collection: " : "")
                + eref.name;

        cimg.find('.carousel-image-capt')
            .empty()
            .append(capt);

        var dia   = getDia(args.cid, i);
        var state = isMarkedDia(dia);
        var cls   = state ? "carousel-image-marked" : "carousel-image-unmarked";

        var rt    = getRatingVal(dia);
        console.log('init rating for entry ' + i + ' to ' + rt);
        console.log(JSON.stringify(cimg));

        // set the stars the image is rated with
        var j = 1;
        for (j = 1; j <= rt; ++j) {
            cimg.find('div.carousel-caption a' + diaStarSelector(j))
                .removeClass("carousel-image-unmarked")
                .addClass("carousel-image-marked");
        }

        // initialize image marked flag in carousel
        cimg.find(carmarksel1)
            .removeClass("carousel-image-marked")
            .removeClass("carousel-image-unmarked")
            .addClass(cls);

        // add handler for mark/unmark image via carousel
        cimg.find(carmarksel)
            .on('click', function (e) {
                var state = isMarkedDia(toggleDiaMark(args.cid, i));
                var cls   = state ? "carousel-image-marked" : "carousel-image-unmarked";
                console.log(e.target);
                $(e.target)
                    .removeClass("carousel-image-marked")
                    .removeClass("carousel-image-unmarked")
                    .addClass(cls);
                console.log(e.target);
                console.log("toggle image mark: " + args.cid + " ." + i);
            });

        // add handler for rating an image via carousel
        cimg.find('div.carousel-caption a.star')
            .on('click', function (e) {
                // var state = isMarkedDia(toggleDiaMark(args.cid, i));
                // var cls   = state ? "carousel-image-marked" : "carousel-image-unmarked";
                console.log(e.target);
                var bno = parseInt($(e.target).closest("a.star").attr("data-star"));
                var bgp = $(e.target).closest("span.carousel-stars");
                var b1m = bgp.find(diaStarSelector(1)).hasClass("carousel-image-marked");
                var b2u = bgp.find(diaStarSelector(2)).hasClass("carousel-image-unmarked");
                if (bno === 1 && b1m && b2u) {
                    bno = 0;
                }
                var j = 0;
                for (j = 0; j <= 5; ++j) {
                    var cls = j <= bno ? "carousel-image-marked" : "carousel-image-unmarked";
                    bgp.find(diaStarSelector(j))
                        .removeClass("carousel-image-marked")
                        .removeClass("carousel-image-unmarked")
                        .addClass(cls);
                }
                // console.log(e.target);
                console.log("set rating: " + args.cid + " ." + i + "= " + bno);
                console.log(args);
                setRatingOnServer(args.cid, args.path, i, bno);
            });


        // insert the icon ref into cimg

        var iref = iconRef(previewGeo().img, args.path, e, i);

        console.log("new iconref: " + iref);
        cimg.find("div." + imgboxclass + " img")
            .attr('src', iref)
            .attr('alt', eref.name);
        c.find('div.carousel-inner').append(cimg);

    }); // end forEach loop

    // set the initially active slide
    c.find("li[data-slide-to='0']")
        .addClass('active');
    c.find("div.carousel-inner div:first-child")
        .addClass('active');

    $('#CarouselModal')
        .off('keypress')
        .on('keypress', function(e){
            console.log('CarouselModal:');
            console.log(e);
            var charCode = e.which || e.keyCode;
            if ( charCode == 109 ) {  // 'm': mark
                console.log('keypress: toggle image mark');
                $('#CarouselModalBody div.carousel-inner div.item.active ' + carmarksel1).click();
            }
            else
                if ( charCode == 110
                     ||
                     charCode == 62  // nice try: only with keyup or keydown event
                   ) { // 'n' or '>': next image
                    console.log('keypress: next image');
                    $("#CarouselModalBody .carousel-control-next").click();    // bootstrap 4
                    $("#CarouselModalBody .carousel-control.right").click();   // bootstrap 3
                }
            else
                if ( charCode == 112
                     ||
                     charCode == 60
                   ) { // 'p'or '<': prev image
                    console.log('keypress: prev image');
                    $("#CarouselModalBody .carousel-control-prev").click();   // bootstrap 4
                    $("#CarouselModalBody .carousel-control.left").click();   // bootstrap 3
                }
            else
                if ( charCode >= 48 && charCode <= 53) { // '0'..'5' set rating
                    var rating = charCode - 48;
                    var sel = '[data-star="' + rating + '"]';
                    console.log('keypress: set rating to ' + rating);
                    $('#CarouselModalBody div.carousel-inner div.item.active ' + sel).click();
                }
            return false;
        });

    $('#CarouselModal')
        .off('keyup')
        .on('keyup', function(e){
            console.log('CarouselModal:');
            console.log(e);
            var charCode = e.keyCode;
            if ( charCode == 39
               ) { // right arrow
                console.log('keypres: next image');
                $("#CarouselModalBody .carousel-control.right").click();
            }
            else if ( charCode == 37
                    ) { // left arrow
                console.log('keypres: prev image');
                $("#CarouselModalBody .carousel-control.left").click();
            }
            return false;
        });


    // configure the modal box and its size
    $('#CarouselModal > div.modal-dialog')
        .attr('class', 'modal-dialog modal-carousel-' + g.geo);
    $('#CarouselModalBody')
        .empty()
        .append(c);

    var clab = "";
    var ttt  = md["Descr:Title"];
    if ( ttt ) {
        clab += "<h3>" + ttt + "<h3>"
    }
    ttt  = md["Descr:Subtitle"];
    if ( ttt ) {
        clab += "<h4>" + ttt + "<h4>"
    }
    clab += "<h5>" + args.path + "</h5>";

    $('#CarouselModalLabel')
        .empty()
        .append(clab);

    $('#CarouselModal').modal('show');
}

function previewImage() {
    var args = {};
    args.cid  = activeCollectionId();
    args.path = collectionPath(args.cid);
    args.dia  = getLastMarkedEntry(args.cid);
    if (! args.dia) {
        statusError('no marked image/collection found');
        return ;
    }
    args.iscol  = $(args.dia).hasClass('colmark');
    args.pos    = getEntryPos(args.dia);
    args.name   = getDiaName(args.dia);
    args.fmt    = previewGeo().img;
    clearEntryMark($(args.dia));

    $('#PreviewModal > div.modal-dialog')
        .attr('class', 'modal-dialog modal-preview-' + previewGeo().geo);

    $('#PreviewModalBody > div')
        .addClass(hiddenclass);

    $('#PreviewModalBlog')    /* blog text is hidden */
        .addClass(hiddenclass);

    $('#PreviewModalImg')     /* image div is hidden */
        .addClass(hiddenclass);

    $('#PreviewModalMovie')   /* movie div is hidden */
        .addClass(hiddenclass);

    $('#PreviewModalLabel')
        .empty()
        .append('Preview: ' + args.path + "/" + args.name);

    // if dia has a "data-mp4" class, compute the movie ref
    // and insert it in the movie div
    // else
    // if dia has a .jpg then load the preview ref and show the image
    // via call back insertPreviewRef

    // images are tagged with "data-jpg"
    // movies are tagged with "data-jpg" and "data-mp4"
    // blog entries are tagged with "data-md"
    // collections are tagged with "data-jpg" and "data-md"

    if ( $(args.dia).hasClass('data-mp4') ) {  // movie
        getMovieRef(args);
    }
    else if ( $(args.dia).hasClass('data-jpg') ) { // image or coll
        getPreviewRef(args);
    }

    if ( $(args.dia).hasClass('data-md') ) { // blog entry or coll
        getBlogText(args);
    }
}

function insertPreviewRef(ref, args) {
    // come from: previewImage and getPreviewRef
    console.log('insertPreviewRef');
    console.log(ref);
    console.log(args);

    // make the div for images visible
    $('#PreviewModalImg').removeClass(hiddenclass);

    // insert the image ref, browser will load and show the image
    $('#PreviewModalImgRef')
        .attr('src', ref)
        .attr('alt', args.path);

    $('#PreviewModal').modal('show');
}

function insertMovieRef0(ref, args) {
    args.movieRef = ref;
    getMovieMeta(args);
}

function insertMovieRef(meta, args) {
    var ref  = args.movieRef;
    var geo  = meta["Composite:ImageSize"];
    var movW = meta["QuickTime:ImageWidth"];
    var movH = meta["QuickTime:ImageHeight"];

    // compute width and height of movie window
    var g = fitGeo(movW, movH);

    // come from: previewImage and getMovieRef
    console.log('insertMovieRef');
    console.log(ref);
    console.log(meta);
    console.log(geo);
    console.log(movW);
    console.log(movH);
    console.log(args);

    // make the div for images visible
    $('#PreviewModalMovie')
        .empty()
        .append('<video class="data-mp4" controls="" width="' + g.w + '" height="' + g.h + '">'
                + '<source id="PreviewModalMovieRef" src="' + ref + '" type="video/mp4">'
                + '</video>'
               )
        .removeClass(hiddenclass);

    $('#PreviewModal').modal('show');
}

function closePreviewModal() {
    // statusMsg("Preview closed");
    // $('#PreviewModalMovieRef')
    //     .removeAttr('src');

    $('#PreviewModalMovie')
        .empty();
}

function insertBlogText(txt, args) {
    console.log('insertBlogText');
    console.log(txt);
    console.log(args);

    if (txt === "") {
        console.log('empty text');
        return;
    }
    // make the div for images visible
    $('#PreviewModalBody div.data-md').removeClass(hiddenclass);
    $('#PreviewModalBlog').removeAttr('hidden');
    $('#PreviewModalBlog')
        .empty()
        .append(txt);

    $('#PreviewModal').modal('show');
}

// ----------------------------------------

function blogEdit() {
    statusClear();
    var args  = {};
    args.cid  = activeCollectionId();
    args.path = collectionPath(args.cid);
    args.img  = getLastMarkedEntry(args.cid);
    if (! args.img) {
        statusError("no blog entry marked in: " + args.path);
        return;
    }
    args.isimg  = $(args.img).hasClass('imgmark');
    args.iscol  = $(args.img).hasClass('colmark');
    args.isblog = $(args.img).hasClass('data-md');

    if (args.isimg && ! args.isblog) {
        statusError("last marked isn't a blog entry in: " + args.path);
        return;
    }
    args.iname = getDiaName(args.img);
    args.pos   = getEntryPos(args.img);

    var hdl = "???";
    if (args.iscol) {
        hdl = 'Edit blog text for collection: ';
    }
    if (args.isimg) {
        hdl = 'Edit blog entry: ';
    }

    // prepare the edit modal title
    $('#EditBlogModalLabel')
        .empty()
        .append(hdl + args.path + "/" + args.iname);

    console.log('blogEdit');
    console.log(args);

    clearEntryMark($(args.img));

    // on the highway to call back hell
    getBlogTextForEdit(args);
}

function insertBlogTextForEdit(res, args) {
    // come from: blogEdit

    console.log('insertBlogTextForEdit');
    console.log(res);

    $('#EditBlogContents')
        .empty()
        .val(res);

    // install event handler for OK button
    // and pass the args data to the handler
    $('#EditBlogOK')
        .off('click')
        .on('click', function () {
            console.log("EditBlogOK clicked");
            $('#EditBlogModal').modal('hide');
            saveBlogText(args);
        });

    // modal box is configured,
    // show it and wait for the save button beeing clicked
    $('#BlogEditButton').click();
}

function saveBlogText(args) {
    // come from: insertBlogTextForEdit
    var res = $('#EditBlogContents').val();
    console.log('saveBlogText');
    console.log(args);
    console.log(res);

    saveBlogTextFromEdit(args, res);
}

// ----------------------------------------

// ajax calls

function removeFromColOnServer(path, args) {
    addHistCmd("remove from " + splitName(path),
               function () {
                   modifyServer("removeFromCollection", path, args,
                                function () {
                                    getColFromServer(path, refreshCollection1);
                                });
               });
}

function copyToColOnServer(spath, dpath, args) {
    addHistCmd("copy from " + splitName(spath) + " to " + splitName(dpath),
               function () {
                   copyMoveToColOnServer("copyToCollection", spath, dpath, args);
               });
}

function moveToColOnServer(spath, dpath, args) {
    addHistCmd("move from " + splitName(spath) + " to " + splitName(dpath),
               function () {
                   copyMoveToColOnServer("moveToCollection", spath, dpath, args);
               });
}

function copyMoveToColOnServer(cpmv, spath, dpath, args) {
    modifyServer(cpmv, spath, [args, dpath],
                 function () {
                     getColFromServer(spath, refreshCollection );
                     getColFromServer(dpath, refreshCollection1);
                 });
}

function sortColOnServer(path, ixs) {
    addHistCmd("sort in " + splitName(path),
               function () {
                   modifyServer("sort", path, ixs,
                                function () {
                                    getColFromServer(path, refreshCollection);
                                });
               });
}

function sortColByDateOnServer(path, ixs) {
    addHistCmd("sort by date in " + splitName(path),
               function () {
                   modifyServer("sortByDate", path, ixs,
                                function () {
                                    getColFromServer(path, refreshCollection);
                                });
               });
}

function changeWriteProtectedOnServer(path, ixs, ro, opcs) {
    addHistCmd("write protect in " + splitName(path),
               function () {
                   modifyServer("changeWriteProtected", path, [ixs, ro],
                                function () {
                                    getColFromServer(path, refreshCollectionF);
                                    markWriteProtected(opcs, ro);
                                });
               });
}

function setMetaOnServer(path, ixs, metadata) {
    var entrymeta = anyMarked(ixs);

    addHistCmd("set metadata " + ( entrymeta ? "in" : "of") + " " + splitName(path),
               function () {
                   function cb() {
                       getColFromServer(path, refreshCollectionF);
                   };

                   if (entrymeta) {
                       modifyServer("setMetaData", path, [ixs, metadata], cb);
                   }
                   else {
                       modifyServer("setMetaData1", path, [-1, metadata], cb);
                   }
               });
}

function setRatingOnServer(cid, path, ix, rating) {
    console.log('setRatingOnserver:' + path + ", " + ix + ", " + rating);
    addHistCmd("set rating in " + splitName(path),
               function () {
                   modifyServer("setRating1", path, [ix, rating],
                                function () {
                                    setRatingInCollection(cid, ix, rating);
                                });
               });
}

function setRatingsOnServer(cid, path, ixs, rating) {
    console.log('setRatingsOnserver:' + path + ", " + ixs + ", " + rating);
    addHistCmd("set ratings in " + splitName(path),
               function () {
                   modifyServer1("setRating", path, [ixs, rating],
                                 function () {
                                     setRatingsInCollection(cid, ixs, rating);
                                 });
               });
}

function getHistIdFromServer(cname, processRes) {
    console.log('getHistIdFromServer');
    modifyServer1("newUndoEntry", pathArchive(), cname,
                 function (hid) {
                     console.log("getHistIdFromServer: hid=" + hid);
                     processRes(hid);
                 });
}

function resetHistInServer(hid, cont) {
    console.log("resetHistInServer hid=" + hid);
    modifyServer("applyUndo", pathArchive(), hid, cont);
}

function dropHistAtInServer(hid) {
    console.log("dropHistAtInServer id=" + hid);
    modifyServer("dropUndoEntries", pathArchive(), hid, noop);
}

function getHistoryFromServer(cont) {
    console.log("getHistFromServer");
    modifyServer1("listUndoEntries", pathArchive(), [],
                  function (hs) {
                      console.log("undo history");
                      console.log(hs);
                      cont(hs);
                  });
}

function getColFromServer(path, showCol) {
    readServer("collection", path,
               function (col) {
                   col.metadata = col.metadata;
                   showCol(path, col);
               });
}

function getIsWriteableFromServer(path, markWriteable) {
    readServer('isWriteable', path, markWriteable);
}

function getIsColFromServer(path, cleanupCol) {
    readServer('isCollection', path, cleanupCol);
}

function createColOnServer(path, name, showCol) {
    addHistCmd("new collection " + name,
               function () {
                   modifyServer("newcol", path, name,
                                function () {
                                    getColFromServer(path, refreshCollection);
                                });
               });
}

function renameColOnServer(cpath, path, newname, showCol) {
    addHistCmd("rename " + splitName(path) + " to " + newname,
               function () {
                   modifyServer("renamecol", path, newname,
                                function () {
                                    getColFromServer(cpath, refreshCollection1);
                                });
               });
}

function fillMetaFromServer(args) {
    // thread the args object into the callback function
    readServer1('metadata',
                args.path,
                args.pos,
                function (res) {
                    fillMetaData1(res, args);
                }
               );
}

function getMetaFromServer(args) {
    // thread the args object into the callback function
    readServer1('metadata',
                args.path,
                args.pos,
                function (res) {
                    showMetaData(res, args);
                }
               );
}

function getRatingFromServer(path, pos, setRating) {
    readServer1('rating', path, pos, setRating);
}

function getAllRatingsFromServer(colId, cont) {
    var path = collectionPath(colId);
    getRatingsFromServer(path,
                         function(ratings) {
                             setAllRatingsInCollection(colId, ratings);
                             cont();
                         });

}

// TODO: substitute getRatingFromServer for every picture
// with getRatingsFromServer for the whole collection
function getRatingsFromServer(path, setRating) {
    readServer('ratings', path, setRating);
}

function getPreviewRef(args) {
    var entry = { ColEntry : args.iscol ? "COL" : "IMG",
                  ref : args.path + "/" + args.name
                };
    var res = iconRef(args.fmt, args.path, entry, args.pos);
    insertPreviewRef(res, args);
}

// --------------------
// an example for callback hell:
// getMovieRef, insertMovieRef0, getMovieMeta, insertMovieRef

function getMovieRef(args) {
    var ref = args.path + "/pic-" + fmtIx(args.pos);
    args.pathpos = ref;
    console.log("getMovieRef: " + ref);
    readServer("mediaPath",
               ref,
               function (res) { insertMovieRef0(res, args); }
              );
}

function getMovieMeta(args) {
    var ref = args.path;
    console.log("getMovieMeta: " + ref);
    readServer1("metadata",
                args.path,
                args.pos,
                function (res) {
                    insertMovieRef(res, args);
                }
               );
}
// --------------------

function getBlogText(args) {
    readServer1('blogcontents',
                args.path,
                args.pos,
                function (res) { insertBlogText(res, args); }
               );
}

function getBlogTextForEdit(args) {
    readServer1('blogsource',
                args.path,
                args.pos,
                function (res) { insertBlogTextForEdit(res, args); }
               );
}

function saveBlogTextFromEdit(args, text) {
    modifyServer("saveblogsource",
                 args.path,
                 [args.pos, text],
                 function () {
                     statusMsg('blog contents saved for: ' + args.iname);
                 });
}

function saveImgStoreStart() {
    // statusClear();
    statusMsg('taking snapshot of image archive, one moment please');
    var name = $('#saveImgStoreName').val();
    $('#saveImgStoreName').val('');
    saveImgStore(name);
}

function saveImgStore(text) {
    console.log('saveImgStore: ' + text);
    addHistCmd("save catalog",
               function () {
                   modifyServer("snapshot",
                                pathArchive(),
                                text,
                                function () {
                                    statusMsg('snapshot of image archive taken');
                                });
               });
}

// ----------------------------------------
/*
// http communication

function callServer(getOrModify, fct, args, processRes, processNext) {
    if (serverVersion.server === "catalog-servant") {
        callServantServer(getOrModify, fct, args, processRes, processNext);
    } else {
        callScottyServer(getOrModify, fct, args, processRes, processNext);
    }
}

// --------------------

function callServantServer(getOrModify, fct, args, processRes, processNext) {
    var rpc = [fct, args];
    console.log('callServantServer: ' + getOrModify);
    console.log(rpc);
    console.log(JSON.stringify(rpc));

    $.ajax({
        type: "POST",
        url: "/" + getOrModify + '/' + fct,
        data: JSON.stringify(args),
        dataType: 'json'
    }).done(function (res) {
            processRes(res);
    }).fail(function (err){
        statusError(err.resposeText);
    }).always(processNext);
}

// --------------------

function callScottyServer(getOrModify, fct, args, processRes, processNext) {
    var rpc = [fct, args];
    console.log('callScottyServer: ' + getOrModify);
    console.log(rpc);
    console.log(JSON.stringify(rpc));

    $.ajax({
        type: "POST",
        url: "/" + getOrModify + '.json',
        data: JSON.stringify(rpc),
        dataType: 'json'
    }).done(function (res) {
        if (res.err) {
            statusError(res.err);
        } else {
            processRes(res);
        }
    }).fail(function (err){
        statusError(err.resposeText);
    }).always(processNext);
}
 */

// ----------------------------------------

// make a query call to server

function readServer(fct, path, processRes) {
    readServer1(fct, path, [], processRes);
    // callServer("get", fct, [path, []], processRes, noop);
}

function readServer1(fct, path, args, processRes) {
    callServer("get", fct, [path, args], processRes, showError, noop);
}

// make a modifying call to server
// modifying calls usualls don't get an interesting result back
// in success case (), else the error message
// so most modifying ops are procedures, not functions

function modifyServer(fct, path, args, processNext) {
    callServer("modify", fct, [path, args], ignoreRes, showError, processNext);
}

// a modify with a result, e.g. a log file of the operation
function modifyServer1(fct, path, args, processRes) {
    callServer("modify", fct, [path, args], processRes, showError, noop);
}

function ignoreRes(res) {}

function noop() {}

function showError(err) {
    statusError(err);
}

// ----------------------------------------
//
// the "main" program
// set the event handlers

$(document).ready(function () {
    $('#collectionTab a').click(function (e) {
        e.preventDefault();
        $(this).tab('show');
    });

    openSystemCollections();

    // event handler for navbar buttons
    $("#CloseButton")
        .on('click', function (e) {
            closeCollection(activeCollectionId());
        });

    $("#MoveAllFromClipboardButton")
        .on('click', function (e) {
            moveAllFromClipboard(activeCollectionId());
        });

    $("#CopyAllFromClipboardButton")
        .on('click', function (e) {
            copyAllFromClipboard(activeCollectionId());
        });

    $("#MoveAllToClipboardButton")
        .on('click', function (e) {
            moveAllToClipboard(activeCollectionId());
        });

    $("#CopyAllToClipboardButton")
        .on('click', function (e) {
            copyAllToClipboard(activeCollectionId());
        });

    $("#MoveFromClipboardButton")
        .on('click', function (e) {
            moveMarkedFromClipboard(activeCollectionId());
        });

    $("#CopyFromClipboardButton")
        .on('click', function (e) {
            copyMarkedFromClipboard(activeCollectionId());
        });

    $("#MoveToClipboardButton")
        .on('click', function (e) {
            moveMarkedToClipboard(activeCollectionId());
        });

    $("#CopyToClipboardButton")
        .on('click', function (e) {
            copyMarkedToClipboard(activeCollectionId());
        });

    $("#RemoveButton")
        .on('click', function (e) {
            removeMarkedFromClipboard();
        });

    $("#CollectionImgButton")
        .on('click', function (e) {
            setCollectionImg(activeCollectionId());
        });

    $('#newCollectionButton')
        .on('click', function () {
            statusClear();
        });

    $('#newCollectionModal')
        .on('show.bs.modal', function () {
            statusClear();
        });

    $('#newCollectionModal')
        .on('hidden.bs.modal', function () {
            $('#newCollectionButton').blur();
        });

    $('#newCollectionOK')
        .on('click', function (e) {
            console.log("newCollectionOK clicked");
            $('#newCollectionModal').modal('hide');
            createCollection();
        });

    $('#WriteProtectedButton')
        .on('click', function () {
            statusClear();
            writeProtectedCollection();
        });

    $('#RenameCollectionButton0')
        .on('click', function () {
            statusClear();
            renameCollectionCheck();
        });

    $('#RenameCollectionOK')
        .on('click', function (e) {
            console.log("renameCollectionOK clicked");
            $('#RenameCollectionModal').modal('hide');
            renameCollection();
        });

    $("#SortButton")
        .on('click', function (e) {
            statusClear();
            sortCollection(activeCollectionId());
        });

    $("#SortByDateButton")
        .on('click', function (e) {
            statusClear();
            sortCollByDate(activeCollectionId());
        });

    $('#MetaDataButton')
        .on('click', function () {
            statusClear();
            fillMetaData();
        });

    $('#MetaDataModal')
        .on('show.bs.modal', function () {
            statusClear();
        });

    $('#MetaDataOK')
        .on('click', function (e) {
            console.log("MetaDataOK clicked");
            $('#MetaDataModal').modal('hide');
            setMetaData();
        });

    $('#MetaDataClear')
        .on('click', function (e) {
            console.log("MetaDataClear clicked");
            clearMetaDataBox();
        });

    $('#MetaDataCopy')
        .on('click', function (e) {
            console.log("MetaDataCopy clicked");
            copyMetaData();
        });

    $('#MetaDataPaste')
        .on('click', function (e) {
            console.log("MetaDataPaste clicked");
            pasteMetaData();
        });

    $('#PreviewButton')
        .on('click', function () {
            statusClear();
            previewImage();
        });

    $('#CarouselButton')
        .on('click', function () {
            statusClear();
            imageCarousel();
        });
    /*
    // clear status line on modal closed

    $('#CarouselModal')
        .on('hidden.bs.modal', statusClear);
    */

    $('#ShowMetaDataModal')
        .on('show.bs.modal', function () {
            console.log("ShowMetaDataModal.click()");
            statusClear();
            getMetaData();
        });

    $('#BlogEditButton0')
        .on('click', function () {
            statusClear();
            blogEdit();
        });

    // #BlogEditButton triggers the modal box
    // it is invoked by blogEdit handler

    // mark menue event handler
    $('#MarkAll')   .on('click', function (e) { eachADia(isUnmarkedDia,       setEntryMark); });
    $('#Mark1')     .on('click', function (e) { eachADia(hasRating(geInt(1)), setEntryMark); });
    $('#Mark2')     .on('click', function (e) { eachADia(hasRating(geInt(2)), setEntryMark); });
    $('#Mark3')     .on('click', function (e) { eachADia(hasRating(geInt(3)), setEntryMark); });
    $('#Mark4')     .on('click', function (e) { eachADia(hasRating(geInt(4)), setEntryMark); });
    $('#Mark5')     .on('click', function (e) { eachADia(hasRating(geInt(5)), setEntryMark); });
    $('#ToggleMark').on('click', function (e) { eachADia(constTrue,           toggleMark); });
    $('#UnmarkAll') .on('click', function (e) { eachADia(isMarkedDia,         clearEntryMark); });
    $('#Unmark1')   .on('click', function (e) { eachADia(hasRating(eqInt(1)), clearEntryMark); });
    $('#Unmark2')   .on('click', function (e) { eachADia(hasRating(eqInt(2)), clearEntryMark); });
    $('#Unmark3')   .on('click', function (e) { eachADia(hasRating(eqInt(3)), clearEntryMark); });
    $('#Unmark4')   .on('click', function (e) { eachADia(hasRating(eqInt(4)), clearEntryMark); });
    $('#Unmark5')   .on('click', function (e) { eachADia(hasRating(eqInt(5)), clearEntryMark); });

    // rating menue event handler
    [0,1,2,3,4,5].forEach(function (e, i) {
        $('#Rating' + i)
            .on('click', function () {
                console.log("set ratings to " + i);
                setRating(i);
            });
    });


    $('#saveImgStoreOK')
        .on('click', function (e) {
            console.log("saveImgStoreOK clicked");
            $('#saveImgStoreModal').modal('hide');
            saveImgStoreStart();
        });

    $('#SyncCollection')
        .on('click', function () {
            // statusClear();
            statusMsg('sync collection with images on the filesystem, one moment please');
            syncActiveCollection('syncCol');
        });

    $('#SyncExif')
        .on('click', function () {
            // statusClear();
            statusMsg('recomputing exif data');
            exifActiveCollection();
        });

    $('#NewCollection')
        .on('click', function () {
            // statusClear();
            statusMsg('import new subcollections collection from filesystem, one moment please');
            syncActiveCollection('newSubCols');
        });

    // refresh all collections, just a debug op
    $('#RefreshCollection')
        .on('click', function () {
            statusClear();
            // statusMsg('refreshing all open collections');
            checkAllColAreThere(true, true);
        });

    $('#ClearUndoHistory')
        .on('click', function () {
            statusClear();
            /* clearUndoHistory(); */
            remUndoUntilLastSave();
        });

    $('#ConsistencyCheck')
        .on('click', function () {
            statusClear();
            // statusMsg('refreshing all open collections');
            checkArchiveConsistency();
        });

    $('#dropdownUndoHistory')
        .on('click', function () {
            // statusMsg("Open Undo History");
            setHistory();
        });

    $('#PreviewModal')
        .on('hidden.bs.modal', closePreviewModal);

});

// ----------------------------------------
//
// "constants"

function pathArchive()     { return "/archive"; }
function pathCollections() { return "/archive/collections"; }
function pathClipboard()   { return "/archive/collections/clipboard"; }
function pathPhotos()      { return "/archive/collections/photos"; }
function pathTimeline()    { return "/archive/collections/timeline"; }
function pathImports()     { return "/archive/collections/imports"; }
function pathTrash()       { return "/archive/collections/trash"; }

function idCollections() { return path2id(pathCollections()); }
function idClipboard()   { return path2id(pathClipboard()); }
function idTrash()       { return path2id(pathTrash()); }

function iconSize() { return "pad-160x160"; }

// ----------------------------------------
//
// new URL scheme functions

function iconPrefix(padSize) {
    return "/docs/iconp/" + padSize.slice(4);
}

function iconRef(padSize, colPath, entry, ix) {
    return iconPrefix(padSize)
           + toRef(colPath, entry, ix)
           + ".jpg"
           + uniqueIconRef();
}

function toRef(colPath, entry, ix) {
    if (entry.ColEntry == "IMG") {
        return colPath + "/pic-" + fmtIx(ix);
    }
    if (entry.ColEntry == "COL") {
        return entry.ref;
    }
}

function fmtIx(ix) {
    return ("0000" + ix).slice(-4);
}

// disable caching of images by appending
// a unique ref count "?<cnt>" to the url
//
// when images have been sorted the img url (path + imgNum)
// may reference another image than before sorting
//
// this hack works independently of browser cache strategie
// no response headers need to be generated by the server
//
// --> servant servers maybe simplified

var iconRefPx = "?" + Date.now() + "-";
var iconRefCount = 0;

function uniqueIconRef() {
    iconRefCount++;
    return iconRefPx + iconRefCount;
}

// ----------------------------------------
//
// one of 1600x1200, 1400x1050, 1280x800, 900x60
// else edit.css must be extended
// .modal-preview, .modal-carousel, .img-box

function previewGeo() {
    var g = window.screen;
    if (g.width === 2560 && g.height === 1440) {
        return previewGeoXY(1600, 1200);
    }
    return previewGeoXY(1280,800);
}

function previewGeoXY(x, y) {
    var o = {w : x, h: y};
    o.geo = "" + o.w + "x" + o.h;
    o.img = "pad-" + o.geo;
    return o;
}

function previewMovieGeo(w, h) {
    var o = previewGeo();
    var res = fitGeo(w, h, o.w, o.h);
    console.log("previewMovieGeo");
    console.log(res);
    return res;
}

function fitGeo(orgW, orgH) {
    var pg   = previewGeo();
    var pad  = 32;
    var dstW = pg.w - pad;
    var dstH = pg.h - pad;
    var res = {};
    if (orgW <= dstW && orgH <= dstH) {
        res.w = orgW; res.h = orgH;
    } else {
        var orgAR = orgW / orgH;
        var dstAR = dstW / dstH;
        if ( orgAR >= dstAR ) {
            res.w = dstW;
            res.h = Math.floor(dstW / orgAR);
        } else {
            res.h = dstH;
            res.w = Math.floor(dstH * orgAR);
        }
    }
    return res;
}

// ----------------------------------------
