// catalog server with servant
//
// rpc communication

var serverVersion = { "server"  : "servant",
                      "version" : "0.5.14.10",
                      "date"    : "2026-02-17"
                    };

// --------------------

function callServer(getOrModify, fct, args, processRes, processErr, processNext) {
    var rpc = [fct, args];
    var arg0 = args[0];
    var arg1 = args[1];

    console.log('callServantServer: ' + getOrModify);
    console.log(rpc);
    console.log(JSON.stringify(rpc));
    console.log(arg0);
    console.log(arg1);

    $.ajax({
        type: "POST",
        contentType: "application/json",
        url: "/" + getOrModify + '/' + fct + arg0,
        data: JSON.stringify(arg1),
        dataType: 'json'
    }).done(function (res) {
        processRes(res);
    }).fail(function (err){
        console.log("got server error");
        console.log(err);
        console.log(err.status);
        if (err.status == 500) {
            // internal server error
            // remove name of the function raising the error
            var msg = err.responseJSON || err.responseText;
            console.log(msg);
            processErr(msg.replace(/[^:]*: /, ""));
        } else
            if (err.status == 404) {
                processErr("unimplemented server operation: " + fct);
            } else {
                processErr("server error: " + err.status +
                           " when processing operation " + fct);
            }
    }).always(processNext);
}

// ----------------------------------------

// make a query call to server

function readServer(fct, path, processRes) {
    readServer1(fct, path, [], processRes);
    // callServer("get", fct, [path, []], processRes, noop);
}

function readServer1(fct, path, args, processRes) {
    callServer("get", fct, [path, args], processRes, userErrorHandler, noop);
}

// make a modifying call to server
// modifying calls usualls don't get an interesting result back
// in success case (), else the error message
// so most modifying ops are procedures, not functions

function modifyServer(fct, path, args, processNext) {
    callServer("modify", fct, [path, args], ignoreRes, userErrorHandler, processNext);
}

// a modify with a result, e.g. a log file of the operation
function modifyServer1(fct, path, args, processRes) {
    callServer("modify", fct, [path, args], processRes, userErrorHandler, noop);
}

function ignoreRes(res) {}

function noop() {}

// defaut error report
// can be overwritten in edit.js or zoom.js

function statusErrorDefault(err) {
    console.log("server error: " + err);
}

var userErrorHandler = statusErrorDefault;

// --------------------

// simple server calls for reading

function getMetaFromServer(path, pos, processMeta) {
    readServer1('metadata', path, pos, processMeta);
}

function getKeywordColPathFromServer(kw, processKW) {
    readServer1('keywords', "/dummy-path", [kw], processKW);
}

function getRatingFromServer(path, pos, setRating) {
    readServer1('rating', path, pos, setRating);
}

// TODO: substitute getRatingFromServer for every picture
// with getRatingsFromServer for the whole collection
function getRatingsFromServer(path, setRating) {
    readServer('ratings', path, setRating);
}

function getIsWriteableFromServer(path, markWriteable) {
    readServer('isWriteable', path, markWriteable);
}

function getIsColFromServer(path, cleanupCol) {
    readServer('isCollection', path, cleanupCol);
}


// ----------------------------------------
