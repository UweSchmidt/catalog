// --------------------
//
// usefull basic functions

// --------------------

function trc (t, txt) {
    if ( t > 0 ) {
        console.log(txt);
    }
}

function trc2(txt, x) {
    console.log(`${txt} ${x ? JSON.stringify(x) : ""}`);
}

// null represents the empty tuple ()
// Void :: ()                           // void is a keyword
const Void = null;

// --------------------
//
// arithmetc ops

// for arithmetic in folds
function add(x, y)  { return x + y; }
function sub(x, y)  { return x - y; }
function mul(x, y)  { return x * y; }
function quot(x, y) { return x / y; }

// integer div and mod
function div(x, y) { return Math.floor(x / y); }
function mod(x, y) { return Math.floor(x % y); }

// predicates
function odd(x)  { return mod(x, 2) === 1; }
function even(x) { return mod(x, 2) === 0; }

function eq(x, y) { return x === y; }
function ne(x, y) { return x !=  y; }
function ge(x, y) { return x >=  y; }
function gr(x, y) { return x >   y; }
function le(x, y) { return x <=  y; }
function lt(x, y) { return x <  y; }

// --------------------
//
// type checks

function isDefined(x) {
    return x !== undefined;
}

function isEmpty(x) {
    return x === undefined || x === null || x === "";
}

function isString(x) {
    return typeof x === 'string';
}

function isObject(x) {
    // JS is full of warts

    return typeof x === 'object'
        && ! Array.isArray(x)
        && x !== null;
}

function isNumber(x) {
    return  typeof x === 'number';
}

function isPositive(x) {
    return isNumber(x) && x > 0;
}

function isNegative(x) {
    return isNumber(x) && x < 0;
}

function toNum(x) {
    return 1 * x;
}

function isUpper(x) {
    const c = x.charCodeAt(0);
    return c >= 'A'.charCodeAt(0)
        && c <= 'Z'.charCodeAt(0);
}

function isLower(x) {
    const c = x.charCodeAt(0);
    return c >= 'a'.charCodeAt(0)
        && c <= 'z'.charCodeAt(0);
}

function isAlpha(x) {
    return isUpper(x) || isLower(x);
}

function isDigit(x) {
    const c = x.charCodeAt(0);
    return c >= '0'.charCodeAt(0)
        && c <= '9'.charCodeAt(0);
}

function isAlphaNum(x) {
    return isAlpha(x) || isDigit(x);
}

// --------------------
//
// string functions

function fromOrd(...args) {
    return String.fromCharCode(...args);
}

// (String, ...) -> String
function concatS(...args) {
    return args.join("");
}

function replicate(n, s) {
    if (n < 1)
        return "";
    if (n < 2)
        return s;
    if (odd(n))
        return s + replicate(n - 1, s);
    if (even(n)) {
        const s1 = replicate(div(n,2), s);
        return s1 + s1;
    };
}

function fillR(n, xs) {
    return xs + replicate(n - xs.length, " ");
}

function fillL(n, xs) {
    return replicate(n - xs.length, " ") + xs;
}


// simple quoting
// works only if s doen't contain double quotes

function qt(s) {
    return '"' + s + '"';
}

// intercalate a list of strings

function intercalate(s, xs) {
    return xs.join(s);
}

function lines(txt) {
    return txt.split("\n");
}

function take(n, xs) {
    return xs.slice(0, Math.max(0, n));
}

function drop(n, xs) {
    return xs.slice(Math.max(0, n));
}

function unlines(xs) {
    return intercalate("\n", xs);
}

function unwords(xs) {
    return intercalate(" ", xs);
}

function nl(s) { return s + "\n"; }

// --------------------
//
// functions

function id(x)     { return x; }
function cnst(c)   { return (x) => { return c; }; }

function fst(x, y) { return x; }
function snd(x, y) { return y; }

// function composition
// comp(f1, f2, f3)(v) === f3(f2(f1(v)))

function comp(...fs) {
    function composed(x) {
        let res = x;
        for (const f of fs) {
            res = f(res);
        }
        return res;
    }

    return composed;
}

// flip
// ((a1, a2) -> b) -> ((a2,a1) -> b)

function flip(f) {
    function f1(x, y) {
        return f(y, x);
    }
    return f1;
}

// currying
// (a1 -> a2 -> b) -> (a1 -> a2 -> b)

function curry(f) {
    function f1(x1) {
        function f2(x2) {
            return f(x1, x2);
        }
        return f2;
    }
    return f1;
}

// currying
// (a1 -> (a2, ...) -> b) -> (a1 -> (a2, ...) -> b)

function curry1(f) {
    function f1(x1) {
        function f2(...x2) {
            return f(x1, ...x2);
        }
        return f2;
    }
    return f1;
}

// add the value computed by function fct1 as fst arg to a function f

function withArg1(fct1, f) {
    function go(...args) {
        return f(fct1(), ...args);
    }
    return go;
}

// withArg :: (() -> m a) -> (a1 -> a2 -> ... -> m b) -> (a2 -> ... -> m b)
const withArg = curry(withArg1);

// --------------------
//

// curried fold

function foldl(f2, unit) {
    function go(xs) {
        let res = unit;
        for (const x of xs) {
            res = f2(res, x);
        }
        return res;
    }
    return go;
}

const sum     = foldl(add, 0);
const product = foldl(mul, 1);

// curried map function
// process all elements of an iterable and
// build a list of all result values

function map(f) {
    function go(xs) {
        let res = [];
        for (const x of xs) {
            res.push(f(x));
        }
        return res;
    }
    return go;
}

const toList = map(id);

// curried filter function
function filter(p) {
    function go(xs) {
        let ys = [];
        for (const x of xs) {
            if (p(x)) {
                ys.push(x);
            }
        }
        return ys;
    }
    return go;
}

// curries zipWith function
// process all elements of two iterables and
// zip the results together into a list

function zipWith(f) {
    function go(xs, ys) {
        const iys = ys[Symbol.iterator]();
        let res = [];
        for (const x of xs) {
            const iy = iys.next();
            if ( iy.done )
                break;
            res.push(f(x, iy.value));
        }
        return res;
    }
    return go;
}

// concat two iterables into a list

function conc(xs, ys) {
    let res = [];
    for (const x of xs) { res.push(x); }
    for (const y of ys) { res.push(y); }
    return res;
}

function cons(x, xs) {
    xs.unshift(x);
    return xs;
}

// --------------------
// save way to add events
// https://stackoverflow.com/questions/641857/javascript-window-resize-event

var addEvent = function(object, type, callback) {
    if (object == null || typeof(object) == 'undefined') return;
    if (object.addEventListener) {
        object.addEventListener(type, callback, false);
    } else if (object.attachEvent) {
        object.attachEvent("on" + type, callback);
    } else {
        object["on"+type] = callback;
    }
};

// --------------------
