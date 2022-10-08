// --------------------
//
// usefull basic functions

// --------------------

function trc (t, Text) {
    if ( t > 0 ) {
        console.log(Text);
    }
}

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
// string functions

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

// simple quoting
// works only if s doen't contain double quotes

function qt(s) {
    return '"' + s + '"';
}

// intercalate a list of strings

function intercalate(s, xs) {
    const l = xs.length;
    if (l === 0) {
        return "";

    } else {
        let res = xs[0];
        for (let i = 1; i < l; i++) {
            res += s + xs[i];
        }
        return res;
    }
}

// --------------------
//
// functions

function id(x) { return x; }

function cnst(c) {
    function f (x) { return c; };
    return f;
}

// function composition
// comp(f1, f2, f3)(v) === f1(f2(f3(v)))

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

// currying
// curry(f)(a1)(a2) = f(a1,a2)

function curry(f) {
    function f1(x1) {
        function f2(x2) {
            return f(x1, x2);
        }
        return f2;
    }
    return f1;
}

function curry1(f) {
    function f1(x1) {
        function f2(...x2) {
            return f(x1, ...x2);
        }
        return f2;
    }
    return f1;
}

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

// --------------------
