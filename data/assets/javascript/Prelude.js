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

function div(x, y) {
    return Math.floor(x / y);
}

function mod(x, y) {
    return Math.floor(x % y);
}

function odd(x) {
    return mod(x, 2) === 1;
}

function even(x) {
    return mod(x, 2) === 0;
}

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
    let f = (x) => { return c; };
    return f;
}

// --------------------
//
// list functions

// process all elements of an iterable and
// build a list of all result values

function map(f, xs) {
    let res = [];
    for (const x of xs) {
        res.push(f(x));
    }
    return res;
}

function zipWith(f, xs, ys0) {
    const ys = map(id, ys0);
    let res = [];
    for (const x of xs) {
        if ( ys.length === 0 ) break;
        const y = ys.shift();
        res.push(f(x, y));
    }
    return res;
}

function conc(xs, ys) {
    return map(id, xs).concat(map(id,ys));
}

// --------------------
