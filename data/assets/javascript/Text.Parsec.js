// ----------------------------------------
//
// parsec like module with similar parser combinators

function mkParsec() {

    // parsers defined as const must be declared before used

    // Parser a
    const item = satisfy(cnst(true));

    // Parser a -> Parser ()
    const del = (p) => {
        return fmap(cnst(Void), p);      // discard result, e.g. for separators
    };

    // Parser String -> Parser String
    const manyT = (p)     => { return joinT(many(p)); };
    const someT = (p)     => { return joinT(some(p)); };

    // (Parser String, ...) -> ParserString
    const seqT  = (...ps) => { return joinT(seq(...ps)); };

    const dquote     = charOrd(34);
    const squote     = charOrd(39);
    const bslash     = charOrd(92);

    const ws         = oneOf(' \t\n');
    const blank      = oneOf(' \t');
    const newline    = char('\n');
    const newlines   = someT(newline);

    const lineCmtJS  = lineCmtP('//');
    const lineCmtHS  = lineCmtP('--');

    const noCmt      = failure("no comment");

    const lineSep   = lineSep1(noCmt);
    const lineSepJS = lineSep1(lineCmtJS);
    const lineSepHS = lineSep1(lineCmtHS);

    const textSep   = textSep1(noCmt, noCmt);

    // whitespace parsers

    const ws0        = manyT(ws);
    const ws1        = someT(ws);

    const someBlanks = someT(blank);
    const manyBlanks = manyT(blank);

    // regex parsers

    const ident      = seqT(satisfy(isAlpha), manyT(satisfy(isAlphaNum)));
    const digit      = oneOf("0123456789");
    const manyDigits = manyT(digit);
    const someDigits = someT(digit);
    const sign       = oneOf('-+');
    const optSign    = opt("", sign);
    const intS       = seqT(sign, someDigits);
    const intOS      = seqT(optSign, someDigits);
    const fractN     = seqT(someDigits,
                            opt("", seqT(char('.'), someDigits))
                           );
    const fractS     = seqT(sign, fractN);
    const fract      = seqT(optSign, fractN);

    const textLit    = seqT(dquote,
                            pCut,
                            manyT(alt(noneOfOrd(34, 10),
                                      seqT(bslash, item)
                                     )
                                 ),
                            pCut,
                            dquote,
                           );
    const pathLit    = someT(seqT(char('/'),
                                  pCut,
                                  someT(alt(satisfy(isAlphaNum),
                                            oneOf('+-._')
                                           )
                                       )
                                 )
                            );


    // --------------------
    // all exported parsers

    const ps = {
        // main entry points
        parse:       parse,
        parse1:      parse1,
        parseEither: parseEither,

        // char parser
        item:    item,
        satisfy: satisfy,
        char:    char,
        oneOf:   oneOf,
        noneOf:  noneOf,

        // basic parsers
        unit: unit,
        cut:  pCut,
        fail: failure,
        withErr: withErr,
        eof: eof,
        void: del,

        // compound parsers
        followedBy: followedBy,
        notFollowedBy: notFollowedBy,
        map: fmap,
        app: app,
        bind: bind,
        seq: seq,
        alt: alt,   // binary alternative
        alts: alts, // n-ary alternative
        opt: opt,
        many: many,
        some: some,

        // context parsers
        cxR: cxR,
        cxL: cxL,
        cx:  cx,

        // text parsers
        manyT: manyT,
        someT: someT,
        seqT: seqT,
        word: word,
        words: words,

        // charCode parsers (don't mess up with quoting)
        charOrd: charOrd,
        oneOfOrd: oneOfOrd,
        noneOfOrd: noneOfOrd,
        wordOrd: wordOrd,

        // whitespace and comment parsers, text literal parsers
        dquote:     dquote,
        squote:     squote,
        bslash:     bslash,
        ws:         ws,
        ws0:        ws0,
        ws1:        ws1,
        blank:      blank,
        someBlanks: someBlanks,
        manyBlanks: manyBlanks,
        newline:    newline,
        newlines:   newlines,

        lineCmtJS: lineCmtJS,
        lineCmtHS: lineCmtHS,

        lineSep1:  lineSep1,
        lineSep:   lineSep,
        lineSepJS: lineSepJS,
        lineSepHS: lineSepHS,

        textSep1:  textSep1,
        textSep:   textSep,

        // token parsers, (parsers for token that consume trailing whitespace)

        // this parser variable is used in other token parsers
        // to consume trailing whitespace,
        // it can be configures by storing another token parser in this field
        token:      tokenCL,

        tokenL:     tokenL,
        tokenLJS:   tokenLJS,
        tokenT:     tokenT,
        tokenC:     tokenC,
        tokenCL:    tokenCL,

        wordToken:  wordToken,
        wordToken0: wordToken0,
        wordTokens: wordTokens,

        // regex parsers
        ident:      ident,
        digit:      manyDigits,
        someDigits: someDigits,
        sign:       sign,
        optSign:    optSign,
        intS:       intS,    // ints with sign
        intOS:      intOS,   // ints with optional sign
        fractN:     fractN,  // fractional without sign
        fractS:     fractS,  //      "     with     "
        fract:      fract,   //      "     with optional sign
        textLit:    textLit, // text literal in double qoutes
        pathLit:    pathLit, // abs dir path
    };

    function initParserState(inp) {
        let s0 = {
            inp : inp,
            ix  : 0,
            cc  : 0,
            lc  : 0,
            backtrack: false,
        };

        // save the variable parts of the parser state
        function saveState() {
            return [s0.ix, s0.cc, s0.lc, s0.backtrack];
        }

        // restore saved parser state
        // for backtracking in alt and followedBy parsers
        function resetState(cs) {
            s0.ix = cs[0];
            s0.cc = cs[1];
            s0.lc = cs[2];
            s0.backtrack = cs[3];
        }

        function resetBacktrack(cs) {
            s0.backtrack = cs[3];
        }
        // prepare error message with source line and position
        function showErr(msg) {
            const ls  = lines(s0.inp);
            const lc1 = s0.lc;
            const lc0 = Math.max(0, lc1 - 3);
            let   ls1 = take(lc1 - lc0 + 1, drop(lc0, ls));
            const l2  = replicate(s0.cc, " ") + "^^^^^";
            const l3  = `line ${s0.lc + 1}: ${msg}`;
            ls1.push(l2,l3,"");
            trc(1, unlines(ls1));
        }

        function insErr(msg) {
            const ls  = lines(s0.inp);
            const l2  = replicate(s0.cc, " ") + "^^^^^";
            const l3  = `error: ${msg}`;
            ls.splice(s0.lc + 1, 0, l2, l3);
            return unlines(ls);
        }

        function rest() {
            return s0.inp.slice(s0.ix);
        }

        s0.save           = saveState;
        s0.reset          = resetState;
        s0.resetBacktrack = resetBacktrack;
        s0.errmsg         = showErr;
        s0.insertErrmsg   = insErr;
        s0.rest           = rest;
        return s0;
    }

    // object used as module

    function parse(p1, inp) {
        const s0 = initParserState(inp);
        trc(1,JSON.stringify(s0));

        const res = p1(s0);
        trc(1,JSON.stringify(res));

        return res;
    }

    function parseEither(p1, inp) {
        const res = parse(p1, inp);

        if ( res.err ) {
            return Left(res.state.insertErrmsg(res.err));
        }
        return Right(res.res);
    }

    function parse1(p1, inp) {
        const res = parse(p1, inp);

        if ( res.err ) {
            const msg = res.state.errmsg(res.err);
            trc(1, msg);
            return "";
        }
        return res.res;
    }

    function succ(res, state) { return {res: res, state: state}; };
    function fail(err, state) { return {err: err, state: state}; };
    //                                            ^^^^^^^^^^^^
    //                                            for better error reporting

    // (a -> Bool) -> Parser a
    function satisfy(pred) {
        return (state) => {
            const c = state.inp[state.ix];
            if ( c ) {
                if ( pred(c) ) {
                    state.ix++;
                    if ( c === "\n" ) {
                        state.lc += 1;
                        state.cc  = 0;
                    }
                    else {
                        state.cc += 1;
                    }
                    return succ(c,state);
                }
                else {
                    return fail(`unexpected char '${c}'`, state);
                }
            }
            else {
                return fail("end of input", state);
            }
        };
    }

    // Char -> Parser String
    function char(c) {
        return satisfy((c1) => { return (c === c1)});
    }

    // Set Char -> Parser Char
    function oneOf(s) {
        return satisfy((c1) => {
            return s.indexOf(c1) >= 0;
        });
    }

    // Set Char -> Parser Char
    function noneOf(s) {
        return satisfy((c1) => {
            return s.indexOf(c1) < 0;
        });
    }

    // a -> Parser a
    function unit(res) {
        return (state) => { return succ(res, state); };
    }

    // mark state to not backtrack in an alt parser
    // but fail instead
    //
    // Parser ()
    function pCut(state) {
        state.backtrack = false;
        return succ(Void, state);
    }

    // String -> Parser a
    function failure(msg) {
        return (state) => { return fail(msg, state); };
    }

    function withErr(p, msg) {
        return (state) => {
            const res = p(state);
            if ( res.err ) {
                res.err = msg;
            }
            return res;
        };
    }

    // Parser ()
    function eof(state) {
        // trc(1,`eof: ${state.inp} ${state.ix}`);
        const c = state.inp[state.ix];
        if ( ! c ) {
            return succ(Void, state);
        }
        else {
            const rest = state.inp.slice(state.ix, state.ix + 40);
            return fail(`end of input expected, but seen: ${line1(rest)}`, state);
        }
    }

    // Parser String -> Parser ()        // Void = ()
    function followedBy(p) {
        function go(state) {
            const s1 = state.save();
            const r1 = p(state);
            state.reset(s1);
            if ( r1.err ) {
                return fail("followed context not matched", state);
            }
            else {
                return succ(Void, state);
            }
        }
        return go;
    }

    // Parser String -> Parser ()
    function notFollowedBy(p) {
        return alt(seqT(followedBy(p),
                        failure("wrong context followed")
                       ),
                   unit(Void)
                  );
    }

    // (a -> b) -> Parser a -> Parser b
    function fmap(f, p) {
        return (st0) => {
            const r1 = p(st0);
            // trc2('fmap: r1=', r1);
            if ( r1.err ) {
                return r1;
            }
            else {
                r1.res = f(r1.res);
                // trc2("fmap: rs=", r1);
                return r1;
            }
        };
    }

    // generalisation of <$> and <*>
    // f <$> Parser a <*> Parser b <*> ...
    //
    // ((a, b, ...) -> r) -> Parser a -> Parser b -> ... -> Parser r
    function app(f, ...ps) {
        function f1(xs) {
            if ( xs === Void ) {
                return xs;
            }
            return f(...xs);
        }
        return fmap(f1, seq(...ps));
    }

    // Parser a -> Parser b -> Parser a                // <*  from Applicative
    function cxR(p, cx) {
        return app(id, p, del(cx));                  // discard right context
    }

    // Parser b -> Parser a -> Parser a                // *>  from Applicative
    function cxL(cx, p) {
        return app(id, del(cx), p);                  // discard left context
    }

    // Parser b -> Parser a -> Parser c -> Parser a    // *>  <* from Applicative
    function cx(cx1, p, cx2) {
        return app(id, del(cx1), p, del(cx2));     // discard context
    }

    // Parser a -> (a -> Parser b) -> Parser b    // >>=
    function bind(p1, f) {
        return (state) => {
            const r1 = p1(state);
            if ( r1.err ) {
                return r1;
            }
            else {
                const p2 = f(r1.res);
                return p2(r1.state);
            }
        };
    }

    // [Parser a]                           -> Parser [a]
    // (Parser a, Parser b, ...)            -> Parser (a, b, ...)
    // (Parser a, Parser (), Parser b, ...) -> Parser (a, b, ...)

    function seq(...ps) {
        function go(state) {
            let   st = state;
            let   xs = [];
            const l  = ps.length;
            if ( l === 0 ) {
                return succ(xs, state);
            }
            for (let i = 0; i < ps.length; i++) {
                const rs = ps[i](st);
                if ( rs.err )
                    return rs;
                if ( rs.res !== Void ) {
                    xs.push(rs.res);
                }
                // trc(1,`seq: ${JSON.stringify(rs.res)} ${JSON.stringify(xs)}`);
                st = rs.state;
            }
            return succ(xs.length === 0 ? Void : xs, st);
        }
        return go;
    }

    // Parser a -> Parser a -> Parser a
    function alt(p1, p2) {
        return (state) => {
            const s1 = state.save();

            // allow backtracking, as long as not cut parser is reached
            state.backtrack = true;
            // trc2("alt: state=", state);

            const r1 = p1(state);
            // trc2("alt: r1=", r1);

            if ( r1.err ) {                     // p1 failed
                if ( r1.state.backtrack ) {     // backtracking allowed
                    state.reset(s1);            // reset state and
                    const r2 = p2(state);       // run p2
                    // trc2("alt: r2=", r2);
                    return r2;
                }
                else {                          // no backtracking
                    // r1.state.resetBacktrack(s1);// restore backtracking flag
                    trc(1, "alt: no backtracking");
                    return r1;                  // and report error
                }
            }
            else {                              // p1 succeded
                r1.state.resetBacktrack(s1);    // restore backtracking flag
                return r1;                      // and return result
            }
        };
    }

    // (Parser a, ...) -> Parser a
    function alts(...ps) {
        let res = failure("no alternatve matched");
        for (let i = ps.length; i > 0; i--) {
            res = alt(ps[i-1], res);
        }
        return res;
    }

    // a -> Parser a -> Parser a
    function opt(defVal, p) {
        return alt(p, unit(defVal));
    }

    // many and some parsers always return a list
    // if parser p is of type 'Parser ()', the result
    // will be always the empty list

    // Parser a  -> Parser [a]
    // Parser () -> Parser []  (res: empty list)

    function many(p) {
        function go(state) {
            let   st = state;
            let   xs = [];                       // result accumulator

            while ( true ) {
                const s1 = st.save();
                st.backtrack = true;
                let rs = p(st);

                if ( rs.err ) {
                    if ( rs.state.backtrack ) {  // terminate loop
                        st.reset(s1);            // reset state
                        const res = succ(xs, st);
                        // trc2('many: res:', res);
                        return res;              // and return xs
                    }
                    else {                       // error when running p
                        // trc2("many: failed:", rs);
                        return rs;               // propagate error
                    }
                }
                // one more item parsed
                if ( rs.res !== Void ) {
                    xs.push(rs.res);             // update xs
                }
                st = rs.state;                   // update st
            }
        }
        return go;
    }

    // Parser a  -> Parser [a]
    // Parser () -> Parser []  (res: empty list)
    function some(p) {
        function cons1(...args) {
            // trc(1,`cons1: ${JSON.stringify(args)}`);
            if ( args.length === 1 ) {
                return id(...args);           // return args[0];
            }
            if ( args.length === 2 ) {
                return cons(...args);
            }
            throw "some: cons1: wrong args";
        }
        return app(cons1, p, many(p));
    }

    // Parser [String] -> Parser String
    function joinT(p) {
        return fmap((xs)  => { return concatS(...xs); }, p);
    }

    // String -> Parser String
    function word(w) {
        const ps = map((c) => {return char(c);})(w);
        return seqT(...ps);
    }

    // CAUTION: if a word w1 is prefix of a word w2
    // w1 must be put into the list behind w2
    // else tokenising does not word properly
    // right: ['fixandfoxi, fix']
    // wrong: ['fix', 'fixandfoxi']
    // see 'wordToken'

    function words(ws) {
        const ps = map(word)(ws);
        return alts(...ps);
    }

    // for not mess up with quoting

    function charOrd(n) {
        return char(fromOrd(n));
    }

    function oneOfOrd(...ns) {
        return oneOf(fromOrd(...ns));
    };

    function noneOfOrd(...ns) {
        return noneOf(fromOrd(...ns));
    };

    function wordOrd(...ns) {
        return word(fromOrd(...ns));
    }

    // --------------------
    //
    // whitespace and comment separators


    // whitespace parsing for line oriented parsers
    function lineSep1(cmt) {
        return alts(some(del(alts(blank, cmt))),
                    followedBy(newline),
                    eof
                   );
    };

    function textSep1(lcmt, tcmt) {
        return alts(some(del(alts(ws,
                                  newline,
                                  lcmt,
                                  tcmt
                                 )
                            )
                        ),
                    eof
                   );
    };

    // String -> Parser ()
    function lineCmtP(w) {
        return seq(word(w), manyT(noneOf('\n')));
    };

    // --------------------
    //
    // token parsers

    // Parser String

    // token parsers discard trailing whitespace
    // and stop backtracking, due to successfully parse a token
    //
    // tokenBL discard whitespace only on current line
    // tokenWS discards whitespace and newlines

    // Parser a -> Parser a

    function tokenL  (p) { return cxR(p, lineSep);   };  // space and tabs
    function tokenLJS(p) { return cxR(p, lineSepJS); };  // space, tabs, //...
    function tokenT  (p) { return cxR(p, textSep);   };    // space, tab, nl

    // if token parser succeds,
    // backtracking is disabled
    function tokenC  (p) { return cxR(p, pCut);      };
    function tokenCL (p) { return tokenC(tokenL(p)); };

    // the seq of words does not matter with this parser
    // even if a word w1 is prefix of a word w2, and
    // w2 occurs behind w1 the parser works

    function wordTokens(ws) {
        const ps = map(wordToken)(ws);
        return alts(...ps);
    }

    // String -> Parser ()
    function wordToken(w) {
        return ps.token(word(w));
    }

    function wordToken0(w) {
        return del(wordToken(w));
    }

    return ps;
}

// ----------------------------------------
