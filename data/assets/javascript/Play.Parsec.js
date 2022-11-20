// ----------------------------------------
//
// VM instr parsers

// create interface objects

const PS  = mkParsec();     // Parsec parsers
const PVM = mkVMParser();   // VM code parsers

function mkVMParser() {

    const pNat    = PS.map(toNum, PS.someDigits);
    const pInt    = PS.map(toNum, PS.intOS);
    const pIntS   = PS.map(toNum, PS.intS);
    const pFractN = PS.map(toNum, PS.fractN);
    const pGeo    = PS.app(toV2,
                           PS.fractN,
                           PS.void(PS.char('x')),
                           PS.fractN
                          );                             // 12.3x4.5
    const pOff    = PS.app(toV2, PS.fractS, PS.fractS);  // +1.5-2.0

    const geoSy   = PS.token(pGeo);
    const offSy   = PS.token(pOff);
    const scaleSy = PS.opt(V2(1,1),
                           PS.alt(geoSy,
                                  PS.map(V2, PS.token(pFractN))
                                 )
                          );
    const shiftSy = PS.opt(V2(0,0), offSy);
    const goSy    = PS.token(PS.app(GO, pGeo, PS.opt(V2(0,0), pOff)));

    // duration parser
    const durSy   = PS.map(toNum, PS.token(PS.cxR(PS.fractN, PS.char('s'))));

    // keyword and name parser
    const identSy   = PS.withErr(PS.token(PS.ident),
                                 "identifier expected"
                                );

    const algSy     = PS.opt(resizeDefault, PS.wordTokens(resizeWords));
    const dirSy     = PS.opt(dirDefault,    PS.wordTokens(dirWords));
    const transSy   = PS.opt(trDefault,     PS.wordTokens(trWords));
    const alnSy     = PS.opt(alnDefault,    PS.wordTokens(alnWords));
    const statusSy  = PS.withErr(PS.wordTokens(statiWords),
                                 "status expected"
                                );
    const typeSy    = PS.withErr(PS.wordTokens(tyWords),
                                 "slide type expected"
                                );
    const textSy    = PS.withErr(PS.map((xs) => {return JSON.parse(xs);},
                                        PS.token(PS.textLit)
                                       ),
                                 "text literal expected"
                                );
    const pathSy    = PS.withErr(PS.token(PS.pathLit),
                                 "path expected"
                                );
    const jobSy     = PS.withErr(identSy,
                                 "'prev', 'par' or job name expected"
                                );

    // compound parsers
    const geoSpec        = PS.app(GS, algSy, scaleSy, dirSy, shiftSy);

    const instrInit      = PS.app(mkInit,  PS.wordToken0(opInit),  identSy);
    const instrType      = PS.app(mkType,  PS.wordToken0(opType),  typeSy);
    const instrFrame     = PS.app(mkFrame, PS.wordToken0(opFrame), geoSpec);
    const instrText      = PS.app(mkText,  PS.wordToken0(opText),  alnSy, textSy);
    const instrPath      = PS.app(mkPath,  PS.wordToken0(opPath),  pathSy);

    const instrLoadpage  = PS.map(cnst(mkLoadpage()),  PS.wordToken0(opLoadpage));
    const instrLoadmedia = PS.map(cnst(mkLoadmedia()), PS.wordToken0(opLoadmedia));
    const instrWaitclick = PS.map(cnst(mkWaitclick()), PS.wordToken0(opWaitclick));
    const instrFinish    = PS.map(cnst(mkFinish()),    PS.wordToken0(opFinish));

    const instrRender    = PS.app(mkRender,  PS.wordToken0(opRender), geoSpec);
    const instrPlace     = PS.app(mkPlace,   PS.wordToken0(opPlace),  geoSpec);
    const instrMove      = PS.app(mkMove,    PS.wordToken0(opMove),   durSy, geoSpec);

    const instrFadein    = PS.app(mkFadein,  PS.wordToken0(opFadein),  durSy, transSy, PS.opt('prev', jobSy));
    const instrFadeout   = PS.app(mkFadeout, PS.wordToken0(opFadeout), durSy, transSy);

    const instrWait      = PS.app(mkWait,    PS.wordToken0(opWait), statusSy, jobSy);
    const instrDelay     = PS.app(mkDelay,   PS.wordToken0(opDelay),  durSy);
    const instrStatus    = PS.app(mkStatus,  PS.wordToken0(opStatus), statusSy);

    const instr0   = PS.alts(instrInit,
                             instrType,
                             instrFrame,
                             instrText,
                             instrPath,
                             instrLoadpage,
                             instrLoadmedia,
                             instrRender,
                             instrPlace,
                             instrMove,
                             instrFadein,
                             instrFadeout,
                             instrDelay,
                             instrWaitclick,
                             instrWait,
                             instrStatus,
                             instrFinish,
                            );

    function linep(p) {return PS.cx(PS.manyBlanks, p, PS.seq(PS.cut, PS.newlines));}

    const instr1 = linep(instr0);

    const code   = PS.cxR(PS.many(instr1), PS.eof);

    // howto build a recursive parser in js?
    // e.g. for parsing nested blocks
    //
    //const vmblock = ... vmblock ... doesn't work

    function vmblock(state) {
        return PS.app(VMProg,
                      linep(PS.wordToken0('begin')),
                      PS.many((state1) => {return vmblock(state1);}),
                      PS.many(instr1),
                      linep(PS.wordToken0('end')),
                     )(state);
    }

    const vmprogp = PS.cxR(vmblock, PS.eof);

    function parseProg (inp) {return PS.parseEither(vmprogp, inp);}
    function parseCode (inp) {return PS.parseEither(code,    inp);}

    function parseInstr(inp) {return PS.parse1(instr1,  inp);}
    function parseOff  (inp) {return PS.parse1(PS.cxR(offSy, PS.eof), inp);}

    return {
        vmProg: vmprogp,
        vmCode: code,

        parseProg:  parseProg,
        parseCode:  parseCode,
        parseInstr: parseInstr,
        parseOff:   parseOff,
    };
}

// --------------------
