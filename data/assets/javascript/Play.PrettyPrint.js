// ----------------------------------------
//
// pretty printer for instructions

function mkPrettyPrint() {
    const pp = {
        align:    ppAlign,
        clickdur: ppSimple,
        code:     showCode,
        dir:      ppSimple,
        dur:      ppDur,
        geo:      showGeo,
        go:       showGO,
        gs:       ppGS,
        instr:    ppInstr,
        num:      ppSimple,
        off:      showOff,
        path:     ppPath,
        pathpx:   ppPathPrefix,
        prog:     ppProg,
        scale:    ppScale,
        status:   ppStatusSet,
        text:     ppSimple,
    };

    function ppSimple(x) { return "" + x; }
    function ppDur(x)    { return "" + x + "s"; }

    function ppPath(l) {
        return l[0] + (l[1] ? ppSimple(l[1]) : "");
    }

    function ppPathPrefix(p) { return rePathPx + p; }

    function ppGS(gs) {
        return filter((x) => {return x !== "";})([
            pp.text(gs.alg),
            pp.scale(gs.scale),
            (gs.dir === dirDefault) ? "" : pp.text(gs.dir),
            (nullV2(gs.shift) ? "" : pp.off(gs.shift))
        ]).join(" ");
    }

    function ppAlign(aln) {
        if ( aln === alnDefault ) {
            return "";
        }
        return aln;
    }

    function ppScale(sc) {
        if ( eqV2(sc, V2(1,1)) )
            return "";
        if ( sc.x === sc.y)
            return pp.num(sc.x);
        return pp.geo(sc);
    }

    function ppStatusSet(ss) {
        let del = '{';
        let res = '';
        for (let e of ss) {
            res += del + e;
            del = ',';
        }
        return res + '}';
    }

    function ppInstr(i) {
        return ppInstr1("", i);
    }

    function ppInstr1(ind, i) {
        const op = i.op;
        let  res = [fillR(10, op)];

        switch ( op ) {
        case opLoadpage:
        case opLoadmedia:
        case opWaitclick:
        case opFinish:
            break;

        case opInit:
            res.push(i.name);
            break;

        case opType:
            res.push(i.type);
            break;

        case opFrame:
            res.push(pp.gs(i.gs));
            break;

        case opText:
            res.push(pp.align(i.align), JSON.stringify(i.text));
            break;

        case opPath:
            res.push(i.path);
            break;

        case opRender:
        case opPlace:
            res.push(pp.gs(i.gs));
            break;

        case opFadein:
        case opFadeout:
            res.push(pp.dur(i.dur), i.trans, i.job);
            break;

        case opStatus:
            res.push(i.st);
            break;

        case opMove:
            res.push(pp.dur(i.dur));
            res.push(pp.gs(i.gs));
            break;

        case opDelay:
            res.push(pp.dur(i.dur));
            break;

        case opWait:
            res.push(i.status);
            res.push(ppSimple(i.job));
            break;

        default:
            res.push('unknown op');
        }
        return nl(ind + unwords(res));
    }

    function showCode(is) {
        return (map(ppInstr)(is)).join("");
    }

    function ppProg(prog0) {
        function go(prog, ind) {
            const ind1  = ind + '  ';
            return [
                nl(ind + 'begin'),
                map((p) => {return go(p, ind1);})(prog.blocks).join(''),
                map((i) => {return ppInstr1(ind1, i);})(prog.code).join(''),
                nl(ind + 'end'),
                nl(''),
            ].join('');
        }
        return go(prog0, '');
    }

    return pp;
}

const PP = mkPrettyPrint();

// ----------------------------------------
