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
        scale:    ppScale,
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

    function ppInstr(i) {
        return ppInstr1("", i);
    }

    function ppInstr1(ind, i) {
        const op = i.op;
        let  res = [fillR(10, op)];
        let ind1 = (op !== opInit && op !== opFinish) ? ind + "    " : ind;

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
            res.push(pp.dur(i.dur), i.trans);
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
            if ( isNumber(i.reljno) ) {
                if ( i.reljno !== -1 ) {
                    res.push(pp.num(i.reljno));
                }
            } else {
                res.push(i.name);
            }
            break;

        default:
            res.push('unknown op');
        }
        return nl(ind1 + unwords(res));
    }

    function showCode(is) {
        return (map(ppInstr)(is)).join("");
    }

    return pp;
}

const PP = mkPrettyPrint();

// ----------------------------------------
