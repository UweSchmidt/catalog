// --------------------
//
// geo spec edit panel

function mkEditGeoPanel(editId) {

    const alignButtonId = 'align';
    const scaleButtonId = 'scale';
    const shiftButtonId = 'shift';
    const algButtonId   = 'radio';

    function initEditGeoHandlers() {
        setAlignEH();
        setScaleEH();
        setShiftEH();
        setAlgEH();
    }

    function setAlgEH() {
        for (let i = 1; i <= 5; i++) {
            const ide = algButtonId + (-i);
            addEvent(getElem(ide), 'click', editGSAlg);
        }
    }
    function setAlignEH() {
        for (let i = 1; i <= 9; i++) {
            const ide = alignButtonId + (-i);
            addEvent(getElem(ide), 'click', editGSDir);
        }
    }
    function setScaleEH() {
        for (let i = 1; i <= 3; i++) {
            const ide = scaleButtonId + (-i);
            addEvent(getElem(ide), 'click', editGSScale);
        }
    }
    function setShiftEH() {
        for (let i = 1; i <= 9; i++) {
            const ide = shiftButtonId + (-i);
            addEvent(getElem(ide), 'click', editGSShift);
        }
    }

    function editGSAlg(ev) {
        const alg = ev.target.getAttribute('data-alg');
        const df  = algGS(alg);
        trc(1, `editGSAlg: alg=${alg}`);
        editGS(df);
    }
    function editGSDir(ev) {
        const dir = ev.target.getAttribute('data-dir');
        const df  = dirGS(dir);
        trc(1, `editGSDir: dir=${dir}`);
        editGS(df);
    }
    function editGSScale(ev) {
        let   sc = toNum(ev.target.getAttribute('data-scale'));
        let   sf = scaleGS1;
        if ( sc !== 0 ) {
            if ( ! ev.altKey ) {
                sc = 10 * sc;
            }
            sc = (100 + sc)/100;
            sf = scaleGS(V2(sc));
        }
        trc(1, `editGSScale: sc=${sc}`);
        editGS(sf);
    }
    function editGSShift(ev) {
        const st = ev.target.getAttribute('data-shift');
        let   sh = parse1(offSy, st);
        let   sf = shiftGS0;
        if ( ! nullV2(sh) ) {
            if ( ! ev.altKey ) {          // alt key => small steps
                sh = mulV2(sh, 10);
            }
            sh = mulV2(sh, 0.75);
            sh = divV2(sh, 100);          // 7.5% (or 0.75%) shift step
            sf = shiftGS(sh);
        }
        trc(1, `editGSShift: sh=${showGeo(sh)}`);
        editGS(sf);
    }

    function showEditGeo() {
        const e = getElem(editId);
        setCSS(e, 'display', 'block');
    }
    function hideEditGeo() {
        const e = getElem(editId);
        setCSS(e, 'display', 'none');
    }

    return { id:           editId,
             show:         showEditGeo,
             hide:         hideEditGeo,
             initHandlers: initEditGeoHandlers,
           };
}

// --------------------