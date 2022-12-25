// --------------------
//
// geo spec edit panel

function mkEditGeoPanel(editId) {

    const editGeoDisId  = 'edit-geo-cancel';
    const alignButtonId = 'align';
    const scaleButtonId = 'scale';
    const shiftButtonId = 'shift';
    const algButtonId   = 'radio';
    const algPanelId    = 'edit-alg-panel';
    const textAlignBId  = 'text-align';
    const textAlignId   = 'text-align-panel';
    const textEditBId   = 'text-edit';

    function initEditGeoHandlers() {
        setDismissEH();
        setAlignEH();
        setScaleEH();
        setShiftEH();
        setAlgEH();
        setEditTextEH();
        setAlignTextEH();
    }

    function setDefaultChecked(alg) {
        // first remove checked attribute(s)
        for (let i = 1; i <= 5; i++) {
            const ide  = algButtonId + (-i);
            getElem(ide).removeAttribute('checked');
        }

        // then set the right checked attribute
        //
        // doing this in one loop showed wrong or no defaults

        for (let i = 1; i <= 5; i++) {
            const ide  = algButtonId + (-i);
            const e    = getElem(ide);
            const alg1 = e.getAttribute('data-alg');
            //trc(1,`setChecked: id=${ide} alg=${alg} data-alg=${alg1}`);
            if ( alg === alg1 ) {
                e.setAttribute('checked', 'checked');
            }
        }

    }

    function setDismissEH() {
        addEvent(getElem(editGeoDisId), 'click', hideEditGeo);

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
    function setEditTextEH() {
        addEvent(getElem(textEditBId), 'click', editTextInstr);
    }
    function setAlignTextEH() {
        for (let i = 1; i <= 3; i++) {
            const ide = textAlignBId + (-i);
            addEvent(getElem(ide), 'click', editTextAln);
        }
    }

    function editTextAln(ev) {
        const aln = ev.target.getAttribute('data-text-align');
        const df  = alnText(aln);
        trc(1, `editTextAln: aln=${aln}`);
        editTextAlign(df);
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
        let   sh = PVM.parseOff(st);    // offset parser from Play.Parsec
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

    function configEditGeo() {
        const ty = vm.getLastType();
        const gs = vm.getLastGS();

        switch ( ty ) {
        case 'text':
            setDisplay(textAlignId, true);
            setDisplay(algPanelId,  false);
            return ;

        case 'img':
        default:
            setDisplay(textAlignId, false);
            setDisplay(algPanelId,  true);
            setDefaultChecked(gs.alg);
            return;
        }
    }

    function showEditGeo() {
        configEditGeo();
        setDisplay(editId, true);
    }

    function hideEditGeo() {
        setDisplay(editId, false);
    }

    return { id:             editId,
             show:           showEditGeo,
             hide:           hideEditGeo,
             defaultchecked: setDefaultChecked,
             initHandlers:   initEditGeoHandlers,
           };
}

// --------------------
