// --------------------
//
// text/code edit panel

function mkEditTextPanel(editId) {
    const editOKId     = editId + "-ok";
    const editCancelId = editId + "-cancel";
    const editHeadId   = editId + "-headline";
    const editAreaId   = editId + "-area";

    const edt = { id:           editId,
                  idArea:       editAreaId,
                  show:         showEditText,
                  hide:         hideEditText,
                  getText:      getEditText,
                  setText:      setEditText,
                  setCallback:  setCallback,
                  initHandlers: initEditTextHandlers,
                  edit:         editText,
                  callback:     dummyCallback,
                };

    function initEditTextHandlers() {
        addEvent(getElem(editOKId),     'click',    getEditText);
        addEvent(getElem(editCancelId), 'click', cancelEditText);
    }
    function cancelEditText() {
        edt.hide();
    }
    function getEditText() {
        const e = getElem(editAreaId);
        edt.hide();
        edt.callback(e.value);
    }
    function dummyCallback(txt) {
        trc(1, `editText: res=${txt}`);
    }
    function setCallback(f) {
        edt.callback = f;
    }
    function setEditText(txt) {
        const e = getElem(editAreaId);
        e.value = txt;
    }
    function showEditText() {
        const e = getElem(editId);
        setCSS(e, 'display', 'block');
    }
    function hideEditText() {
        const e = getElem(editId);
        setCSS(e, 'display', 'none');
    }
    function editText(txt, process) {
        edt.setText(txt);
        process && edt.setCallback(process);
        edt.show();
    }

    return edt;
}
