// ----------------------------------------
// status bar

// import DOM.Manipulate
// import DOM.Animation

function mkStatusBar(id) {
    const state = {
        id      : id,
        enabled : true,
        timer   : undefined,
        dur     : 2.0 * 1000 // 2 seconds
    };
    function show(msg, dur) {
        if ( state.enabled ) {
            dur = dur || 1;
            dur = state.dur * dur;
            state.hide();
            const s = getElem(state.id);
            s.innerHTML = msg;
            state.timer = setTimeout(state.hide, dur);
            showAnim(state.id, 'info');

        }
    }
    function hide() {
        if ( status.timer != undefined ) {
            clearTimeout(state.timer);
        }
        hideAnim(state.id, 'info');
    }
    state.show = show;
    state.hide = hide;
    return state;
}

// ----------------------------------------
