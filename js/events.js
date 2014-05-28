var Events = (function() {

    function trapEvent(mountAt, eventName, callback) {
        mountAt.addEventListener(eventName, function(e) {
            var obj = extractEvent(eventName, e);
            callback(e.target, obj);
        });
    }

    function extractEvent(eventName, event) {
        if (eventName === 'click') {

            var button = event.button;
            if (!('which' in event)) {
                // IE<9
                button = button === 2 ? 2 : button === 4 ? 1 : 0;
            }

            var pageX = 'pageX' in event ?
                event.pageX :
                event.clientX + document.body.scrollLeft +
                    document.documentElement.scrollLeft;

            var pageY = 'pageY' in event ?
                event.pageY :
                event.clientY + document.body.scrollTop +
                    document.documentElement.scrollTop;

            return {
                type:     eventName,
                button:   button,
                altKey:   event.altKey,
                ctrlKey:  event.ctrlKey,
                metaKey:  event.metaKey,
                shiftKey: event.shiftKey,
                clientX:  event.clientX,
                clientX:  event.clientX,
                pageX:    pageX,
                pageY:    pageY,
                screenX:  event.screenX,
                screenX:  event.screenY
            };
        }
    }

    return {
        listen: function (mountAt, callback) {
            trapEvent(mountAt, 'click', callback)
        }
    };

}());
