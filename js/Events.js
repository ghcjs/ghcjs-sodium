var Events = (function() {

    function trapEvent(eventName, extractor, callback) {
        document.addEventListener(eventName, function(e) {
            var obj = extractor(eventName, e);
            callback({
                target: e.target,
                eventName: eventName,
                eventObject: obj
            });
        });
    }

    function extractMouseEvent(eventName, event) {
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

    return {
        listen: function (callback) {
            trapEvent('click'    , extractMouseEvent, callback);
            trapEvent('dblclick' , extractMouseEvent, callback);
            trapEvent('mousedown', extractMouseEvent, callback);
            trapEvent('mouseup'  , extractMouseEvent, callback);
        }
    };

}());
