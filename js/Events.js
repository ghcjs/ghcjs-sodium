var Events = (function() {

    function trapEvent(extractor) {
        return function (eventName, callback) {
            document.addEventListener(eventName, function(e) {
                var obj = extractor(e);
                callback({
                    target: e.target,
                    eventName: eventName,
                    eventObject: obj
                });
            });
        };
    }

    var trapKeyboardEvent = trapEvent(function (event) {
        var key = event.keyCode || event.charCode;
        return {
            key:      key,
            altKey:   event.altKey,
            ctrlKey:  event.ctrlKey,
            metaKey:  event.metaKey,
            shiftKey: event.shiftKey,
            location: event.location,
            locale:   event.locale,
            repeat:   event.repeat
        };
    });

    var trapInputEvent = trapEvent(function (event) {
        var target = event.target;
        return {
            checked: target.checked,
            value:   target.value
        };
    });

    var trapChangeEvent = trapEvent(function (event) {
        var target = event.target;
        var eventObj = {
            checked: target.checked,
            value:   target.value
        };

        var prevChecked = target.getAttribute('checked') ? true : false;
        var prevValue   = target.getAttribute('value');
        if (prevValue === null) {
            prevValue = '';
        }

        target.checked = prevChecked;
        if (target.value !== prevValue) {
            target.value = prevValue;
        }

        return eventObj;
    });

    var trapMouseEvent = trapEvent(function (event) {
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
            clientY:  event.clientY,
            pageX:    pageX,
            pageY:    pageY,
            screenX:  event.screenX,
            screenY:  event.screenY
        };
    });

    var events = {
        keydown   : trapKeyboardEvent,
        keypress  : trapKeyboardEvent,
        keyup     : trapKeyboardEvent,
        input     : trapInputEvent,
        change    : trapChangeEvent,
        click     : trapMouseEvent,
        dblclick  : trapMouseEvent,
        mousedown : trapMouseEvent,
        mouseup   : trapMouseEvent
    }

    return {
        listen: function (callback) {
            for (eventName in events) {
                var trapper = events[eventName];
                trapper(eventName, callback);
            }
        }
    };

}());