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

    var trapKeyboardEvent = trapEvent(function (e) {
        var key = e.keyCode || e.charCode;
        return {
            key:      key,
            altKey:   e.altKey,
            ctrlKey:  e.ctrlKey,
            metaKey:  e.metaKey,
            shiftKey: e.shiftKey,
            location: e.location,
            locale:   e.locale,
            repeat:   e.repeat
        };
    });

    var trapInputEvent = trapEvent(function (e) {
        var target = e.target;
        return {
            checked: target.checked,
            value:   target.value
        };
    });

    var trapChangeEvent = trapEvent(function (e) {
        var target = e.target;
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

    var trapSubmitEvent = trapEvent(function (e) {
        return {};
    });

    var trapMouseEvent = trapEvent(function (e) {
        var button = e.button;
        if (!('which' in e)) {
            // IE<9
            button = button === 2 ? 2 : button === 4 ? 1 : 0;
        }

        var pageX = 'pageX' in e ?
            e.pageX :
            e.clientX + document.body.scrollLeft +
                document.documentElement.scrollLeft;

        var pageY = 'pageY' in e ?
            e.pageY :
            e.clientY + document.body.scrollTop +
                document.documentElement.scrollTop;

        return {
            button:   button,
            altKey:   e.altKey,
            ctrlKey:  e.ctrlKey,
            metaKey:  e.metaKey,
            shiftKey: e.shiftKey,
            clientX:  e.clientX,
            clientY:  e.clientY,
            pageX:    pageX,
            pageY:    pageY,
            screenX:  e.screenX,
            screenY:  e.screenY
        };
    });

    var events = {
        keydown   : trapKeyboardEvent,
        keypress  : trapKeyboardEvent,
        keyup     : trapKeyboardEvent,
        input     : trapInputEvent,
        change    : trapChangeEvent,
        submit    : trapSubmitEvent,
        mousedown : trapMouseEvent,
        mouseup   : trapMouseEvent,
        click     : trapMouseEvent,
        dblclick  : trapMouseEvent,
        mousemove : trapMouseEvent
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
