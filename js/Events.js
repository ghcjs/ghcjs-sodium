var Events = (function() {
    "use strict";

    function trapEvent(eventName, extractor, useCapture) {
        return function (callback) {
            document.addEventListener(eventName, function(e) {
                var obj = extractor(e);
                callback({
                    target: e.target,
                    eventName: eventName,
                    eventObject: obj
                });
            }, useCapture);
        };
    }

    function trapKeyboardEvent(eventName) {
        return trapEvent(eventName, function (e) {
            return {
                key:      e.keyCode || e.charCode,
                altKey:   e.altKey,
                ctrlKey:  e.ctrlKey,
                metaKey:  e.metaKey,
                shiftKey: e.shiftKey,
                location: e.location,
                locale:   e.locale,
                repeat:   e.repeat
            };
        });
    }

    function trapCapturedEvent(eventName) {
        return trapEvent(eventName, function () {
            return {};
        }, true);
    }

    function trapInputEvent() {
        return trapEvent('input', function (e) {
            var target = e.target;
            var checked = target.checked ? true : false;
            var value = target.value ? target.value : '';

            return {
                checked: checked,
                value:   value
            };
        });
    }

    function trapMouseEvent(eventName) {
        return trapEvent(eventName, function (e) {
            return {
                button:   e.button,
                altKey:   e.altKey,
                ctrlKey:  e.ctrlKey,
                metaKey:  e.metaKey,
                shiftKey: e.shiftKey,
                clientX:  e.clientX,
                clientY:  e.clientY,
                pageX:    e.pageX,
                pageY:    e.pageY,
                screenX:  e.screenX,
                screenY:  e.screenY
            };
        });
    }

    var events = {
        keydown   : trapKeyboardEvent('keydown'),
        keypress  : trapKeyboardEvent('keypress'),
        keyup     : trapKeyboardEvent('keyup'),
        focus     : trapCapturedEvent('focus'),
        blur      : trapCapturedEvent('blur'),
        input     : trapInputEvent(),
        submit    : trapCapturedEvent('submit'),
        mousedown : trapMouseEvent('mousedown'),
        mouseup   : trapMouseEvent('mouseup'),
        click     : trapMouseEvent('click'),
        dblclick  : trapMouseEvent('dblclick'),
        mousemove : trapMouseEvent('mousemove'),
        mouseenter: trapMouseEvent('mouseover'),
        mouseleave: trapMouseEvent('mouseout')
    };

    return {
        listen: function (callback) {
            var eventName;
            for (eventName in events) {
                if (events.hasOwnProperty(eventName)) {
                    events[eventName](callback);
                }
            }
        }
    };

}());
