var Events = (function() {
    "use strict";

    var isOldIE = !('addEventListener' in document);

    function listenTo(eventName, handler, useCapture) {
        if (isOldIE) {
            document.attachEvent('on' + eventName, handler);
        } else {
            document.addEventListener(eventName, handler, useCapture);
        }
    }

    function trapEvent(eventName, extractor, useCapture) {
        return function (callback) {
            listenTo(eventName, function(e) {
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
    }

    function trapFocusEvent() {
        var eventName = isOldIE ? 'focusin' : 'focus';
        return trapEvent(eventName, function () {
            return {};
        }, true);
    }

    function trapBlurEvent() {
        var eventName = isOldIE ? 'focusout' : 'blur';
        return trapEvent(eventName, function () {
            return {};
        }, true);
    }

    function trapInputEvent() {
        return trapEvent('input', function (e) {
            var target = e.target;
            return {
                checked: target.checked,
                value:   target.value
            };
        });
    }

    function trapSubmitEvent() {
        return trapEvent('submit', function () {
            return {};
        });
    }

    function trapMouseEvent(eventName) {
        return trapEvent(eventName, function (e) {
            var button = e.button;
            if (isOldIE) {
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
    }

    var events = {
        keydown   : trapKeyboardEvent('keydown'),
        keypress  : trapKeyboardEvent('keypress'),
        keyup     : trapKeyboardEvent('keyup'),
        focus     : trapFocusEvent(),
        blur      : trapBlurEvent(),
        input     : trapInputEvent(),
        submit    : trapSubmitEvent(),
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
