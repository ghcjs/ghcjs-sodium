{-# LANGUAGE EmptyDataDecls #-}
module FRP.GHCJS.DOM
    ( DOMElement
    , DOMEvent
    ) where

import GHCJS.Types

data DOMElement_
data DOMEvent_

-- | A JavaScript DOM node or element.
type DOMElement = JSRef DOMElement_

-- | A JavaScript DOM event.
type DOMEvent = JSRef DOMEvent_
