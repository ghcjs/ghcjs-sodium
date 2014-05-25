{-# LANGUAGE EmptyDataDecls #-}
module FRP.GHCJS.DOM
    ( Element
    , Event
    ) where

import GHCJS.Types

data Element_
data Event_

-- | A JavaScript DOM node or element.
type Element = JSRef Element_

-- | A JavaScript DOM event.
type Event = JSRef Event_
