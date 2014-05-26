-- | HTML attributes and properties.
module FRP.GHCJS.Internal.Attributes
    ( Attributes(..)
    ) where

import           FRP.GHCJS.DOM
import           Data.Default

-- | DOM element attributes and properties.
class Default a => Attributes a where
    -- | Apply a set of attributes to a DOM node.
    applyAttributes :: a -> DOMElement -> IO ()
