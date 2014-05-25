-- | HTML attributes and properties.
module FRP.GHCJS.Internal.Attributes
    ( Attributes(..)
    ) where

import qualified FRP.GHCJS.DOM as DOM
import           Data.Default

-- | DOM element attributes and properties.
class Default a => Attributes a where
    -- | Apply a set of attributes to a DOM node.
    applyAttributes :: a -> DOM.Element -> IO ()
