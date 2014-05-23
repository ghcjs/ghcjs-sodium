-- | HTML attributes and properties.
module FRP.GHCJS.Internal.Attributes
    ( Attributes(..)
    ) where

import qualified GHCJS.DOM.Element             as DOM

import           Data.Default
import           FRP.GHCJS.Events

-- | DOM element attributes and properties.
class (Default a, HasEvents a) => Attributes a where
    -- | Apply a set of attributes to a DOM node.
    applyAttributes :: a -> DOM.Element -> IO ()