-- | DOM elements.
module FRP.GHCJS.Element
    ( -- * Elements
      Element
    , Component(..)
    , component
    , text
    ) where

import qualified Data.DList      as DList
import           Data.Text       (Text)

import           FRP.GHCJS.Types

-- | Create a component.
component :: Component -> Element
component = Element . DList.singleton . Parent

-- | Create a text node.
text :: Text -> Element
text = Element . DList.singleton . Text
