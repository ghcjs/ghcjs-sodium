-- | DOM elements.
module FRP.GHCJS.Element
    ( -- * Elements
      Element
    , text
    ) where

import qualified Data.DList      as DList
import           Data.Text       (Text)

import           FRP.GHCJS.Types

-- | Create a text node.
text :: Text -> Element
text = Element . DList.singleton . Text
