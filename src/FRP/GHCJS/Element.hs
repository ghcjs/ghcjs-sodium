{-# LANGUAGE OverloadedStrings #-}
module FRP.GHCJS.Element
    ( -- * Text
      Element
    , text
      -- * Grouping content
    , div
    , main
      -- * Text-level semantics
    , span
    ) where

import           Control.Lens
import           Data.Text                     (Text)
import           Prelude                       hiding (div, span)

import           FRP.GHCJS.Attributes
import           FRP.GHCJS.Events
import           FRP.GHCJS.Internal.Attributes
import           FRP.GHCJS.Internal.Element

-- | Create a text node.
text :: Text -> Element
text = Text

-- | Create a tag with attributes.
tag :: (Attributes a, HasEvents a) => Text -> a -> [Element] -> Element
tag name attrs = Extend component . Tag name (attrs ^. events)
  where
    component = Component
        { componentName = name
        , create        = applyAttributes attrs
        , update        = applyAttributes attrs
        , destroy       = \_ -> return ()
        }

-- | The HTML @div@ element.
div :: GlobalAttributes -> [Element] -> Element
div = tag "div"

-- | The HTML @main@ element.
main :: GlobalAttributes -> [Element] -> Element
main = tag "main"

-- | The HTML @span@ element.
span :: GlobalAttributes -> [Element] -> Element
span = tag "span"
