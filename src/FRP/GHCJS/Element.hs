{-# LANGUAGE OverloadedStrings #-}
module FRP.GHCJS.Element
    ( -- * Elements
      Element
      -- * Components
    , Component(..)
    , extend
      -- * Text
    , text
      -- * Grouping content
    , div
    , main
      -- * Text-level semantics
    , span
    ) where

import           Data.Text            (Text)
import           Prelude              hiding (div, span)

import           FRP.GHCJS.Attributes
import           FRP.GHCJS.Types

-- | Extend an element with a 'Component'.
extend :: Component -> Element -> Element
extend = Extend

-- | Create a text node.
text :: Text -> Element
text = Text

-- | Create a tag with attributes.
tag :: Attributes a => Text -> a -> [Element] -> Element
tag name attrs = Extend component . Tag name
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
