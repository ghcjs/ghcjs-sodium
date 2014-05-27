{-# LANGUAGE OverloadedStrings #-}
module Alder.Html
    ( -- * Text
      Element
    , text
    , tag
    , div
    , span
    ) where

import           Prelude               hiding (div, span)

import           Data.Monoid
import           Data.Text             (Text)

import qualified Alder.Html.Attributes as A
import           Alder.Types

-- | Create a text node.
text :: Text -> Element
text = Text

-- | Create a tag with attributes and the specified component name.
tag :: Text -> A.Attributes -> [Element] -> Element
tag name attrs = Element name component
  where
    component = Component
        { handleEvent = mempty
        , create      = A.applyAttributes attrs
        , destroy     = \_ -> return ()
        }

-- | The HTML @div@ element.
div :: A.Attributes -> [Element] -> Element
div = tag "div"

-- | The HTML @span@ element.
span :: A.Attributes -> [Element] -> Element
span = tag "span"
