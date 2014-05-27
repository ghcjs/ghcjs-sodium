{-# LANGUAGE OverloadedStrings #-}
module Alder.Html
    ( Element
    , text
    , tag
    , div
    , span
    ) where

import           Prelude               hiding (div, span)

import           Data.Text             (Text)

import qualified Alder.Html.Attributes as A
import           Alder.Mount

-- | Create a text node.
text :: Text -> Element
text = Text

-- | Create a tag with attributes and the specified component name.
tag :: Text -> A.Attributes -> [Element] -> Element
tag name attrs = Element name (A.attributes attrs) (A.handleEvent attrs)

-- | The HTML @div@ element.
div :: A.Attributes -> [Element] -> Element
div = tag "div"

-- | The HTML @span@ element.
span :: A.Attributes -> [Element] -> Element
span = tag "span"
