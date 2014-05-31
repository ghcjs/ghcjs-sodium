{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Alder.Html.Attributes
    ( class_
    , hidden
    , id
    , onClick
    ) where

import           Data.Text           (Text)

import qualified Alder.Html.Events   as E
import           Alder.Html.Internal

class_ :: Text -> Attribute
class_ = attribute "class"

hidden :: Attribute
hidden = boolean "hidden"

id :: Text -> Attribute
id = attribute "id"

onClick :: Handler f => f E.MouseEvent -> Attribute
onClick = onEvent "click"
