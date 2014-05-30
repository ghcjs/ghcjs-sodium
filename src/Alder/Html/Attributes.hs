{-# LANGUAGE OverloadedStrings #-}
module Alder.Html.Attributes
    ( class_
    , hidden
    , id
    , onClick
    ) where

import           Prelude             (($), fail)

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as HashSet
import           Data.Text           (Text)

import qualified Alder.Html.Events   as E
import           Alder.Html.Internal

attribute :: Text -> Text -> Attribute
attribute name value = Attribute $ \a ->
    a { attributes = HashMap.insert name value (attributes a) }

booleanAttribute :: Text -> Attribute
booleanAttribute name = attribute name ""

onEvent :: (Handler f, FromJSON e) => Text -> f e -> Attribute
onEvent eventName handler = Attribute $ \a ->
    a { handlers = HashMap.insert eventName h (handlers a) }
  where
    h v = case fromJSON v of
        Error s   -> fail s
        Success e -> fire handler e

class_ :: Text -> Attribute
class_ v = Attribute $ \a ->
    a { classSet = HashSet.insert v (classSet a) }

hidden :: Attribute
hidden = booleanAttribute "hidden"

id :: Text -> Attribute
id = attribute "id"

onClick :: Handler f => f E.MouseEvent -> Attribute
onClick = onEvent "click"
