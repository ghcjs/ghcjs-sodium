{-# LANGUAGE OverloadedStrings #-}
-- | HTML attributes and properties.
module Alder.Html.Attributes
    ( ElementId
    , Style
    , Attributes
    , def
    , attributes
    , handleEvent
    , class_
    , hidden
    , id
    , onClick
    , style
    ) where

import           Prelude             hiding (id)

import           Data.Aeson.Types
import           Data.Foldable       as Foldable (Foldable, toList)
import           Data.HashMap.Strict as HashMap
import           Data.HashSet        as HashSet
import           Data.Monoid
import           Data.Text           as Text

import           Alder.Html.Events

data Attributes = Attributes
    { attributes :: !(HashMap Text Text)
    , events     :: !(HashMap Text (Value -> IO ()))
    }

def :: Attributes
def = Attributes
    { attributes = HashMap.empty
    , events     = HashMap.empty
    }

handleEvent :: Attributes -> Value -> IO ()
handleEvent attrs v = case parse eventType v of
    Error   _ -> return ()
    Success t -> case HashMap.lookup t (events attrs) of
        Nothing -> return ()
        Just h  -> h v
  where
    eventType = withObject "event" $ \ev -> ev .: "type"

attribute :: Text -> Maybe Text -> Attributes -> Attributes
attribute attr v = withAttributes $ case v of
    Nothing -> HashMap.delete attr
    Just s  -> HashMap.insert attr s
  where
    withAttributes f a = a { attributes = f (attributes a) }

event :: FromJSON e => Text -> (e -> IO ()) -> Attributes -> Attributes
event name h = withEvents $ HashMap.insert name extract
  where
    withEvents f a = a { events = f (events a) }

    extract v = case fromJSON v of
        Error s   -> fail s
        Success e -> h e

-- | Convert a non-empty text attribute.
nonNull :: Text -> Maybe Text
nonNull t = if Text.null t then Nothing else Just t

-- | Convert a boolean attribute.
boolean :: Bool -> Maybe Text
boolean b = if b then Just "" else Nothing

-- | Separate the elements of a container with spaces.
spaceSep :: Foldable f => f Text -> Maybe Text
spaceSep = nonNull . Text.intercalate " " . Foldable.toList

-- | An element identifier.
type ElementId = Text

-- | A CSS style declaration.
type Style = HashMap Text Text

class_ :: HashSet Text -> Attributes -> Attributes
class_ = attribute "class" . spaceSep

hidden :: Bool -> Attributes -> Attributes
hidden = attribute "hidden" . boolean

id :: ElementId -> Attributes -> Attributes
id = attribute "id" . nonNull

onClick :: (MouseEvent -> IO ()) -> Attributes -> Attributes
onClick = event "click"

style :: HashMap Text Text -> Attributes -> Attributes
style = attribute "style" . spaceSep . fmap fromValue . HashMap.toList
  where
    fromValue (k, v) = k <> ": " <> v <> ";"
