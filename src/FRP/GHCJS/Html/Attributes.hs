{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeOperators             #-}
-- | HTML attributes and properties.
module FRP.GHCJS.Html.Attributes
    ( ElementId
    , Style
    , Attributes(..)
    , def
    , applyAttributes
    ) where

import           Control.Monad
import           Data.Foldable         as Foldable (Foldable, toList)
import           Data.HashMap.Strict   as HashMap hiding ((!))
import           Data.HashSet          as HashSet
import           Data.Maybe
import           Data.Monoid
import           Data.Set              as Set
import           Data.Text             as Text
import           GHCJS.Types

import           FRP.GHCJS.Html.Events
import           FRP.GHCJS.JavaScript
import           FRP.GHCJS.Types

-- | Set or remove an attribute from an element.
attribute :: DOMElement -> Text -> Maybe Text -> IO ()
attribute e attr Nothing  = call e "removeAttribute" attr
attribute e attr (Just v) = apply e "setAttribute" (attr, v)

-- | Set a property on an element.
property :: JSValue a => DOMElement -> Text -> a -> IO ()
property = setProp

-- | Update the style of an element.
updateStyle :: DOMElement -> HashMap Text Text -> IO ()
updateStyle e obj = do
    decl <- e ! "style" :: IO (JSRef ())
    -- TODO: We get a little overzealous removing properties because we don't
    -- check for shorthand properties.
    len <- decl ! "length" :: IO Int
    toRemove <- fmap catMaybes . forM (upto len) $ \i -> do
        prop <- call decl "item" i
        return $ if HashMap.member prop obj then Nothing else Just prop
    forM_ toRemove $ \prop -> call decl "removeProperty" prop :: IO ()
    forM_ (HashMap.toList obj) $ \(prop, new) -> do
        old <- call decl "getPropertyValue" prop
        when (old /= Just new) $ apply decl "setProperty" (prop, new)
  where
    upto n = Prelude.takeWhile (< n) [0..]

-- | Convert a non-empty text attribute.
nonNull :: Text -> Maybe Text
nonNull t = if Text.null t then Nothing else Just t

-- | Convert a ternary attribute, which may be @true@, @false@, or a third
-- default state.
ternary :: Maybe Bool -> Maybe Text
ternary = fmap (\b -> if b then "true" else "false")

-- | Show a value as text.
showText :: Show a => a -> Text
showText = Text.pack . show

-- | Separate the elements of a container with spaces.
spaceSep :: Foldable f => (a -> Text) -> f a -> Text
spaceSep f = Text.intercalate " " . fmap f . Foldable.toList

-- | An element identifier.
type ElementId = Text

-- | A CSS style declaration.
type Style = HashMap Text Text

-- | Attributes.
data Attributes = Attributes
    { _click           :: Input MouseEvent
    , _accessKey       :: !(Set Char)
    , _className       :: !(HashSet Text)
    , _contentEditable :: !(Maybe Bool)
    , _contextMenu     :: !ElementId
    , _draggable       :: !(Maybe Bool)
    , _hidden          :: !Bool
    , _id_             :: !ElementId
    , _lang            :: !Text
    , _spellCheck      :: !(Maybe Bool)
    , _style           :: !Style
    , _tabIndex        :: !(Maybe Int)
    , _title           :: !Text
    }

-- | Default attributes.
def :: Attributes
def = Attributes
    { _click           = mempty
    , _accessKey       = Set.empty
    , _className       = HashSet.empty
    , _contentEditable = Nothing
    , _contextMenu     = Text.empty
    , _draggable       = Nothing
    , _hidden          = False
    , _id_             = Text.empty
    , _lang            = Text.empty
    , _spellCheck      = Nothing
    , _style           = HashMap.empty
    , _tabIndex        = Nothing
    , _title           = Text.empty
    }

-- | Apply attributes to a newly-created DOM element.
applyAttributes :: Attributes -> DOMElement -> IO ()
applyAttributes a e = do
    attr "accessKey"       (nonNull . spaceSep Text.singleton . _accessKey)
    prop "className"       (spaceSep id . _className)
    attr "contentEditable" (ternary . _contentEditable)
    attr "contextmenu"     (nonNull . _contextMenu)
    attr "draggable"       (ternary . _draggable)
    prop "hidden"          _hidden
    prop "id"              _id_
    attr "lang"            (nonNull . _lang)
    attr "spellcheck"      (ternary . _spellCheck)
    attr "tabindex"        (fmap showText . _tabIndex)
    attr "title"           (nonNull . _title)

    updateStyle e (_style a)
  where
    attr k f = attribute e k (f a)
    prop k f = property e k (f a)
