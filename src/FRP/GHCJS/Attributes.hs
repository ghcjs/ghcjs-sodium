{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | HTML attributes and properties.
module FRP.GHCJS.Attributes where

import           Control.Lens
import           Control.Monad
import           Data.Foldable         as Foldable
import           Data.HashSet
import           Data.Text             as Text
import           Data.Set
import           GHC.Generics          (Generic)
import qualified GHCJS.DOM.Element     as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.Node        as DOM

import           FRP.GHCJS.Default

-- | DOM element attributes and properties.
class Default a => Attributes a where
    -- | Apply a set of attributes to a DOM node.
    applyAttributes :: a -> DOM.Node -> IO ()

-- | Set or remove an attribute from an element.
attribute
    :: DOM.IsElement e
    => a
    -> e
    -> Getting (Maybe Text) a b
    -> Text
    -> (b -> Maybe Text)
    -> IO ()
attribute a e l attr f = case views l f a of
    Nothing    -> DOM.elementRemoveAttribute e attr
    Just value -> DOM.elementSetAttribute e attr value

-- | Set a property on an element.
property
    :: a
    -> e
    -> Getting b a c
    -> (e -> b -> IO ())
    -> (c -> b)
    -> IO ()
property a e l setProp f = setProp e (views l f a)

-- | Set a property on an element, and remove it by removing the attribute.
hybridProperty
    :: DOM.IsElement e
    => a
    -> e
    -> Getting (Maybe b) a c
    -> (e -> b -> IO ())
    -> Text
    -> (c -> Maybe b)
    -> IO ()
hybridProperty a e l setProp attr f = case views l f a of
    Nothing    -> DOM.elementRemoveAttribute e attr
    Just value -> setProp e value

-- | Set a property on an element if the value has not changed. Use this for
-- properties such as @value@ that have side-effects when set.
safeProperty
    :: Eq b
    => a
    -> e
    -> Getting b a c
    -> (e -> IO b)
    -> (e -> b -> IO ())
    -> (c -> b)
    -> IO ()
safeProperty a e l getProp setProp f = do
    let new = views l f a
    old <- getProp e
    when (old /= new) $ setProp e new

-- | Convert a boolean attribute.
boolean :: Text -> Bool -> Maybe Text
boolean name b = if b then Just name else Nothing

-- | Convert a ternary attribute, which may be @true@, @false@, or a third
-- state.
ternary :: Text -> Maybe Bool -> Text
ternary third = maybe third (\b -> if b then "true" else "false")

-- | Convert a non-empty text attribute.
nonNull :: Text -> Maybe Text
nonNull t = if Text.null t then Nothing else Just t

-- | Separate the elements of a container with spaces.
spaceSep :: Foldable f => (a -> Text) -> f a -> Text
spaceSep f = Text.intercalate " " . fmap f . Foldable.toList

-- | An element identifier.
type ElementId = Text

-- | Text directionality.
data Dir = LTR | RTL | Auto
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Element style.
type Style = ()

-- | Global attributes.
data GlobalAttributes = GlobalAttributes
    { _accessKey       :: Set Char
    , _className       :: HashSet Text
    , _contentEditable :: Maybe Bool
    , _contextMenu     :: ElementId
    , _dir             :: Maybe Dir
    , _draggable       :: Maybe Bool
    , _hidden          :: Bool
    , __id             :: ElementId
    , _lang            :: Text
    , _spellCheck      :: Bool
    , _style           :: Style
    , _tabIndex        :: Maybe Int
    , _title           :: Text
    } deriving (Eq, Show, Generic)

makeClassy ''GlobalAttributes

instance Default GlobalAttributes

instance Attributes GlobalAttributes where
    applyAttributes a n = do
        prop    accessKey       DOM.htmlElementSetAccessKey             (spaceSep Text.singleton)
        prop    className       DOM.elementSetClassName                 (spaceSep id)
        prop    contentEditable DOM.htmlElementSetContentEditable       (ternary "inherit")
        attr    contextMenu     "contextmenu"                           nonNull
        hybrid  dir             DOM.htmlElementSetDir "dir"             (fmap dirValue)
        hybrid  draggable       DOM.htmlElementSetDraggable "draggable" id
        attr    hidden          "hidden"                                (boolean "hidden")
        prop    _id             DOM.elementSetId                        id
        prop    lang            DOM.htmlElementSetLang                  id
        prop    spellCheck      DOM.htmlElementSetSpellcheck            id
        -- TODO: style
        hybrid  tabIndex        DOM.htmlElementSetTabIndex "tabindex"   id
        prop    title           DOM.htmlElementSetTitle                 id
      where
        e      = DOM.castToHTMLElement n
        attr   = attribute a e
        prop   = property a e
        hybrid = hybridProperty a e

        dirValue :: Dir -> Text
        dirValue LTR  = "ltr"
        dirValue RTL  = "rtl"
        dirValue Auto = "auto"
