{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | HTML attributes and properties.
module FRP.GHCJS.Attributes
    ( Default(..)
    , Attributes(..)
      -- * Attributes
    , GlobalAttributes
    , accessKey
    , className
    , contentEditable
    , contextMenu
    , dir
    , draggable
    , hidden
    , id_
    , lang
    , spellCheck
    , style
    , tabIndex
    , title
    ,
    ) where

import           Control.Lens
import           Control.Monad
import           Data.Foldable         as Foldable
import           Data.HashSet
import           Data.Set
import           Data.Text             as Text
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
    Nothing  -> DOM.elementRemoveAttribute e attr
    Just new -> do
        old <- DOM.elementGetAttribute e attr
        when (old /= new) $ DOM.elementSetAttribute e attr new

-- | Set a property on an element.
property
    :: Eq b
    => a
    -> e
    -> Getting b a c
    -> (e -> IO b)
    -> (e -> b -> IO ())
    -> (c -> b)
    -> IO ()
property a e l getProp setProp f = do
    let new = views l f a
    old <- getProp e
    when (old /= new) $ setProp e new

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
    , _id_             :: ElementId
    , _lang            :: Text
    , _spellCheck      :: Maybe Bool
    , _style           :: Style
    , _tabIndex        :: Maybe Int
    , _title           :: Text
    } deriving (Eq, Show, Generic)

makeClassy ''GlobalAttributes

instance Default GlobalAttributes

instance Attributes GlobalAttributes where
    applyAttributes a n = do
        attr accessKey       "accessKey"       (nonNull . spaceSep Text.singleton)
        attr contentEditable "contentEditable" ternary
        attr contextMenu     "contextmenu"     nonNull
        attr dir             "dir"             (fmap dirValue)
        attr draggable       "draggable"       ternary
        attr lang            "lang"            nonNull
        attr spellCheck      "spellcheck"      ternary
        attr tabIndex        "tabindex"        (fmap showText)
        attr title           "title"           nonNull


        prop className DOM.elementGetClassName  DOM.elementSetClassName  (spaceSep id)
        prop hidden    DOM.htmlElementGetHidden DOM.htmlElementSetHidden id
        prop id_       DOM.elementGetId         DOM.elementSetId         id
        -- TODO: style
      where
        e    = DOM.castToHTMLElement n
        attr = attribute a e
        prop = property a e

        dirValue LTR  = "ltr"
        dirValue RTL  = "rtl"
        dirValue Auto = "auto"
