{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | HTML attributes and properties.
module FRP.GHCJS.Attributes
    ( Default(..)
      -- * Event handlers
    , EventHandlers
    , HasEventHandlers(..)
      -- * Global attributes
    , ElementId
    , Style
    , Dir(..)
    , GlobalAttributes
    , HasGlobalAttributes(..)
      -- * Input attributes
    , InputType(..)
    , InputValue
    , URL
    , Step(..)
    , InputAttributes
    , HasInputAttributes(..)
    ) where

import           Prelude                       hiding (max, min)

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable                 (Foldable)
import qualified Data.Foldable                 as Foldable
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.HashSet                  (HashSet)
import           Data.Maybe
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           GHC.Generics                  (Generic)
import           GHCJS.Types

import           Data.Default
import qualified FRP.GHCJS.DOM                 as DOM
import           FRP.GHCJS.Event
import           FRP.GHCJS.Input
import           FRP.GHCJS.JavaScript
import           FRP.GHCJS.Internal.Attributes

-- | Set or remove an attribute from an element.
attribute
    :: a
    -> DOM.Element
    -> Getting (Maybe Text) a b
    -> Text
    -> (b -> Maybe Text)
    -> IO ()
attribute a e l attr f = case views l f a of
    Nothing  -> call e "removeAttribute" attr
    Just new -> do
        old <- do
            hasAttr <- call e "hasAttribute" attr
            if hasAttr
                then Just <$> call e "getAttribute" attr
                else return Nothing
        when (old /= Just new) $ apply e "setAttribute" (attr, new)

-- | Set a property on an element.
property
    :: (Eq b, JSValue b)
    => a
    -> DOM.Element
    -> Getting b a c
    -> Text
    -> (c -> b)
    -> IO ()
property a e l prop f = do
    let new = views l f a
    old <- e ! prop
    when (old /= new) $ setProp e prop new

-- | Update the style of an element.
updateStyle :: DOM.Element -> HashMap Text Text -> IO ()
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
    upto n = takeWhile (< n) [0..]

-- | Convert a non-empty text attribute.
nonNull :: Text -> Maybe Text
nonNull t = if Text.null t then Nothing else Just t

-- | Convert a boolean attribute.
boolean :: Bool -> Maybe Text
boolean b = if b then Just "" else Nothing

-- | Convert a ternary attribute, which may be @true@, @false@, or a third
-- default state.
ternary :: Maybe Bool -> Maybe Text
ternary = fmap (\b -> if b then "true" else "false")

-- | Show a value as text.
showText :: Show a => a -> Text
showText = Text.pack . show

-- | Separate the elements of a container with spaces.
spaceSep :: Foldable f => (a -> Text) -> f a -> Text
spaceSep f = Text.intercalate " " . map f . Foldable.toList

-- | Event handlers.
data EventHandlers = EventHandlers
    { _click :: Input MouseEvent
    } deriving (Generic)

makeClassy ''EventHandlers

instance Default EventHandlers

-- | An element identifier.
type ElementId = Text

-- | A CSS style declaration.
type Style = HashMap Text Text

-- | Text directionality.
data Dir = LTR | RTL | Auto
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Global attributes.
data GlobalAttributes = GlobalAttributes
    { _globalEventHandlers :: EventHandlers
    , _accessKey           :: !(Set Char)
    , _className           :: !(HashSet Text)
    , _contentEditable     :: !(Maybe Bool)
    , _contextMenu         :: !ElementId
    , _dir                 :: !(Maybe Dir)
    , _draggable           :: !(Maybe Bool)
    , _hidden              :: !Bool
    , _id_                 :: !ElementId
    , _lang                :: !Text
    , _spellCheck          :: !(Maybe Bool)
    , _style               :: !Style
    , _tabIndex            :: !(Maybe Int)
    , _title               :: !Text
    } deriving (Generic)

makeClassy ''GlobalAttributes

instance Default GlobalAttributes

instance HasEventHandlers GlobalAttributes where
    eventHandlers = globalEventHandlers

instance Attributes GlobalAttributes where
    applyAttributes a e = do
        attr accessKey       "accessKey"       (nonNull . spaceSep Text.singleton)
        prop className       "className"       (spaceSep id)
        attr contentEditable "contentEditable" ternary
        attr contextMenu     "contextmenu"     nonNull
        attr dir             "dir"             (fmap dirValue)
        attr draggable       "draggable"       ternary
        prop hidden          "hidden"          id
        prop id_             "id"              id
        attr lang            "lang"            nonNull
        attr spellCheck      "spellcheck"      ternary
        attr tabIndex        "tabindex"        (fmap showText)
        attr title           "title"           nonNull

        updateStyle e (a ^. style)
      where
        attr = attribute a e
        prop = property a e

        dirValue LTR  = "ltr"
        dirValue RTL  = "rtl"
        dirValue Auto = "auto"

-- | An input type.
data InputType
    = Hidden | Text | Search | URL | Tel | Email | Password | DateTime
    | Date | Month | Week | Time | Number | Range | Color | Checkbox | Radio
    | File | Submit | Image | Reset | Button
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Default InputType where
    def = Text

-- | An input value.
-- TODO: better type
type InputValue = Text

-- | A URL string.
type URL = Text

-- | An input step value.
data Step = AnyStep | Step Double
    deriving (Eq, Ord, Read, Show)

-- | Input element attributes.
data InputAttributes = InputAttributes
    { _inputGlobalAttributes :: !GlobalAttributes
    , _accept                :: !Text
    , _alt                   :: !Text
    , _autocomplete          :: !Text
    , _autofocus             :: !Bool
    , _checked               :: !Bool
    , _disabled              :: !Bool
    , _form                  :: !ElementId
    , _height                :: !(Maybe Int)
    , _list                  :: !ElementId
    , _max                   :: !InputValue
    , _maxLength             :: !(Maybe Int)
    , _min                   :: !InputValue
    , _minLength             :: !(Maybe Int)
    , _multiple              :: !Bool
    , _name                  :: !Text
    , _pattern               :: !Text
    , _placeHolder           :: !Text
    , _readOnly              :: !Bool
    , _required              :: !Bool
    , _size                  :: !(Maybe Int)
    , _src                   :: !URL
    , _step                  :: !(Maybe Step)
    , _type_                 :: !InputType
    , _value                 :: !InputValue
    , _width                 :: !(Maybe Int)
    } deriving (Generic)

makeClassy ''InputAttributes

instance Default InputAttributes

instance HasEventHandlers InputAttributes where
    eventHandlers = globalAttributes . eventHandlers

instance HasGlobalAttributes InputAttributes where
    globalAttributes = inputGlobalAttributes

instance Attributes InputAttributes where
    applyAttributes a e = do
        applyAttributes (a ^. globalAttributes) e

        -- set "checked" and "value" as attributes so we can later retrieve
        -- them when extracting change events
        attr accept       "accept"       nonNull
        attr alt          "alt"          nonNull
        attr autocomplete "autocomplete" nonNull
        prop autofocus    "autofocus"    id
        attr checked      "checked"      boolean
        prop disabled     "disabled"     id
        attr form         "form"         nonNull
        attr height       "height"       (fmap showText)
        attr list         "list"         nonNull
        attr max          "max"          nonNull
        attr maxLength    "maxlength"    (fmap showText)
        attr min          "min"          nonNull
        attr minLength    "minlength"    (fmap showText)
        prop multiple     "multiple"     id
        attr name         "name"         nonNull
        attr pattern      "pattern"      nonNull
        attr placeHolder  "placeholder"  nonNull
        prop readOnly     "readonly"     id
        prop required     "required"     id
        attr size         "size"         (fmap showText)
        attr src          "src"          nonNull
        attr step         "step"         (fmap stepValue)
        attr type_        "type"         (Just . typeValue)
        attr value        "value"        Just
        attr width        "width"        (fmap showText)
      where
        attr = attribute a e
        prop = property a e

        stepValue AnyStep  = "any"
        stepValue (Step s) = showText s

        -- isn't that convenient
        typeValue = Text.toLower . showText
