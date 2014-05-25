{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
-- | HTML attributes and properties.
module FRP.GHCJS.Attributes
    ( Default(..)
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

import           Prelude                       hiding (min, max)

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
import qualified GHCJS.DOM.CSSStyleDeclaration as DOM
import qualified GHCJS.DOM.Element             as DOM
import qualified GHCJS.DOM.HTMLElement         as DOM
import qualified GHCJS.DOM.HTMLInputElement    as DOM
import           GHCJS.DOM.Types               (maybeJSNull)
import           GHCJS.Foreign                 (fromJSString)
import           GHCJS.Types

import           Data.Default
import           FRP.GHCJS.Events
import           FRP.GHCJS.Internal.Attributes

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

-- | Update the style of an element.
updateStyle :: DOM.IsElement e => e -> Style -> IO ()
updateStyle e obj = do
    Just decl <- DOM.elementGetStyle e
    -- TODO: We get a little overzealous removing properties because we don't
    -- check for shorthand properties.
    len <- DOM.cssStyleDeclarationGetLength decl
    toRemove <- fmap catMaybes . forM (upto len) $ \i -> do
        prop <- DOM.cssStyleDeclarationItem decl i
        return $ if HashMap.member prop obj then Nothing else Just prop
    forM_ toRemove $ \prop ->
        DOM.cssStyleDeclarationRemoveProperty decl prop :: IO JSString
    forM_ (HashMap.toList obj) $ \(prop, new) -> do
        old <- fmap fromJSString . maybeJSNull <$>
            DOM.cssStyleDeclarationGetPropertyValue decl prop
        when (old /= Just new) $
            DOM.cssStyleDeclarationSetProperty decl prop new ("" :: JSString)
  where
    upto n = takeWhile (< n) [0..]

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
spaceSep f = Text.intercalate " " . map f . Foldable.toList

-- | An element identifier.
type ElementId = Text

-- | A CSS style declaration.
type Style = HashMap Text Text

-- | Text directionality.
data Dir = LTR | RTL | Auto
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

-- | Global attributes.
data GlobalAttributes = GlobalAttributes
    { _accessKey       :: !(Set Char)
    , _className       :: !(HashSet Text)
    , _contentEditable :: !(Maybe Bool)
    , _contextMenu     :: !ElementId
    , _dir             :: !(Maybe Dir)
    , _draggable       :: !(Maybe Bool)
    , _hidden          :: !Bool
    , _id_             :: !ElementId
    , _lang            :: !Text
    , _spellCheck      :: !(Maybe Bool)
    , _style           :: !Style
    , _tabIndex        :: !(Maybe Int)
    , _title           :: !Text
    , _globalEvents    :: !Events
    } deriving (Generic)

makeClassy ''GlobalAttributes

instance Default GlobalAttributes

instance HasEvents GlobalAttributes where
    events = globalEvents

instance Attributes GlobalAttributes where
    applyAttributes a e = do
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

        updateStyle e (a ^. style)
      where
        attr = attribute a e
        prop = property a (DOM.castToHTMLElement e)

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

instance HasEvents InputAttributes where
    events = events

instance HasGlobalAttributes InputAttributes where
    globalAttributes = inputGlobalAttributes

instance Attributes InputAttributes where
    applyAttributes a e = do
        applyAttributes (a ^. globalAttributes) e

        attr accept       "accept"       nonNull
        attr alt          "alt"          nonNull
        attr autocomplete "autocomplete" nonNull
        attr form         "form"         nonNull
        attr height       "height"       (fmap showText)
        attr list         "list"         nonNull
        attr max          "max"          nonNull
        attr maxLength    "maxlength"    (fmap showText)
        attr min          "min"          nonNull
        attr minLength    "minlength"    (fmap showText)
        attr name         "name"         nonNull
        attr pattern      "pattern"      nonNull
        attr placeHolder  "placeholder"  nonNull
        attr size         "size"         (fmap showText)
        attr src          "src"          nonNull
        attr step         "step"         (fmap stepValue)
        attr type_        "type"         (Just . typeValue)
        attr width        "width"        (fmap showText)

        prop autofocus DOM.htmlInputElementGetAutofocus DOM.htmlInputElementSetAutofocus id
        prop checked   DOM.htmlInputElementGetChecked   DOM.htmlInputElementSetChecked   id
        prop disabled  DOM.htmlInputElementGetDisabled  DOM.htmlInputElementSetDisabled  id
        prop multiple  DOM.htmlInputElementGetMultiple  DOM.htmlInputElementSetMultiple  id
        prop readOnly  DOM.htmlInputElementGetReadOnly  DOM.htmlInputElementSetReadOnly  id
        prop required  DOM.htmlInputElementGetRequired  DOM.htmlInputElementSetRequired  id
        prop value     DOM.htmlInputElementGetValue     DOM.htmlInputElementSetValue     id
      where
        attr = attribute a e
        prop = property a (DOM.castToHTMLInputElement e)

        stepValue AnyStep  = "any"
        stepValue (Step s) = showText s

        -- isn't that convenient
        typeValue = Text.toLower . showText
