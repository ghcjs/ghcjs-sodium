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
    , HasGlobalAttributes(..)
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
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
import           GHCJS.DOM.Types               (maybeJSNull)
import           GHCJS.Foreign                 (fromJSString)
import           GHCJS.Types

import           Data.Default
import           FRP.GHCJS.Input
import           FRP.GHCJS.Mount

-- | DOM element attributes and properties.
class Default a => Attributes a where
    -- | Apply a set of attributes to a DOM node.
    applyAttributes :: a -> DOM.Element -> Mount ()

-- | Set or remove an attribute from an element.
attribute
    :: DOM.IsElement e
    => a
    -> e
    -> Getting (Maybe Text) a b
    -> Text
    -> (b -> Maybe Text)
    -> Mount ()
attribute a e l attr f = liftIO $ case views l f a of
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
    -> Mount ()
property a e l getProp setProp f = liftIO $ do
    let new = views l f a
    old <- getProp e
    when (old /= new) $ setProp e new

-- | Update the style of an element.
updateStyle :: DOM.IsElement e => e -> Style -> Mount ()
updateStyle e obj = liftIO $ do
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
    , _globalInputs    :: !Inputs
    } deriving (Generic)

makeClassy ''GlobalAttributes

instance Default GlobalAttributes

instance HasInputs GlobalAttributes where
    inputs = globalInputs

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

        updateInputs e (a ^. inputs)
      where
        attr = attribute a e
        prop = property a (DOM.castToHTMLElement e)

        dirValue LTR  = "ltr"
        dirValue RTL  = "rtl"
        dirValue Auto = "auto"
