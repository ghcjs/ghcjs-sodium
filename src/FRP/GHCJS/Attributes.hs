{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | HTML attributes and properties.
module FRP.GHCJS.Attributes where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Foldable       as Foldable
import           Data.Monoid
import           Data.Text           as Text
import qualified GHCJS.DOM.Element   as DOM
import qualified GHCJS.DOM.Node      as DOM

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
    -> Text
    -> Getting (First Text) a Text
    -> IO ()
attribute a e attr l = case a ^? l of
    Nothing    -> DOM.elementRemoveAttribute e attr
    Just value -> DOM.elementSetAttribute e attr value

-- | Set a property on an element.
property
    :: a
    -> e
    -> (e -> b -> IO ())
    -> Getting b a b
    -> IO ()
property a e setProp l = setProp e (a ^. l)

-- | Set a property on an element if the value has not changed. Use this for
-- properties such as @value@ that hav side-effects when set.
safeProperty
    :: Eq b
    => a
    -> e
    -> (e -> IO b)
    -> (e -> b -> IO ())
    -> Getting b a b
    -> IO ()
safeProperty a e getProp setProp l = do
    let new = a ^. l
    old <- getProp e
    when (old /= new) $ setProp e new

-- | Convert a boolean attributes.
boolean :: Text -> Fold Bool Text
boolean name = folding $ \b -> if b then Just name else Nothing

-- | Convert a non-empty text attribute.
nonEmpty :: Fold Text Text
nonEmpty = filtered (not . Text.null)

-- | Separate the elements of a container with spaces.
spaceSep :: Foldable f => Getting Text a Text -> Fold (f a) Text
spaceSep l = folding $ \f -> case view l <$> Foldable.toList f of
    [] -> Nothing
    ts -> Just (Text.intercalate " " ts)
