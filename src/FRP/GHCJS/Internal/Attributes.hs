-- | HTML attributes and properties.
module FRP.GHCJS.Internal.Attributes
    ( Attributes(..)
    , getAttribute
    , setAttribute
    , removeAttribute
    ) where

import           Control.Applicative
import           Data.Text           (Text)
import qualified GHCJS.DOM.Element   as DOM

import           Data.Default

-- | DOM element attributes and properties.
class Default a => Attributes a where
    -- | Apply a set of attributes to a DOM node.
    applyAttributes :: a -> DOM.Element -> IO ()

-- | Get an attribute value.
getAttribute :: DOM.IsElement e => e -> Text -> IO (Maybe Text)
getAttribute e attr = do
    hasAttr <- DOM.elementHasAttribute e attr
    if hasAttr
        then Just <$> DOM.elementGetAttribute e attr
        else return Nothing

-- | Set the value of an attribute.
setAttribute :: DOM.IsElement e => e -> Text -> Text -> IO ()
setAttribute = DOM.elementSetAttribute

-- | Remove an attribute.
removeAttribute :: DOM.IsElement e => e -> Text -> IO ()
removeAttribute = DOM.elementRemoveAttribute
