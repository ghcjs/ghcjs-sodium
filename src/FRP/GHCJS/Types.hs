{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | DOM types.
module FRP.GHCJS.Types
    ( Element(..)
    , Node(..)
    , Component(..)
    ) where

import           Data.DList     (DList)
import           Data.Monoid
import           Data.Text      (Text)
import qualified GHCJS.DOM.Node as DOM

-- | A consecutive sequence of DOM elements.
newtype Element = Element (DList Node)
    deriving (Monoid)

-- | A DOM node.
data Node
    = Parent Component Element
    | Text Text

-- | An HTML component.
data Component = Component
    { -- | A component name that uniquely identifies the type or class of
      -- this component. 'update' and 'delete' may assume that the 'DOM.Node'
      -- has been created by 'create' of the same component name.
      name     :: Text
      -- | The underlying tag name.
    , tagName  :: Text
      -- | Create the component.
    , create   :: DOM.Node -> IO ()
      -- | Update an existing DOM node for this component.
    , update   :: DOM.Node -> IO ()
      -- | Delete the component, performing any cleanup.
    , delete   :: DOM.Node -> IO ()
    }
