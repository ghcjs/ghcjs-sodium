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
    = Parent Component
    | Text Text

-- | An HTML component on top of a DOM node. The component name should
-- uniquely identify the type or class of component, so the component can be
-- reused for 'update'. In other words, 'update' and 'delete' may assume that
-- the 'DOM.Node' has been created by 'create' of the same component name.
data Component = Component
    { name     :: Text
    , create   :: IO DOM.Node
    , update   :: DOM.Node -> IO ()
    , delete   :: DOM.Node -> IO ()
    , children :: Element
    }
