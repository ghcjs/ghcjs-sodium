{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | DOM types.
module FRP.GHCJS.Types
    ( -- * Mounting
      Mount
    , MountState(..)
    , document
      -- * Elements
    , Element(..)
    , _Extend
    , _Tag
    , _Text
    , Component(..)
    ) where

import           Control.Lens.TH
import           Data.Text          (Text)
import           GHCJS.DOM.Document
import           GHCJS.DOM.Node

import           Control.Monad.IOState

-- | A state monad for mounting.
type Mount = IOState MountState

-- | The mount state.
data MountState = MountState
    { _document :: !Document
    }

makeLenses ''MountState

-- | A document element.
data Element
      -- | Extend an 'Element' with initialization and update operations.
    = Extend Component Element
      -- | A vanilla HTML tag.
    | Tag !Text [Element]
      -- | A text node.
    | Text !Text

-- | A logical component in the document.
data Component = Component
    { -- | A component name that uniquely identifies the type or class of
      -- this component. 'update' and 'delete' may assume that the 'Node'
      -- has been created by 'create' of the same component name.
      componentName :: !Text
      -- | Create the component.
    , create        :: Node -> Mount ()
      -- | Update an existing DOM node for this component.
    , update        :: Node -> Mount ()
      -- | Delete the component, performing any cleanup.
    , destroy       :: Node -> Mount ()
    }

makePrisms ''Element
