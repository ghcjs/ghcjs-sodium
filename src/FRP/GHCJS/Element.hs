-- | DOM elements.
module FRP.GHCJS.Element
    ( -- * Elements
      Element(..)
    , Component(..)
    ) where

import           Data.Text      (Text)
import qualified GHCJS.DOM.Node as DOM

-- | A document element.
data Element
      -- | Extend an 'Element' with initialization and update operations.
    = Extend Component Element
      -- | A vanilla HTML tag.
    | Tag Text [Element]
      -- | A text node.
    | Text Text

-- | A logical component in the document.
data Component = Component
    { -- | A component name that uniquely identifies the type or class of
      -- this component. 'update' and 'delete' may assume that the 'DOM.Node'
      -- has been created by 'create' of the same component name.
      name    :: Text
      -- | Create the component.
    , create  :: DOM.Node -> IO ()
      -- | Update an existing DOM node for this component.
    , update  :: DOM.Node -> IO ()
      -- | Delete the component, performing any cleanup.
    , destroy :: DOM.Node -> IO ()
    }
