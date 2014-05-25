module FRP.GHCJS.Internal.Element
    ( Element(..)
    , Component(..)
    ) where

import           Data.Text                (Text)
import qualified GHCJS.DOM.Element        as DOM
import qualified GHCJS.DOM.Event          as DOM

import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Event

-- | A document element.
data Element
      -- | Extend an 'Element' with initialization and update operations.
    = Extend Component Element
      -- | A vanilla HTML tag.
    | Tag !Text (EventType -> Input DOM.Event) [Element]
      -- | A text node.
    | Text !Text

-- | A logical component in the document.
data Component = Component
    { -- | A component name that uniquely identifies the type or class of
      -- this component. 'update' and 'delete' may assume that the 'Node'
      -- has been created by 'create' of the same component name.
      componentName :: !Text
      -- | Create the component.
    , create        :: DOM.Element -> IO ()
      -- | Update an existing DOM node for this component.
    , update        :: DOM.Element -> IO ()
      -- | Delete the component, performing any cleanup.
    , destroy       :: DOM.Element -> IO ()
    }
