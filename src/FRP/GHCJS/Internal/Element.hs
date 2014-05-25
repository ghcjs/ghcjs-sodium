module FRP.GHCJS.Internal.Element
    ( Element(..)
    , Component(..)
    ) where

import           Data.Monoid
import           Data.Text                (Text)
import qualified GHCJS.DOM.Element        as DOM
import qualified GHCJS.DOM.Event          as DOM

import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Event

-- | A document element.
data Element
      -- | Extend a tag with initialization and events.
    = Element !Text !Component [Element]
      -- | A text node.
    | Text !Text

-- | A logical component in the document.
data Component = Component
    { -- | Handle a DOM event.
      handleEvent :: EventType -> Input DOM.Event
      -- | Create the component.
    , create      :: DOM.Element -> IO ()
      -- | Delete the component, performing any cleanup.
    , destroy     :: DOM.Element -> IO ()
    }

instance Monoid Component where
  mempty = Component
      { handleEvent = const mempty
      , create      = \_ -> return ()
      , destroy     = \_ -> return ()
      }
  mappend c1 c2 = Component
      { handleEvent = \evType -> handleEvent c1 evType
                              <> handleEvent c2 evType
      , create      = \e -> create  c1 e >> create  c2 e
      , destroy     = \e -> destroy c2 e >> destroy c1 e
      }
