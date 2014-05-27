{-# LANGUAGE EmptyDataDecls #-}
-- | Types and instances.
module Alder.Types
    ( -- * Inputs
      Input(..)
      -- * Events
    , DOMEvent
    , EventType(..)
      -- * Elements
    , DOMElement
    , Element(..)
    , Component(..)
    ) where

import           Data.Functor.Contravariant
import           Data.Hashable
import           Data.Monoid
import           Data.Text                  (Text)
import           GHCJS.Types

-- | An input into the event graph.
newtype Input a = Input { fire :: a -> IO () }

instance Monoid (Input a) where
    mempty = Input $ \_ -> return ()
    mappend (Input f) (Input g) = Input $ \a -> f a >> g a

instance Contravariant Input where
    contramap f (Input g) = Input (g . f)

data DOMEvent_

-- | A JavaScript DOM event.
type DOMEvent = JSRef DOMEvent_

-- | Event types.
data EventType = Click
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance Hashable EventType where
    hashWithSalt salt = hashWithSalt salt . fromEnum

data DOMElement_

-- | A JavaScript DOM node or element.
type DOMElement = JSRef DOMElement_

-- | A document element.
data Element
      -- | Extend a tag with initialization and events.
    = Element !Text !Component [Element]
      -- | A text node.
    | Text !Text

-- | A logical component in the document.
data Component = Component
    { -- | Handle a DOM event.
      handleEvent :: EventType -> Input DOMEvent
      -- | Create the component.
    , create      :: DOMElement -> IO ()
      -- | Delete the component, performing any cleanup.
    , destroy     :: DOMElement -> IO ()
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
