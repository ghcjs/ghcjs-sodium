{-# LANGUAGE OverloadedStrings #-}
module FRP.GHCJS.Element
    ( -- * Text
      Element
    , text
      -- * Grouping content
    , div
    , main
      -- * Text-level semantics
    , span
      -- * Forms
    , input
    ) where

import           Prelude                       hiding (div, span)

import           Control.Lens
import           Data.Text                     (Text)
import qualified GHCJS.DOM.Event               as DOM

import qualified FRP.GHCJS.Attributes          as A
import           FRP.GHCJS.Input
import           FRP.GHCJS.Internal.Attributes
import           FRP.GHCJS.Internal.Element
import           FRP.GHCJS.Internal.Event

-- | Create a text node.
text :: Text -> Element
text = Text

-- | Select a handler based on the event type.
selectInput :: A.EventHandlers -> EventType -> Input DOM.Event
selectInput handlers evType = case evType of
      Click -> extract A.click
    where
      extract l = Input $ \a -> do
        b <- extractEvent evType a
        fire (handlers ^. l) b

-- | Create a tag with attributes and the specified component name.
tag
    :: (Attributes a, A.HasEventHandlers a)
    => Text
    -> a
    -> [Element]
    -> Element
tag name attrs = Element name component
  where
    component = Component
        { handleEvent = selectInput (attrs ^. A.eventHandlers)
        , create      = applyAttributes attrs
        , destroy     = \_ -> return ()
        }

-- | The HTML @div@ element.
div :: A.GlobalAttributes -> [Element] -> Element
div = tag "div"

-- | The HTML @main@ element.
main :: A.GlobalAttributes -> [Element] -> Element
main = tag "main"

-- | The HTML @span@ element.
span :: A.GlobalAttributes -> [Element] -> Element
span = tag "span"

-- | The HTML @input@ element.
input :: A.InputAttributes -> [Element] -> Element
input = tag "input"
