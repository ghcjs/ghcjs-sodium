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
import qualified Data.Text                     as Text
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
      extract l = Input $ \ev -> do
          ev' <- extractEvent evType ev
          fire (handlers ^. l) ev'

-- | Create a tag with attributes and the specified component name.
tag'
    :: (Attributes a, A.HasEventHandlers a)
    => Text
    -> Text
    -> a
    -> [Element]
    -> Element
tag' name compName attrs = Extend component . Tag name handlers
  where
    component = Component
        { componentName = compName
        , create        = applyAttributes attrs
        , update        = applyAttributes attrs
        , destroy       = \_ -> return ()
        }

    handlers = selectInput (attrs ^. A.eventHandlers)

-- | Create a tag with attributes.
tag
    :: (Attributes a, A.HasEventHandlers a)
    => Text
    -> a
    -> [Element]
    -> Element
tag name = tag' name name

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
input attrs = tag' "input" compName attrs
  where
    -- changing input types is not possible in all browsers, so different
    -- types must be different component classes
    compName = Text.pack $ "input." ++ show (attrs ^. A.type_)
