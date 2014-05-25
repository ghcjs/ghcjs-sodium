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

import           Control.Lens
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Prelude                       hiding (div, span)

import qualified FRP.GHCJS.Attributes          as A
import           FRP.GHCJS.Events
import           FRP.GHCJS.Internal.Attributes
import           FRP.GHCJS.Internal.Element

-- | Create a text node.
text :: Text -> Element
text = Text

-- | Create a tag with attributes.
tag :: (Attributes a, HasEvents a) => Text -> a -> [Element] -> Element
tag name = tag' name name

-- | Create a tag with attributes and the specified component name.
tag'
    :: (Attributes a, HasEvents a)
    => Text
    -> Text
    -> a
    -> [Element]
    -> Element
tag' name compName attrs = Extend component . Tag name (attrs ^. events)
  where
    component = Component
        { componentName = compName
        , create        = applyAttributes attrs
        , update        = applyAttributes attrs
        , destroy       = \_ -> return ()
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
input attrs = tag' "input" compName attrs
  where
    -- changing input types is not possible in all browsers, so different
    -- types must be different component classes
    compName = Text.pack $ "input[type=" ++ show (attrs ^. A.type_) ++ "]"
