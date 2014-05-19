{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | DOM elements.
module FRP.GHCJS.Element
    ( -- * Elements
      Element
    , element
      -- * Nodes
    , Node
    , _Parent
    , _Text
      -- * Tags
    , Tag
    , tagName
    , properties
    , children
    ) where

import           Control.Lens.TH
import           Data.Aeson
import           Data.DList          (DList)
import           Data.HashMap.Strict (HashMap)
import           Data.Monoid
import           Data.Text           (Text)

-- | A consecutive sequence of DOM elements.
newtype Element = Element (DList Node)
    deriving (Monoid)

-- | A DOM node.
data Node
    = Parent Tag
    | Text Text

-- | An HTML tag.
data Tag = Tag
    { _tagName    :: Text
    , _properties :: HashMap Text Value
    , _children   :: Element
    }

makeIso    ''Element
makePrisms ''Node
makeLenses ''Tag
