{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | DOM elements.
module FRP.GHCJS.Element
    ( -- * Elements
      Element(..)
    , element
      -- * Nodes
    , Node(..)
    , _Parent
    , _Text
      -- * Tags
    , Tag(..)
    , tagName
    , properties
    , children
      -- * Properties
    , Properties(..)
    , defaultProperties
    , _class
    , _id
    ) where

import           Control.Lens.TH
import           Data.DList      (DList)
import           Data.HashSet    (HashSet)
import qualified Data.HashSet    as HashSet
import           Data.Monoid
import           Data.Text       (Text)

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
    , _properties :: Properties
    , _children   :: Element
    }

-- | HTML properties.
data Properties = Properties
    { __class :: HashSet Text
    , __id    :: Text
    }

defaultProperties :: Properties
defaultProperties = Properties
    { __class = HashSet.empty
    , __id    = ""
    }

makeIso    ''Element
makePrisms ''Node
makeLenses ''Tag
makeLenses ''Properties
