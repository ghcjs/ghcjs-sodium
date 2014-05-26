{-# LANGUAGE DeriveGeneric #-}
-- | HTML events.
module FRP.GHCJS.Internal.Event
    ( EventType(..)
    , Event(..)
    ) where

import           Data.Hashable
import           GHC.Generics  (Generic)

import           FRP.GHCJS.DOM

-- | Event types.
data EventType = Click
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Hashable EventType

-- | HTML event types.
class Event e where
    -- | Extract relevant information out of a 'DOMEvent'.
    extractEvent :: EventType -> DOMEvent -> IO e
