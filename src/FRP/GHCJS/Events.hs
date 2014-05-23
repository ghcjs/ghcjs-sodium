{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Events.
module FRP.GHCJS.Events
    ( MouseEvent(..)
    , Events
    , HasEvents(..)
    ) where

import           Control.Lens.TH
import           GHC.Generics

import           Data.Default
import           FRP.GHCJS.Input

-- | A mouse event.
data MouseEvent = MouseEvent

-- | A set of event handlers for an element.
data Events = Events
    { _click :: Input MouseEvent
    } deriving (Generic)

instance Default Events

makeClassy ''Events
