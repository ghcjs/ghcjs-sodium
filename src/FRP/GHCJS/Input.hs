{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Events.
module FRP.GHCJS.Input
    ( -- * Input
      Input(..)
    , newInput
      -- * Events
    , MouseEvent(..)
      -- * Inputs
    , Inputs(..)
    , click
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens.TH
import           Data.Monoid
import           FRP.Sodium
import           GHC.Generics

import           Data.Default

-- | An input into the event graph.
newtype Input a = Input (a -> Reactive ())

instance Monoid (Input a) where
    mempty = Input $ \_ -> return ()
    mappend (Input f) (Input g) = Input $ \a -> f a >> g a

instance Default (Input a) where
    def = mempty

-- | Create a new input connected to an 'Event'.
newInput :: Reactive (Event a, Input a)
newInput = second Input <$> newEvent

-- | A mouse event.
data MouseEvent = MouseEvent

-- | A set of event handlers for an element.
data Inputs = Inputs
    { _click :: Input MouseEvent
    } deriving (Generic)

instance Default Inputs

makeLenses ''Inputs
