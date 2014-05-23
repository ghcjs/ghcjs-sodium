-- | Inputs.
module FRP.GHCJS.Input
    ( -- * Input
      Input(..)
    , newInput
    ) where

import           Control.Applicative
import           Control.Arrow
import           Data.Monoid
import           FRP.Sodium

import           Data.Default

-- | An input into the event graph.
newtype Input a = Input { fire :: a -> Reactive () }

instance Monoid (Input a) where
    mempty = Input $ \_ -> return ()
    mappend (Input f) (Input g) = Input $ \a -> f a >> g a

instance Default (Input a) where
    def = mempty

-- | Create a new input connected to an 'Event'.
newInput :: Reactive (Event a, Input a)
newInput = second Input <$> newEvent
