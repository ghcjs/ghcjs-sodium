-- | Inputs.
module FRP.GHCJS.Input
    ( -- * Input
      Input(..)
    ) where

import           Data.Functor.Contravariant
import           Data.Monoid

import           Data.Default

-- | An input into the event graph.
newtype Input a = Input { fire :: a -> IO () }

instance Monoid (Input a) where
    mempty = Input $ \_ -> return ()
    mappend (Input f) (Input g) = Input $ \a -> f a >> g a

instance Default (Input a) where
    def = mempty

instance Contravariant Input where
    contramap f (Input g) = Input (g . f)
