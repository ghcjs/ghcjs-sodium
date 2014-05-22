{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Event handlers.
module FRP.GHCJS.Input
    ( Handler(..)
    , Input(..)
    , newInput
    ) where

import           Control.Applicative
import           Data.DList          (DList)
import qualified Data.DList          as DList
import           Data.Hashable
import           Data.Monoid
import           FRP.Sodium
import           FRP.Sodium.Internal (ioReactive)

import           FRP.GHCJS.StableRef

-- | An event handler.
newtype Handler a = Handler (StableRef (a -> Reactive ()))
    deriving (Eq, Hashable)

-- | An input into the event graph.
newtype Input a = Input (DList (Handler a))
    deriving (Monoid)

-- | Create a new 'Input', linked to an 'Event'.
newInput :: Reactive (Event a, Input a)
newInput = do
    (e, push) <- newEvent
    handler <- ioReactive $ Handler <$> makeStableRef push
    return (e, Input (DList.singleton handler))
