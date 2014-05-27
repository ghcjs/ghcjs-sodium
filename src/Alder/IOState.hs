{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | An IORef-based state monad.
-- TODO: MVar?
module Alder.IOState
    ( IOState(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State.Class
import           Data.IORef

-- | A strict IORef-based state monad.
newtype IOState s a = IOState { runIOState :: IORef s -> IO a }
    deriving (Functor)

instance Applicative (IOState s) where
    pure  = return
    (<*>) = ap

instance Monad (IOState s) where
    return  = IOState . const . return
    m >>= k = IOState $ \r -> do
        a <- runIOState m r
        runIOState (k a) r

instance MonadIO (IOState s) where
    liftIO = IOState . const

instance MonadState s (IOState s) where
    get    = IOState readIORef
    put !s = IOState $ \r -> writeIORef r s
