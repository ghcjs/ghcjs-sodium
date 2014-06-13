module Alder.Unique
    ( -- * Unique values
      Unique
      -- * Supplying 'Unique's
    , MonadSupply(..)
      -- * Tagging values
    , Tagged(..)
    , tag
    , untag
    ) where

import Control.Monad

type Unique = Int

class Monad m => MonadSupply m where
    getUnique :: m Unique

data Tagged a = !Unique :< a

tag :: MonadSupply m => a -> m (Tagged a)
tag a = liftM (:< a) getUnique

untag :: Tagged a -> a
untag (_ :< a) = a
