-- | Approximate equality.
module Data.StableRef
    ( StableRef
    , makeStableRef
    , deref
    ) where

import           Control.Applicative
import           Data.Hashable
import           System.Mem.StableName

-- | A reference to a value with approximate equality. 'StableRef's have
-- approximate equality: if @p '==' q@, then @'deref' p@ and @'deref' q@
-- are the same Haskell object. The converse is not necessarily true, but is
-- likely.
data StableRef a = StableRef (StableName a) a

instance Eq (StableRef a) where
    StableRef m _ == StableRef n _ = m == n

instance Hashable (StableRef a) where
    hashWithSalt salt (StableRef n _) = hashWithSalt salt n

-- | Make a 'StableRef' from an arbitrary object.
makeStableRef :: a -> IO (StableRef a)
makeStableRef a = StableRef <$> makeStableName a <*> pure a

-- | Dereference a 'StableRef'.
deref :: StableRef a -> a
deref (StableRef _ a) = a
