{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
-- | Default values, and a generic deriving mechanism.
module FRP.GHCJS.Default
    ( Default(..)
    ) where

import           Data.Text    as Text
import           GHC.Generics

-- | A class for default values.
class Default a where
    -- | The default value. The default value can be derived for you; it
    -- defaults to the leftmost constructor.
    def :: a
    default def :: (Generic a, GDefault (Rep a)) => a
    def = to gdef

instance Default ()
instance Default Bool
instance Default (Maybe a)

instance Default Text where
    def = Text.empty

-- | Generic default values.
class GDefault f where
    gdef :: f a

instance GDefault V1 where
    gdef = undefined

instance GDefault U1 where
    gdef = U1

instance Default a => GDefault (K1 i a) where
    gdef = K1 def

instance GDefault a => GDefault (M1 i c a) where
    gdef = M1 gdef

instance GDefault a => GDefault (a :+: b) where
    gdef = L1 gdef

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
    gdef = gdef :*: gdef
