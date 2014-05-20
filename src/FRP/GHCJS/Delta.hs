{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}
-- | Lenses for diffing data types.
module FRP.GHCJS.Delta
    ( -- * Deltas
      Delta(..)
    , oldValue
    , newValue
    , slice
    , match
    , equal
    , equalOn
      -- * Diffs
    , Edit(..)
    , diff
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable

-- | Two values, representing the old and new versions of data type. This is
-- also the diagonal functor in @Hask@, the category of Haskell types.
data Delta a = Delta a a
    deriving (Eq, Read, Show, Functor, Foldable, Traversable)

instance Applicative Delta where
    pure a = Delta a a
    Delta f g <*> Delta x y = Delta (f x) (g y)

-- | Get the old value of a 'Delta'.
oldValue :: Delta a -> a
oldValue (Delta x _) = x

-- | Get the new value of a 'Delta'
newValue :: Delta a -> a
newValue (Delta _ y) = y

-- | Focus on and compare one part of the structure.
slice :: ALens s t a b -> Lens (Delta s) (Delta t) (Delta a) (Delta b)
slice l = lens (fmap (^# l)) (\s b -> storing l <$> b <*> s)

-- | A 'Prism' that matches only if both the old and new versions of the type
-- match.
match :: APrism' s a -> Prism' (Delta s) (Delta a)
match = below

-- | A 'Fold' that matches only if the old and new version are equal.
equal :: Eq a => Fold (Delta a) (Delta a)
equal = equalOn id

-- | A 'Fold' that matches only if both the old and new versions yield the
-- same value when the getter is applied.
equalOn :: Eq a => (s -> a) -> Fold (Delta s) (Delta s)
equalOn f = filtered $ \(Delta x y) -> f x == f y

-- | A single edit in an edit script.
data Edit a
    -- | An item present in the new version, but not the old.
    = Insert a
    -- | An item present in the old version, but not the new.
    | Delete a
    -- | An item present in both versions.
    | Both (Delta a)
    deriving (Eq, Read, Show, Functor, Foldable, Traversable)

-- | Convert a pair of lists into an edit script that tells corresponding
-- differences between old and new versions. It is possible to recover
-- both values from the resulting script.
diff :: Iso' (Delta [a]) [Edit a]
diff = iso diff' patch
  where
    diff' (Delta a b) = go a b
      where
        go []     ys     = Insert <$> ys
        go xs     []     = Delete <$> xs
        go (x:xs) (y:ys) = Both (Delta x y) : go xs ys

    patch = Prelude.foldr go (Delta [] [])
      where
        go (Insert y) (Delta xs ys) = Delta xs (y:ys)
        go (Delete x) (Delta xs ys) = Delta (x:xs) ys
        go (Both xy)  d             = (:) <$> xy <*> d
