{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
-- | Lenses for diffing data types.
module FRP.GHCJS.Diff
    ( -- * Deltas
      Delta(..)
    , slice
    , match
    , matchOn
      -- * Diffs
    , Diff(..)
    , Edit(..)
    , recover
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Foldable
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

-- | Two values, representing the old and new versions of data type. This is
-- also the diagonal functor in @Hask@, the category of Haskell types.
data Delta a = Delta a a
    deriving (Eq, Read, Show, Functor, Foldable, Traversable)

instance Applicative Delta where
    pure a = Delta a a
    Delta f g <*> Delta x y = Delta (f x) (g y)

-- | Focus on and compare one part of the structure.
slice :: ALens s t a b -> Lens (Delta s) (Delta t) (Delta a) (Delta b)
slice l = lens (fmap (^# l)) (\s b -> storing l <$> b <*> s)

-- | A 'Prism' that matches only if both the old and new versions of the type
-- match.
match :: APrism' s a -> Prism' (Delta s) (Delta a)
match = below

-- | A 'Fold' that matches only if both the old and new versions yield the
-- same value when the getter is applied.
matchOn :: Eq a => (s -> a) -> Fold (Delta s) (Delta s)
matchOn f = filtered $ \(Delta x y) -> f x == f y

-- | Types that can be deconstructed into edit scripts.
class Diff a b | a -> b where
    -- | Convert a pair of values into an edit script that tells corresponding
    -- differences between old and new versions. It is possible to recover
    -- both values from the resulting script.
    diff :: Iso' (Delta a) [Edit b]

-- | A single edit in an edit script.
data Edit a
    -- | An item present in the new version, but not the old.
    = Insert a
    -- | An item present in the old version, but no the new.
    | Delete a
    -- | An item present in both versions.
    | Both (Delta a)
    deriving (Functor)

-- | Recover the two values from an edit script using a fold.
recover :: (b -> a -> a) -> a -> [Edit b] -> Delta a
recover f z = Prelude.foldr go (Delta z z)
  where
    go (Insert b) (Delta x y) = Delta x (f b y)
    go (Delete a) (Delta x y) = Delta (f a x) y
    go (Both ab)  d           = f <$> ab <*> d

instance Diff [a] a where
    diff = iso diff' (recover (:) [])
      where
        diff' (Delta a b) = go a b
          where
            go []     ys     = Insert <$> ys
            go xs     []     = Delete <$> xs
            go (x:xs) (y:ys) = Both (Delta x y) : go xs ys

instance (Eq k, Hashable k) => Diff (HashMap k v) (k, v) where
    diff = iso diff' recover'
      where
        diff' (Delta m1 m2) =
            values (\(k, d) -> Both ((,) k <$> d))
                (HashMap.intersectionWith Delta m1 m2) ++
            values Insert (HashMap.difference m2 m1) ++
            values Delete (HashMap.difference m1 m2)

        values f m = f <$> HashMap.toList m

        recover' = recover (uncurry HashMap.insert) HashMap.empty
