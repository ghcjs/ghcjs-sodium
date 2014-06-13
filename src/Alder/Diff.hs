{-# LANGUAGE PatternGuards #-}
module Alder.Diff
    ( MonadSupply(..)
    , Tagged(..)
    , tag
    , untag
    , Frag
    , TFrag
    , reconcile
    ) where

import           Control.Monad
import           Data.Either
import           Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Tree

import           Alder.Html.Internal
import           Alder.Unique

type Frag = Tree Node
type TFrag = Tree (Tagged Node)

cons :: Monad m => m a -> m [a] -> m [a]
cons = liftM2 (:)

subTrees :: Tree a -> [Tree a]
subTrees n = n : concatMap subTrees (subForest n)

reconcile :: MonadSupply m => [Frag] -> [TFrag] -> m [TFrag]
reconcile new old = matchForest index new old
  where
    index = HashMap.fromList . mapMaybe withElementId $ concatMap subTrees old

    withElementId t = case untag (rootLabel t) of
        Element _ a | Just i <- elementId a -> Just (i, t)
        _                                   -> Nothing

matchForest
    :: MonadSupply m => HashMap Id TFrag -> [Frag] -> [TFrag] -> m [TFrag]
matchForest index new old = go new others
  where
    (keyed, others) = partitionEithers (mapMaybe splitKey old)

    keyIndex = HashMap.fromList keyed

    splitKey t = case untag (rootLabel t) of
        Element _ a | Just _ <- elementId a  -> Nothing
                    | Just k <- elementKey a -> Just (Left (k, t))
        _                                    -> Just (Right t)

    go (x:xs) ys
        | Element _ a <- rootLabel x
        , Just i <- elementId a
        , Just y <- HashMap.lookup i index
        = matchNode index x y `cons` go xs ys

    go (x:xs) ys
        | Element _ a <- rootLabel x
        , Just k <- elementKey a
        , Just y <- HashMap.lookup k keyIndex
        = matchNode index x y `cons` go xs ys

    go (x:xs) (y:ys)
        = matchNode index x y `cons` go xs ys

    go (x:xs) []
        = create x `cons` go xs []

    go [] _
        = return []

matchNode
    :: MonadSupply m => HashMap Id TFrag -> Frag -> TFrag -> m TFrag
matchNode index x y
    | Element t1 _ <- rootLabel x
    , Element t2 _ <- untag (rootLabel y)
    , t1 == t2
    = transfer index x y

    | Text _ <- rootLabel x
    , Text _ <- untag (rootLabel y)
    = transfer index x y

    | otherwise = create x

transfer
    :: MonadSupply m => HashMap Id TFrag -> Frag -> TFrag -> m TFrag
transfer index (Node a cs1) (Node (Tagged i _) cs2) =
    Node (Tagged i a) `liftM` matchForest index cs1 cs2

create :: MonadSupply m => Frag -> m TFrag
create (Node a cs) = Node `liftM` tag a `ap` mapM create cs
