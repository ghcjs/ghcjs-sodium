{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash         #-}
module Alder.Diff
    ( Diff(..)
    , diffForests
    ) where

import           Prelude                   hiding (drop)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.HashMap.Strict       as HashMap
import           Data.Maybe
import           Data.Text                 (Text)

import           Alder.Html.Internal

type Id = Text

data Diff
    = Skip Diff
    | Revalue Text Diff
    | Relabel Attributes Attributes Diff Diff
    | Match !Id Attributes Attributes Diff Diff
    | Add Node Diff
    | Drop Diff
    | End

instance Show Diff where
    showsPrec _ (Skip d) = showString "Skip -> " . shows d

    showsPrec _ (Revalue t d) =
        showString "Revalue " .
        shows t .
        showString " -> " .
        shows d

    showsPrec _ (Relabel _ _ d1 d2) =
        showString "Relabel (" .
        shows d1 .
        showString ") -> " .
        shows d2

    showsPrec _ (Match i _ _ d1 d2) =
        showString "Match " .
        shows i .
        showString " (" .
        shows d1 .
        showString ") -> " .
        shows d2

    showsPrec _ (Add _ d) = showString "Add -> " . shows d

    showsPrec _ (Drop d) = showString "Drop -> " . shows d

    showsPrec _ End = showString "End"

getId :: Node -> Maybe Id
getId (Element _ as _) = case HashMap.lookup "id" (attributeValues as) of
    Just (Token i) -> Just i
    _              -> Nothing
getId _ = Nothing

children :: Node -> [Node]
children (Element _ _ cs) = cs
children _                = []

descendants :: Node -> [Node]
descendants n = n : concatMap descendants (children n)

type M = MaybeT (State (HashMap Id Node))

diffForests :: [Node] -> [Node] -> Diff
diffForests xs ys = case evalState (runMaybeT (diff xs ys)) index of
    Nothing -> error "no diff"
    Just d  -> d
  where
    index = HashMap.fromList . mapMaybe toId $ concatMap descendants ys

    toId e = (\i -> (i, e)) <$> getId e

diff :: [Node] -> [Node] -> M Diff
diff xs ys = msum
    [ skip xs ys
    , revalue xs ys
    , relabel xs ys
    , match xs ys
    , add xs ys
    , drop xs ys
    , return End
    ]

skip :: [Node] -> [Node] -> M Diff
skip xs ys = do
    index  <- get
    x:xs'  <- return xs
    Just i <- return $ getId x
    guard (not $ HashMap.member i index)
    Skip <$> diff xs' ys

revalue :: [Node] -> [Node] -> M Diff
revalue xs ys = do
    Text _ : xs' <- return xs
    Text t : ys' <- return ys

    Revalue t <$> diff xs' ys'

relabel :: [Node] -> [Node] -> M Diff
relabel xs ys = do
    x:xs' <- return xs
    y:ys' <- return ys
    guard (getId x == getId y)

    Element t1 a1 cs1 <- return x
    Element t2 a2 cs2 <- return y
    guard (t1 == t2)

    Relabel a1 a2 <$> diff cs1 cs2 <*> diff xs' ys'

match :: [Node] -> [Node] -> M Diff
match xs ys = do
    index  <- get
    y:ys'  <- return ys
    Just i <- return $ getId y
    Just x <- return $ HashMap.lookup i index

    Element t1 a1 cs1 <- return x
    Element t2 a2 cs2 <- return y
    guard (t1 == t2)

    put (HashMap.delete i index)
    Match i a1 a2 <$> diff cs1 cs2 <*> diff xs ys'

add :: [Node] -> [Node] -> M Diff
add xs ys = do
    y:ys' <- return ys
    Add y <$> diff xs ys'

drop :: [Node] -> [Node] -> M Diff
drop xs ys = do
    _:xs' <- return xs
    Drop <$> diff xs' ys
