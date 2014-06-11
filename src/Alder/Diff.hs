{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Alder.Diff
    ( Diff(..)
    , diff
    ) where


import           Control.Applicative
import           Control.Monad.State
import           Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Text           (Text)

import           Alder.Html.Internal

data Diff
    = Match Id Attributes Attributes Diff Diff
    | Relabel Attributes Attributes Diff Diff
    | Revalue Text Diff
    | Add Node Diff
    | Drop Diff
    | Pass Diff
    | End

instance Show Diff where
    showsPrec _ (Match i _ _ d1 d2)
        = showString "Match "
        . shows i
        . showString " ("
        . shows d1
        . showString ") -> "
        . shows d2

    showsPrec _ (Relabel _ _ d1 d2)
        = showString "Relabel ("
        . shows d1
        . showString ") -> "
        . shows d2

    showsPrec _ (Revalue t d)
        = showString "Revalue "
        . shows t
        . showString " -> "
        . shows d

    showsPrec _ (Add _ d)
        = showString "Add -> "
        . shows d

    showsPrec _ (Drop d)
        = showString "Drop -> "
        . shows d

    showsPrec _ (Pass d)
        = showString "Pass -> "
        . shows d

    showsPrec _ End
        = showString "End"

nodeId :: Node -> Maybe Id
nodeId (Element _ a _) = elementId a
nodeId _               = Nothing

children :: Node -> [Node]
children (Element _ _ cs) = cs
children _                = []

descendants :: Node -> [Node]
descendants n = n : concatMap descendants (children n)

diff :: [Node] -> [Node] -> Diff
diff a b = evalState (diffM a b) index
  where
    index = HashMap.fromList . mapMaybe withId $ concatMap descendants a

    withId e = (\i -> (i, e)) <$> nodeId e

diffM :: [Node] -> [Node] -> State (HashMap Id Node) Diff
diffM a b = do
    index <- get
    go index a b
  where
    go index (x:xs) ys
        | Just i <- nodeId x
        , not (HashMap.member i index)
        = diffM xs ys

    go _ (x:xs) (y:ys)
        | Text t1 <- x
        , Text t2 <- y
        = (if t1 == t2 then Pass else Revalue t2) <$> diffM xs ys

    go index (x:xs) (y:ys)
        | Element t1 a1 cs1 <- x
        , Element t2 a2 cs2 <- y
        , elementId a1 == elementId a2
        , t1 == t2
        = do case elementId a2 of
                 Nothing -> return ()
                 Just i  -> put (HashMap.delete i index)
             Relabel a1 a2 <$> diffM cs1 cs2 <*> diffM xs ys

    go index xs (y:ys)
        | Just i <- nodeId y
        , Just x <- HashMap.lookup i index
        , Element t1 a1 cs1 <- x
        , Element t2 a2 cs2 <- y
        , t1 == t2
        = do put (HashMap.delete i index)
             Match i a1 a2 <$> diffM cs1 cs2 <*> diffM xs ys

    go _ xs     (y:ys) = Add y <$> diffM xs ys
    go _ (_:xs) ys     = Drop  <$> diffM xs ys
    go _ []     []     = return End
