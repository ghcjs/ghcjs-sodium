-- | Mapping keys to multiple values.
module Data.MultiMap
    ( MultiMap
      -- * Construction
    , empty
    , singleton
      -- * Operations
    , null
    , size
    , member
    , lookup
    , (!)
    , insert
    , delete
      -- * Conversion
    , toList
    , fromList
    ) where

import           Prelude             hiding (foldr, lookup, null)

import           Data.Foldable       hiding (toList)
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List
import           Data.Monoid
import           Data.Traversable
import           GHC.Exts            (build)
import           Text.Read

-- | A map from keys to values, where keys may occur more than once.
newtype MultiMap k v = MultiMap { toHashMap :: HashMap k [v] }
    deriving (Eq)

instance (Eq k, Hashable k) => Monoid (MultiMap k v) where
  mempty = empty
  mappend (MultiMap a) (MultiMap b) = MultiMap (HashMap.unionWith (++) a b)

instance Functor (MultiMap k) where
    fmap f = MultiMap . fmap (map f) . toHashMap

instance Foldable (MultiMap k) where
    foldr f z = foldr (flip (List.foldr f)) z . toHashMap

instance Traversable (MultiMap k) where
    traverse f = fmap MultiMap . traverse (traverse f) . toHashMap

instance (Eq k, Hashable k, Read k, Read v) => Read (MultiMap k v) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- readPrec
        return (fromList xs)

    readListPrec = readListPrecDefault

instance (Show k, Show v) => Show (MultiMap k v) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . shows (toList m)

-- | /O(1)/ The empty map.
empty :: MultiMap k v
empty = MultiMap HashMap.empty

-- | /O(1)/ Construct a map with a single element.
singleton :: Hashable k => k -> v -> MultiMap k v
singleton k v = MultiMap (HashMap.singleton k [v])

-- | /O(1)/ Return 'True' if the map is empty.
null :: MultiMap k v -> Bool
null = HashMap.null . toHashMap

-- | /O(n)/ Return the number of elements in the map.
size :: MultiMap k v -> Int
size = HashMap.foldl' (\a vs -> a + length vs) 0 . toHashMap

-- | /O(n)/ Determine if the element is a member of the map.
member :: (Eq k, Hashable k, Eq v) => k -> v -> MultiMap k v -> Bool
member k v m = v `List.elem` lookup k m

-- | /O(log n)/ Find all the values associated with a key.
lookup :: (Eq k, Hashable k) => k -> MultiMap k v -> [v]
lookup k = HashMap.lookupDefault [] k . toHashMap

-- | /O(log n)/ A flipped and infix version of 'lookup'.
(!) :: (Eq k, Hashable k) => MultiMap k v -> k -> [v]
(!) = flip lookup

-- | /O(log n)/ Insert a value into a map.
-- TODO: strictness
insert :: (Eq k, Hashable k) => k -> v -> MultiMap k v -> MultiMap k v
insert k v = MultiMap . HashMap.insertWith (++) k [v] . toHashMap

-- | /O(n)/ Delete a value from the map.
-- TODO: strictness, running time
delete :: (Eq k, Hashable k, Eq v) => k -> v -> MultiMap k v -> MultiMap k v
delete k v m = case lookup k m of
    [] -> m
    vs -> MultiMap $ case List.delete v vs of
        []  -> HashMap.delete k (toHashMap m)
        vs' -> HashMap.insert k vs' (toHashMap m)

-- | /O(n)/ Return a list of the map's elements. The list is produced lazily.
toList :: MultiMap k v -> [(k, v)]
toList m = build $ \c z ->
    HashMap.foldrWithKey (\k vs a -> List.foldr (\v -> c (k, v)) a vs) z
        (toHashMap m)

-- | /O(n*log n)/ Construct a map from the list's elements.
-- TODO: strictness
fromList :: (Eq k, Hashable k) => [(k, v)] -> MultiMap k v
fromList = List.foldl' (flip (uncurry insert)) empty
