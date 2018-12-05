{-
Copyright Hattori, Hiroki (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Hattori, Hiroki nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.EnumMap.Strict (
  EnumMap(..),
  (!?), (\\),
  size, null,
  empty, singleton,
  insert, insertWith, insertWithKey, insertLookupWithKey,
  delete,
  adjust, adjustWithKey,
  update, updateWithKey, updateLookupWithKey,
  alter, alterF,
  member, notMember, lookup, findWithDefault,
  union, unionWith, unionWithKey, unions, unionsWith,
  difference, differenceWith, differenceWithKey,
  intersection, intersectionWith, intersectionWithKey,
  mergeWithKey,
  map, mapWithKey, traverseWithKey, mapAccum, mapAccumWithKey,
  mapKeys, mapKeysWith, mapKeysMonotonic,
  foldrWithKey, foldlWithKey, foldMapWithKey, foldrWithKey', foldlWithKey',
  elems, keys, assocs, keyset,
  filter, filterWithKey, restrictKeys, withoutKeys,
  partition, partitionWithKey,
  mapMaybe, mapMaybeWithKey, mapEither, mapEitherWithKey,
  isSubmapOf, isSubmapOfBy, isProperSubmapOf, isProperSubmapOfBy
  ) where

import Prelude hiding (map, lookup, null, filter)

import qualified Data.Maybe as MAYBE
import Data.Functor.Classes (Eq1, Ord1, Show1, Read1)
import qualified Data.List as L (map)
-- import Data.Data (Data)
import qualified Data.IntMap.Strict as M
import Control.Applicative (Applicative)
-- import Control.Monad
import GHC.Exts (IsList (..))

import qualified Data.EnumSet.EnumSet as ESet

-- ---------------------------------------------------------------------------
-- | Type

newtype EnumMap e a = EnumMap { toMap :: M.IntMap a }
  deriving (Semigroup, Monoid, Functor, Foldable)

-- ---------------------------------------------------------------------------
-- | Instances

instance (Bounded e, Enum e) => IsList (EnumMap e a) where
  type Item (EnumMap e a) = (e, a)
  fromList = EnumMap . fromList . L.map (\(x, y) -> (fromEnum x, y))
  {-# INLINE fromList #-}
  toList = MAYBE.mapMaybe (\(x, y) -> (,y) <$> ESet.safeToEnum x ) . toList . toMap
  {-# INLINE toList #-}


instance (Bounded e, Enum e) => Traversable (EnumMap e) where
  traverse f = traverseWithKey (const f)
  {-# INLINE traverse #-}

instance (Bounded e, Enum e, Show e, Show a) => Show (EnumMap e a) where
  show x = "fromList" ++ show (assocs x)

deriving instance (Eq e, Eq a) => Eq (EnumMap e a)
deriving instance Eq e => Eq1 (EnumMap e)
deriving instance Ord e => Ord1 (EnumMap e)
--deriving instance Read e => Read1 (EnumMap e)
--deriving instance Show e => Show1 (EnumMap e)



-- ---------------------------------------------------------------------------
-- | Operators

(!?) :: Enum e => EnumMap e a -> e -> Maybe a
(!?) x k = (M.!?) (toMap x) (fromEnum k)

(\\) :: EnumMap e a -> EnumMap e a -> EnumMap e a
(\\) = difference


-- ---------------------------------------------------------------------------
-- | Utility

size :: EnumMap e a -> Int
size = M.size . toMap
{-# INLINE size #-}

null :: EnumMap e a -> Bool
null = M.null . toMap
{-# INLINE null #-}


-- ---------------------------------------------------------------------------
-- | Construction

empty :: EnumMap e a
empty = EnumMap M.empty
{-# INLINE empty #-}

singleton :: Enum e => e -> a -> EnumMap e a
singleton k = EnumMap . M.singleton (fromEnum k)
{-# INLINE singleton #-}


-- ---------------------------------------------------------------------------
-- | Insert / Delete

insert :: Enum e => e -> a -> EnumMap e a -> EnumMap e a
insert k v = EnumMap . M.insert (fromEnum k) v . toMap
{-# INLINE insert #-}

insertWith :: Enum e => (a -> a -> a) -> e -> a -> EnumMap e a -> EnumMap e a
insertWith f k v = EnumMap . M.insertWith f (fromEnum k) v . toMap
{-# INLINE insertWith #-}

insertWithKey :: Enum e => (e -> a -> a -> a) -> e -> a -> EnumMap e a -> EnumMap e a
insertWithKey f k = insertWith (f k) k
{-# INLINE insertWithKey #-}

insertLookupWithKey :: Enum e => (e -> a -> a -> a) -> e -> a -> EnumMap e a -> (Maybe a, EnumMap e a)
insertLookupWithKey f k v x = (r, EnumMap x')
  where (r, x') = M.insertLookupWithKey f' (fromEnum k) v $ toMap x
        f' _ = f k
{-# INLINE insertLookupWithKey #-}

delete :: Enum e => e -> EnumMap e a -> EnumMap e a
delete k = EnumMap . M.delete (fromEnum k) . toMap
{-# INLINE delete #-}

adjust:: Enum e => (a -> a) -> e -> EnumMap e a -> EnumMap e a
adjust f k = EnumMap . M.adjust f (fromEnum k) . toMap
{-# INLINE adjust #-}

adjustWithKey :: Enum e => (e -> a -> a) -> e -> EnumMap e a -> EnumMap e a
adjustWithKey f k = adjust (f k) k
{-# INLINE adjustWithKey #-}

update :: Enum e => (a -> Maybe a) -> e -> EnumMap e a -> EnumMap e a
update f k = EnumMap . M.update f (fromEnum k) . toMap
{-# INLINE update #-}

updateWithKey :: Enum e => (e -> a -> Maybe a) -> e -> EnumMap e a -> EnumMap e a
updateWithKey f k = update (f k) k
{-# INLINE updateWithKey #-}

updateLookupWithKey :: Enum e => (e -> a -> Maybe a) -> e -> EnumMap e a -> (Maybe a, EnumMap e a)
updateLookupWithKey f k x = (l, EnumMap r)
  where (l, r) = M.updateLookupWithKey f' (fromEnum k) $ toMap x
        f' _ = f k
{-# INLINE updateLookupWithKey #-}

alter :: Enum e => (Maybe a -> Maybe a) -> e -> EnumMap e a -> EnumMap e a
alter f k = EnumMap . M.alter f (fromEnum k) . toMap
{-# INLINE alter #-}

alterF :: (Enum e, Functor f) => (Maybe a -> f (Maybe a)) -> e -> EnumMap e a -> f (EnumMap e a)
alterF f k x = EnumMap <$> M.alterF f (fromEnum k) (toMap x)
{-# INLINE alterF #-}



-- ---------------------------------------------------------------------------
-- | Lookup

member :: Enum e => e -> EnumMap e a -> Bool
member k = M.member (fromEnum k) . toMap
{-# INLINE member #-}

notMember :: Enum e => e -> EnumMap e a -> Bool
notMember k = not . member k
{-# INLINE notMember #-}

lookup :: Enum e => e -> EnumMap e a -> Maybe a
lookup k = M.lookup (fromEnum k) . toMap
{-# iNLINE lookup #-}

findWithDefault :: Enum e => a -> e -> EnumMap e a -> a
findWithDefault dft k = M.findWithDefault dft (fromEnum k) . toMap
{-# iNLINE findWithDefault #-}

-- lookupLT
-- lookupGT
-- lookupLE
-- lookupGE



-- ---------------------------------------------------------------------------
-- | Combine


union :: EnumMap e a -> EnumMap e a -> EnumMap e a
union x y = EnumMap $ M.union (toMap x) (toMap y)
{-# INLINE union #-}

unionWith :: (a -> a-> a) -> EnumMap e a -> EnumMap e a -> EnumMap e a
unionWith f x y = EnumMap $ M.unionWith f (toMap x) (toMap y)
{-# iNLINE unionWith #-}

unionWithKey :: (Bounded e, Enum e)
             => (e -> a -> a -> a)
             -> EnumMap e a -> EnumMap e a -> EnumMap e a
unionWithKey f x y = EnumMap $ M.unionWithKey f' (toMap x) (toMap y)
  where f' e = f (toEnum e)
{-# INLINE unionWithKey #-}

unions :: [EnumMap e a] -> EnumMap e a
unions = EnumMap . M.unions . L.map toMap
{-# INLINE unions #-}

unionsWith :: (a -> a -> a) -> [EnumMap e a] -> EnumMap e a
unionsWith f = EnumMap . M.unionsWith f . L.map toMap
{-# INLINE unionsWith #-}



-- ---------------------------------------------------------------------------
-- | Difference

difference :: EnumMap e a -> EnumMap e a -> EnumMap e a
difference x y = EnumMap $ M.difference (toMap x) (toMap y)
{-# INLINE difference #-}

differenceWith :: (a -> b -> Maybe a)
               -> EnumMap e a -> EnumMap e b -> EnumMap e a
differenceWith f x y = EnumMap $ M.differenceWith f (toMap x) (toMap y)
{-# INLINE differenceWith #-}

differenceWithKey :: (Bounded e, Enum e)
                  => (e -> a -> b -> Maybe a)
                  -> EnumMap e a -> EnumMap e b -> EnumMap e a
differenceWithKey f x y = EnumMap $ M.differenceWithKey f' (toMap x) (toMap y)
  where f' e a0 b0 = MAYBE.maybe Nothing (\e' -> f e' a0 b0) $ ESet.safeToEnum e
{-# INLINE differenceWithKey #-}


-- ---------------------------------------------------------------------------
-- | Intersection

intersection :: EnumMap e a -> EnumMap e a -> EnumMap e a
intersection x y = EnumMap $ M.intersection (toMap x) (toMap y)
{-# INLINE intersection #-}

intersectionWith :: (a -> a -> a) -> EnumMap e a -> EnumMap e a -> EnumMap e a
intersectionWith f x y = EnumMap $ M.intersectionWith f (toMap x) (toMap y)
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Bounded e, Enum e)
                    => (e -> a -> a -> a)
                    -> EnumMap e a -> EnumMap e a -> EnumMap e a
intersectionWithKey f x y = EnumMap $ M.intersectionWithKey f' (toMap x) (toMap y)
  where f' e = f (toEnum e)
{-# INLINE intersectionWithKey #-}

-- ---------------------------------------------------------------------------
-- | Universal combinin

mergeWithKey :: (Bounded e, Enum e)
             => (e -> a -> b -> Maybe c)
             -> (EnumMap e a -> EnumMap e c)
             -> (EnumMap e b -> EnumMap e c)
             -> EnumMap e a -> EnumMap e b -> EnumMap e c
mergeWithKey f g h x y = EnumMap $ M.mergeWithKey f' g' h' (toMap x) (toMap y)
  where f' e = f (toEnum e)
        g' = toMap . g . EnumMap
        h' = toMap . h . EnumMap
{-# INLINE mergeWithKey #-}

-- ---------------------------------------------------------------------------
-- | Traversal

map :: (a -> b) -> EnumMap e a -> EnumMap e b
map f = EnumMap . M.map f . toMap
{-# INLINE map #-}

mapWithKey :: (Bounded e, Enum e)
           => (e -> a -> b) -> EnumMap e a -> EnumMap e b
mapWithKey f = EnumMap . M.mapWithKey f' . toMap
  where f' k =  f (toEnum k)
{-# INLINE mapWithKey #-}

traverseWithKey :: (Bounded e, Enum e, Applicative t)
                => (e -> a -> t b) -> EnumMap e a -> t (EnumMap e b)
traverseWithKey f x = EnumMap <$> M.traverseWithKey f' (toMap x)
  where f' k = f (toEnum k)
{-# INLINE traverseWithKey #-}

mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap e b -> (a, EnumMap e c)
mapAccum f acc x = (r, EnumMap x')
  where (r, x') = M.mapAccum f acc $ toMap x
{-# INLINE mapAccum #-}

mapAccumWithKey :: (Bounded e, Enum e)
                => (a -> e -> b -> (a, c))
                -> a -> EnumMap e b -> (a, EnumMap e c)
mapAccumWithKey f acc x = (l, EnumMap r)
  where (l, r) = M.mapAccumWithKey f' acc (toMap x)
        f' acc' = f acc' . toEnum
{-# INLINE mapAccumWithKey #-}

mapKeys :: (Bounded e0, Enum e0, Enum e1)
       => (e0 -> e1) -> EnumMap e0 a -> EnumMap e1 a
mapKeys f = EnumMap . M.mapKeys f' . toMap
  where f' = fromEnum . f . toEnum
{-# INLINE mapKeys #-}

mapKeysWith :: (Bounded e0, Enum e0, Enum e1)
            => (a -> a -> a) -> (e0 -> e1) -> EnumMap e0 a -> EnumMap e1 a
mapKeysWith f g = EnumMap . M.mapKeysWith f g' . toMap
  where g' = fromEnum . g . toEnum
{-# INLINE mapKeysWith #-}

mapKeysMonotonic :: (Bounded e0, Enum e0, Enum e1)
                => (e0 -> e1) -> EnumMap e0 a -> EnumMap e1 a
mapKeysMonotonic f = EnumMap . M.mapKeysMonotonic f' . toMap
  where f' = fromEnum . f . toEnum
{-# INLINE mapKeysMonotonic #-}



-- ---------------------------------------------------------------------------
-- | Folds

foldrWithKey :: (Bounded e, Enum e)
             => (e -> a -> b -> b) -> b -> EnumMap e a -> b
foldrWithKey f acc = M.foldrWithKey f' acc . toMap
  where f' k elm acc' = MAYBE.maybe acc' (\k' -> f k' elm acc') $ ESet.safeToEnum k
{-# INLINE foldrWithKey #-}

foldlWithKey :: (Bounded e, Enum e)
             => (a -> e -> b -> a) -> a -> EnumMap e b -> a
foldlWithKey f acc = M.foldlWithKey f' acc . toMap
  where f' acc' k v = MAYBE.maybe acc' (\k' -> f acc' k' v) (ESet.safeToEnum k)
{-# INLINE foldlWithKey #-}

foldMapWithKey :: (Bounded e, Enum e, Monoid b)
               => (e -> a -> b) -> EnumMap e a -> b
foldMapWithKey f = M.foldMapWithKey f' . toMap
  where f' k v = MAYBE.maybe mempty (`f` v) (ESet.safeToEnum k)
{-# INLINE foldMapWithKey #-}

foldrWithKey' :: (Bounded e, Enum e)
             => (e -> a -> b -> b) -> b -> EnumMap e a -> b
foldrWithKey' f acc = M.foldrWithKey f' acc . toMap
  where f' k v acc' = MAYBE.maybe acc' (\k' -> f k' v acc') (ESet.safeToEnum k)
{-# INLINE foldrWithKey' #-}

foldlWithKey' :: (Bounded e, Enum e)
             => (a -> e -> b -> a) -> a -> EnumMap e b -> a
foldlWithKey' f acc = M.foldlWithKey' f' acc . toMap
  where f' acc' k v = MAYBE.maybe acc' (\k' -> f acc' k' v) (ESet.safeToEnum k)
{-# INLINE foldlWithKey' #-}


-- ---------------------------------------------------------------------------
-- | Conversion

elems :: EnumMap e a -> [a]
elems = M.elems . toMap
{-# INLINE elems #-}

keys :: (Bounded e, Enum e) => EnumMap e a -> [e]
keys = MAYBE.mapMaybe ESet.safeToEnum . M.keys . toMap
{-# INLINE keys #-}

assocs :: (Bounded e, Enum e) => EnumMap e a -> [(e, a)]
assocs = toList
{-# INLINE assocs #-}

keyset :: EnumMap e a -> ESet.EnumSet e
keyset = ESet.EnumSet . M.keysSet . toMap
{-# INLINE keyset #-}


-- ---------------------------------------------------------------------------
-- | Filter

filter :: (a -> Bool) -> EnumMap e a -> EnumMap e a
filter f = EnumMap . M.filter f . toMap
{-# INLINE filter #-}

filterWithKey :: (Bounded e, Enum e)
              => (e -> a -> Bool) -> EnumMap e a -> EnumMap e a
filterWithKey f = EnumMap . M.filterWithKey f' . toMap
  where f' k = f (toEnum k)
{-# INLINE filterWithKey #-}

restrictKeys :: EnumMap e a -> ESet.EnumSet e -> EnumMap e a
restrictKeys em es = EnumMap $ M.restrictKeys (toMap em) (ESet.toSet es)
{-# INLINE restrictKeys #-}

withoutKeys :: EnumMap e a -> ESet.EnumSet e -> EnumMap e a
withoutKeys em es = EnumMap $ M.withoutKeys (toMap em) (ESet.toSet es)
{-# INLINE withoutKeys #-}

partition :: (a -> Bool) -> EnumMap e a -> (EnumMap e a, EnumMap e a)
partition f x = (EnumMap l, EnumMap r)
  where (l, r) = M.partition f (toMap x)
{-# INLINE partition #-}

partitionWithKey :: (Bounded e, Enum e)
                 => (e -> a -> Bool)
                 -> EnumMap e a -> (EnumMap e a, EnumMap e a)
partitionWithKey f x = (EnumMap l, EnumMap r)
  where (l, r) = M.partitionWithKey f' (toMap x)
        f' k = f (toEnum k)
{-# INLINE partitionWithKey #-}

mapMaybe :: (a -> Maybe b) -> EnumMap e a -> EnumMap e b
mapMaybe f = EnumMap . M.mapMaybe f . toMap
{-# INLINE mapMaybe #-}

mapMaybeWithKey :: (Bounded e, Enum e)
                => (e -> a -> Maybe b) -> EnumMap e a -> EnumMap e b
mapMaybeWithKey f = EnumMap . M.mapMaybeWithKey f' . toMap
  where f' k = f (toEnum k)
{-# INLINE mapMaybeWithKey #-}

mapEither :: (a -> Either b c) -> EnumMap e a -> (EnumMap e b, EnumMap e c)
mapEither f x = (EnumMap l, EnumMap r)
  where (l, r) = M.mapEither f (toMap x)
{-# INLINE mapEither #-}

mapEitherWithKey :: (Bounded e, Enum e)
                 => (e -> a -> Either b c)
                 -> EnumMap e a -> (EnumMap e b, EnumMap e c)
mapEitherWithKey f x = (EnumMap l, EnumMap r)
  where (l, r) = M.mapEitherWithKey f' (toMap x)
        f' k = f (toEnum k)
{-# INLINE mapEitherWithKey #-}

-- split :: Enum e => e -> EnumMap e a -> (EnumMap e a, EnumMap a)
-- splitLookup
-- splitRoot


-- ---------------------------------------------------------------------------
-- | Submap

isSubmapOf :: Eq a => EnumMap e a -> EnumMap e a -> Bool
isSubmapOf x y = M.isSubmapOf (toMap x) (toMap y)
{-# INLINE isSubmapOf #-}

isSubmapOfBy :: (a -> b -> Bool) -> EnumMap e a -> EnumMap e b -> Bool
isSubmapOfBy f x y = M.isSubmapOfBy f (toMap x) (toMap y)
{-# INLINE isSubmapOfBy #-}

isProperSubmapOf :: Eq a => EnumMap e a -> EnumMap e a -> Bool
isProperSubmapOf x y = M.isProperSubmapOf (toMap x) (toMap y)
{-# INLINE isProperSubmapOf #-}

isProperSubmapOfBy :: (a -> b -> Bool) -> EnumMap e a -> EnumMap e b -> Bool
isProperSubmapOfBy f x y = M.isProperSubmapOfBy f (toMap x) (toMap y)
{-# INLINE isProperSubmapOfBy #-}


-- ---------------------------------------------------------------------------
-- | Min/Max

-- lookupMin
-- lookupMax
-- findMin
-- findMax
-- deleteMin
-- deleteMax
-- deleteFindMin
-- deleteFindMax
-- updateMin
-- updateMax
-- updateMinWithKey
-- minView
-- maxView
-- minViewWithKey
-- maxViewWithKey

-- vim: ts=8 sw=2 expandtab :

