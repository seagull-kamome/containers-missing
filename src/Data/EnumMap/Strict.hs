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
module Data.EnumMap.Strict (
  ) where

import Data.Functor.Classes (Eq1, Ord1, Show1, Read1)
import qualified Data.List as L (map)
import Data.Data (Data)
import qualified Data.IntMap.Strict as M
import Control.Applicative (Applicative)
import Control.Monad
import GHC.Exts (IsList (..))

import Data.EnumSet.EnumSet (safeToEnum)

-- ---------------------------------------------------------------------------
-- | Type

newtype EnumMap e a = EnumMap { toMap :: M.IntMap a }
  deriving (Semigroup, Monoid, Functor, Foldable, Traversable,
            Eq1, Ord1, Read1, Show1, Eq, Data,Ord, Read, Show)

-- ---------------------------------------------------------------------------
-- | Instances

instance (Bounded e, Enum e) => IsList (EnumMap e a) where
  type Item (EnumMap e a) = (e, a)
  fromList = EnumMap . fromList . L.map (\(x, y) -> (toEnum x, y))
  {-# INLINE fromList #-}
  toList = mapMaybe (\(x, y) -> (,y) <$> safeToEnum x ) . toList
  {-# INLINE toList #-}



-- ---------------------------------------------------------------------------
-- | Operators

(!?) :: EnumMap e a -> e -> Maybe a
(!?) x = (M.!?) (fromEnum x)

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

singleton :: Enum e => e -> EnumMap e a
singleton = EnumMap . M.singleton . fromEnum
{-# INLINE singleton #-}


-- ---------------------------------------------------------------------------
-- | Insert / Delete

insert :: Enum e => e -> a -> EnumMap e a -> EnumMap e a
insert k = EnumMap . M.insert (fromENum k) . toMap
{-# INLINE insert #-}

insertWith :: Enum e => (a -> a -> a) -> e -> a -> EnumMap e a -> EnumMap e a
insertWith f k v = EnumMap . M.insertWith f (fromEnum k) v . toMap
{-# INLINE insertWith #-}

insertWithKey :: Enum e => (e -> a -> a -> a) -> e -> a -> EnumMap e a -> EnumMap e a
insertWithKey f k = insertWIth (f k) k
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
update f k = EnumMap . M.update f . toMap
{-# INLINE update #-}

updateWithKey :: Enum e => (e -> a -> Maybe a) -> e -> EnumMap e a -> EnumMap e a
updateWithKey f k = update (f k) k
{-# INLINE updateWithKey #-}

updateLookupWithKey :: Enum e => (e -> a -> Maybe a) -> e -> EnumMap e a -> (Maybe a, EnumMap e a)
updateLookupWithKey f k x = (r, EnumMap x')
  where (r, x') = M.updateLookupWithKey f' (fromEnum k) v $ toMap x
        f' _ = f k
{-# INLINE updateLookupWithKey #-}

alter :: Enum e => (Maybe a -> Maybe a) -> e -> EnumMap e a -> EnumMap e a
alter f k = EnumMap . M.alter f . toMap
{-# INLINE alter #-}

alterF :: (Enum e, Functor f) => (Maybe a -> f (Maybe a)) -> e -> EnumMap e a -> f (EnumMap e a)
alterF f k x = EnumMap <$> M.alterF f (fromEnum k) (toMap x)
{-# INLINE alterF #-}



-- ---------------------------------------------------------------------------
-- | Lookup

member :: Enum e => e -> EnumMap e a -> Bool
member x = M.member (fromEnum e) . toMap
{-# INLINE member #-}

notMember :: Enum e => e -> EnumMap e a -> Bool
notMember k = not . member k
{-# INLINE notMember #-}

lookup :: Enum e => e -> EnumMap e a -> Maybe a
lookup k = M.lookup (fromEnum k)
{-# iNLINE lookup #-}

findWithDefault :: a -> e -> EnumMap e a -> a
findWithDefault x e = fromMaybe a . M.lookup e
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
  where f' e = f (fromJust $ safeToEnum e)  -- partial but usualy safe
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

differenceWith :: (a -> a -> a) -> EnumMap e a -> EnumMap e a
differenceWith f x y = EnumMap $ M.differenceWith f (toMap x) (toMap y)
{-# INLINE differenceWith #-}

differenceWithKey :: (Bounded e, Enum e)
                  => (e -> a -> a -> a) -> EnumMap e a -> EnumMap e a
differenceWithKey f x y = EnumMap $ M.differenceWithKey f' (toMap x) (toMap y)
  where f' e = f (fromJust $ safeToEnum e)
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
  where f' e = f (fromJust $ safeToEnum e)
{-# INLINE intersectionWithKey #-}

-- ---------------------------------------------------------------------------
-- | Universal combinin

mergeWithKey :: (Bounded e, Enum e)
             => (e -> a -> b -> Maybe c)
             -> (EnumMap e a -> EnumMap e c)
             -> (EnumMap e b -> EnumMap e c)
             -> EnumMap e a -> EnumMap e b -> EnumMap e c
mergeWithKey f g h x y = EnumMap $ M.mergeWithKey f' g' h' (toMap x) (toMap y)
  where f' e = f (fromJust $ safeToEnum e)
        g' = toMap . g . EnumMap
        h' = toMap . h . EnumMap
{-# INLINE mergeWithKey #-}

-- ---------------------------------------------------------------------------
-- | Traversal

map :: (a -> b) -> EnumMap e a -> EnumMap e b
map f = EnumMap . M.map f . toMap
{-# INLINE map #-}

mapKithKey :: (Bounded e, Enum e)
           => (e -> a -> b) -> EnumMap e a -> EnumMap e b
mapWithKey

traverseWithKey :: (Bounded e, Enum e, Applicative t)
                => (e -> a -> t b) -> EnumSet e a -> t (EnumSet e b)

mapAccum :: (a -> b -> (a, c)) -> a -> EnumMap e b -> (a, EnumMap e c)
mapAccumWithKey :: (Bounded e, Enum e)
                => (a -> e -> b -> (a, c))
                -> a -> EnumMap e b -> (a, EnumMap e c)

mapKey :: (e -> e) -> EnumMap e a -> EnumMap e a


-- vim: ts=8 sw=2 expandtab :

