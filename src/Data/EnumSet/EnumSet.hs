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
module Data.EnumSet.EnumSet (
  EnumSet(..),
  size, null,
  empty, singleton, full,
  fromList, toList,
  insert, delete, member, notMember,
  findMin, findMax,
  difference, intersection, union, unions,
  (\\),
  partition, split,
  --
  safeToEnum
  ) where

import Prelude hiding(null)
import Data.Maybe (mapMaybe)
import qualified Data.IntSet as S
import GHC.Exts (IsList (..))

import qualified Data.Binary as BIN

-- ---------------------------------------------------------------------------
-- | Type

newtype EnumSet e = EnumSet { toSet :: S.IntSet }
  deriving (Semigroup, Monoid, BIN.Binary)


-- ---------------------------------------------------------------------------
-- | Instances

safeToEnum :: forall e. (Bounded e, Enum e) => Int -> Maybe e
safeToEnum n | e0 <= n && e1 >= n = Just $ toEnum n
             | otherwise = Nothing
  where e0 = fromEnum (minBound :: e)
        e1 = fromEnum (maxBound :: e)


instance (Bounded e, Enum e) => IsList (EnumSet e) where
  type Item (EnumSet e) = e
  toList = mapMaybe safeToEnum . toList . toSet
  {-# INLINE toList #-}
  fromList = EnumSet . S.fromList . map fromEnum
  {-# INLINE fromList #-}



-- ---------------------------------------------------------------------------
-- | Utility

size :: EnumSet e -> Int
size = S.size . toSet
{-# INLINE size #-}

null :: EnumSet e -> Bool
null = S.null . toSet
{-# INLINE null #-}

-- ---------------------------------------------------------------------------
-- | Construction

empty :: EnumSet e
empty = EnumSet S.empty
{-# INLINE empty #-}

singleton :: Enum e => e -> EnumSet e
singleton = EnumSet . S.singleton . fromEnum
{-# INLINE singleton #-}

full :: forall e. (Bounded e, Enum e) => EnumSet e
full = EnumSet $ fromList $ enumFromTo minBound maxBound
{-# INLINE full #-}

-- ---------------------------------------------------------------------------
-- | Insert / Delete

insert :: Enum e => e -> EnumSet e -> EnumSet e
insert x = EnumSet . S.insert (fromEnum x) . toSet
{-# INLINE insert #-}

delete :: Enum e => e -> EnumSet e -> EnumSet e
delete x = EnumSet . S.delete (fromEnum x) . toSet
{-# INLINE delete #-}

-- ---------------------------------------------------------------------------
-- | Tests

member :: Enum e => e -> EnumSet e -> Bool
member x = S.member (fromEnum x) . toSet
{-# INLINE member #-}

notMember :: Enum e => e -> EnumSet e -> Bool
notMember x = not . member x
{-# iNLINE notMember #-}

-- ---------------------------------------------------------------------------
-- | Lookup

findMin :: Enum e => EnumSet e -> e -- partial
findMin = toEnum . S.findMin . toSet

findMax :: Enum e => EnumSet e -> e -- partial
findMax = toEnum . S.findMax . toSet

-- lookupMin
-- lookupMax

-- ---------------------------------------------------------------------------
-- | Set operations

difference :: EnumSet e -> EnumSet e -> EnumSet e
difference (EnumSet s1) (EnumSet s2) = EnumSet (S.difference s1 s2)

intersection :: EnumSet e -> EnumSet e -> EnumSet e
intersection (EnumSet s1) (EnumSet s2) = EnumSet (S.intersection s1 s2)

(\\) :: EnumSet e -> EnumSet e -> EnumSet e
(\\) = intersection

union :: EnumSet e -> EnumSet e -> EnumSet e
union (EnumSet s1) (EnumSet s2) = EnumSet (S.union s1 s2)

unions :: [EnumSet e] -> EnumSet e
unions = EnumSet . S.unions . map toSet

-- partial
partition :: Enum e => (e -> Bool) -> EnumSet e -> (EnumSet e, EnumSet e)
partition f (EnumSet s0) = let (s1, s2) = S.partition (f . toEnum) s0
                            in (EnumSet s1, EnumSet s2)

split :: (Enum e, Ord e) => e -> EnumSet e -> (EnumSet e, EnumSet e)
split x (EnumSet s0) = let (s1, s2) = S.split (fromEnum x) s0
                        in (EnumSet s1, EnumSet s2)


-- vim: ts=8 sw=2 expandtab :

