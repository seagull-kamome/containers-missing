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
module Data.EnumSet.EnumSmallBitSet (
  EnumBitSet, toBitSet,
  EnumBitSet8, EnumBitSet16, EnumBitSet32, EnumBitSet64,
  EnumSmallBitSet,
  size, null,
  empty, singleton, full,
  fromList, toList,
  insert, delete, member, notMember,
  findMin, findMax,
  difference, intersection, union, unions,
  (\\),
  partition --, split
  ) where

import Prelude hiding(null)
import Data.Word
import Data.Bits (Bits, FiniteBits)
import qualified Data.SmallBitSet as SBS
import GHC.TypeNats (Nat)
import GHC.Exts (IsList (..))

import qualified Data.Binary as BIN

-- ---------------------------------------------------------------------------
-- | Type

newtype EnumBitSet bw e = EnumBitSet { toBitSet :: SBS.BitSet bw }
deriving instance Eq bw => Eq (EnumBitSet bw e)
deriving instance Ord bw => Ord (EnumBitSet bw e)
deriving instance Bits bw => Semigroup (EnumBitSet bw e)
deriving instance (Bits bw, Num bw) => Monoid (EnumBitSet bw e)
deriving instance BIN.Binary bw => BIN.Binary (EnumBitSet bw e)


-- ---------------------------------------------------------------------------
-- | Small bitset

type EnumBitSet8 = EnumBitSet Word8
type EnumBitSet16 = EnumBitSet Word16
type EnumBitSet32 = EnumBitSet Word32
type EnumBitSet64 = EnumBitSet Word64

type family EnumSmallBitSet (w::Nat) :: * -> * where
  EnumSmallBitSet 8 = EnumBitSet Word8
  EnumSmallBitSet 16 = EnumBitSet Word16
  EnumSmallBitSet 32 = EnumBitSet Word32
  EnumSmallBitSet 64 = EnumBitSet Word64

-- ---------------------------------------------------------------------------
-- | Instances

instance (IsList (SBS.BitSet bw), Enum e) => IsList (EnumBitSet bw e) where
  type Item (EnumBitSet bw e) = e
  toList = map toEnum . SBS.toList . toBitSet
  {-# INLINE toList #-}
  fromList = EnumBitSet . SBS.fromList . map fromEnum
  {-# INLINE fromList #-}


instance (FiniteBits bw, Num bw, Enum e, Show [e]) => Show (EnumBitSet bw e) where
  show = show . toList
instance (FiniteBits bw, Num bw, Enum e, Read [e]) => Read (EnumBitSet bw e) where
  readsPrec n str = [(fromList x, str') | (x, str') <- readsPrec n str :: [([e], String)] ]


-- ---------------------------------------------------------------------------
-- | Utility

size :: Bits bw => EnumBitSet bw e -> Int
size = SBS.size . toBitSet
{-# INLINE size #-}

null :: (Bits bw, Num bw) => EnumBitSet bw e -> Bool
null = SBS.null . toBitSet
{-# INLINE null #-}


-- ---------------------------------------------------------------------------
-- | Construction

empty :: Num bw => EnumBitSet bw e
empty = EnumBitSet SBS.empty
{-# INLINE empty #-}

singleton :: (Bits bw, Num bw, Enum e) => e -> EnumBitSet bw e
singleton = EnumBitSet . SBS.singleton . fromEnum
{-# INLINE singleton #-}

full :: (Bits bw, Num bw) => EnumBitSet bw e
full = EnumBitSet SBS.full
{-# INLINE full #-}


-- ---------------------------------------------------------------------------
-- | Insert / Delete

insert :: (Bits bw, Enum e) => e -> EnumBitSet bw e -> EnumBitSet bw e
insert x = EnumBitSet . SBS.insert (fromEnum x) . toBitSet
{-# INLINE insert #-}

delete :: (Bits bw, Enum e) => e -> EnumBitSet bw e -> EnumBitSet bw e
delete x = EnumBitSet . SBS.delete (fromEnum x) . toBitSet
{-# INLINE delete #-}


-- ---------------------------------------------------------------------------
-- | Lookup

member :: (Bits bw, Enum e) => e -> EnumBitSet bw e -> Bool
member x = SBS.member (fromEnum x) . toBitSet
{-# INLINE member #-}

notMember :: (Bits bw, Enum e) => e -> EnumBitSet bw e -> Bool
notMember x = not . member x
{-# iNLINE notMember #-}


findMin :: (FiniteBits bw, Num bw, Enum e) => EnumBitSet bw e -> e -- partial
findMin = toEnum . SBS.findMin . toBitSet

findMax :: (FiniteBits bw, Num bw, Enum e) => EnumBitSet bw e -> e -- partial
findMax = toEnum . SBS.findMax . toBitSet

-- lookupMin
-- lookupMax


-- ---------------------------------------------------------------------------
-- | Set operations

difference :: Bits bw => EnumBitSet bw e -> EnumBitSet bw e -> EnumBitSet bw e
difference (EnumBitSet s1) (EnumBitSet s2) = EnumBitSet (SBS.difference s1 s2)

intersection :: Bits bw
  => EnumBitSet bw e -> EnumBitSet bw e -> EnumBitSet bw e
intersection (EnumBitSet s1) (EnumBitSet s2) = EnumBitSet (SBS.intersection s1 s2)

(\\) :: Bits bw => EnumBitSet bw e -> EnumBitSet bw e -> EnumBitSet bw e
(\\) = intersection

union :: Bits bw => EnumBitSet bw e -> EnumBitSet bw e -> EnumBitSet bw e
union (EnumBitSet s1) (EnumBitSet s2) = EnumBitSet (SBS.union s1 s2)

unions :: (Bits bw, Num bw) => [EnumBitSet bw e] -> EnumBitSet bw e
unions = EnumBitSet . SBS.unions . map toBitSet

partition :: (FiniteBits bw, Num bw, Enum e)
  => (e -> Bool) -> EnumBitSet bw e -> (EnumBitSet bw e, EnumBitSet bw e)
partition f (EnumBitSet s0) = let (s1, s2) = SBS.partition (f . toEnum) s0
                            in (EnumBitSet s1, EnumBitSet s2)

{-
split :: (Enum e, Ord e) => e -> EnumSet e -> (EnumSet e, EnumSet e)
split x (EnumSet s0) = let (s1, s2) = S.split (fromEnum x) s0
                        in (EnumSet s1, EnumSet s2)
-}
