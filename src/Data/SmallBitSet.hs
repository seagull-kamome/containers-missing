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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.SmallBitSet(
  BitSet, toBits, BitSet8, BitSet16, BitSet32, BitSet64, SmallBitSet,
  size, empty, singleton, full, null,
  fromList, toList,
  insert, delete,
  member, notMember, findMin, findMax,
  difference, intersection, union, unions,
  isHabitant, isHabitant',
  habitantRange, habitantRange',
  (\\),
  partition
  ) where

import Prelude hiding(null)
import Data.Word
import Data.Bits (Bits, popCount, (.|.), (.&.),
  complement, setBit, clearBit, testBit, FiniteBits, finiteBitSize)
import Data.Proxy (Proxy(Proxy))
import Data.Ix (inRange, range)
import GHC.TypeNats (Nat)
-- import GHC.Prim
import GHC.Exts (IsList(..))

import qualified Data.Binary as BIN
-- ---------------------------------------------------------------------------

newtype BitSet bw = BitSet { toBits:: bw }
deriving instance Eq bw => Eq (BitSet bw)
deriving instance Ord bw => Ord (BitSet bw)
--deriving instance Show bw => Show (BitSet bw)
--deriving instance Read bw => Read (BitSet bw)
deriving instance BIN.Binary bw => BIN.Binary (BitSet bw)


-- ---------------------------------------------------------------------------
-- | Small bitset

type BitSet8 = BitSet Word8
type BitSet16 = BitSet Word16
type BitSet32 = BitSet Word32
type BitSet64 = BitSet Word64

-- type BitSet# = BitSet Word#

type family SmallBitSet (w::Nat) :: * where
  SmallBitSet 8 = BitSet Word8
  SmallBitSet 16 = BitSet Word16
  SmallBitSet 32 = BitSet Word32
  SmallBitSet 64 = BitSet Word64


-- ---------------------------------------------------------------------------
-- | Instances

instance Bits bw => Semigroup (BitSet bw) where
  (BitSet w0) <> (BitSet w1) = BitSet (w0 .|. w1)
  {-# INLINE (<>) #-}
instance (Bits bw, Num bw) => Monoid (BitSet bw) where
  mempty = BitSet 0
  {-# INLINE mempty #-}

instance (FiniteBits bw, Num bw) => IsList (BitSet bw) where
  type Item (BitSet bw) = Int
  fromList = BitSet . foldr (flip setBit) (0 :: bw)
  {-# INLINE fromList #-}
  toList x = filter (`member` x) $ range $ habitantRange x
  {-# INLINE toList #-}

instance (FiniteBits bw, Num bw) => Show (BitSet bw) where
  show xs = "fromList " ++ show (toList xs)
  {-# INLINE show #-}

instance (FiniteBits bw, Num bw) => Read (BitSet bw) where
  readsPrec n str = [ (fromList x, str') | (x, str') <- readsPrec n str]
  {-# INLINE readsPrec #-}


-- ---------------------------------------------------------------------------
-- | Utilities

-- |
--
-- >>> size $ (fromList [2,3,4] :: BitSet Word32)
-- 3
--
size :: Bits bw => BitSet bw -> Int
size = popCount . toBits
{-# INLINE size #-}



null :: (Num bw, Eq bw) => BitSet bw -> Bool
null (BitSet w) = w == 0
{-# INLINE null #-}


-- ---------------------------------------------------------------------------
-- | Construction

empty :: Num bw => BitSet bw
empty = BitSet 0
{-# INLINE empty #-}

full :: (Bits bw, Num bw) => BitSet bw
full = BitSet (complement 0)
{-# INLINE full #-}

singleton :: forall bw. (Bits bw, Num bw) => Int -> BitSet bw
singleton n = BitSet $ setBit (0 :: bw) n


-- ---------------------------------------------------------------------------
-- | Insert / Delete

insert :: Bits bw => Int -> BitSet bw -> BitSet bw
insert n (BitSet w) = BitSet $ setBit w n
{-# INLINE insert #-}

delete :: Bits bw => Int -> BitSet bw -> BitSet bw
delete n (BitSet w) = BitSet $ clearBit w n
{-# INLINE delete #-}


-- ---------------------------------------------------------------------------
-- | Lookup

member :: Bits bw => Int -> BitSet bw -> Bool
member n (BitSet w) = testBit w n
{-# INLINE member #-}

notMember :: Bits bw => Int -> BitSet bw -> Bool
notMember n = not . member n
{-# INLINE notMember #-}


findMin :: (FiniteBits bw, Num bw) => BitSet bw -> Int   -- partial
findMin s = head $ filter (`member` s) $ range $ habitantRange s

findMax :: (FiniteBits bw, Num bw) => BitSet bw -> Int   -- partial
findMax s = head $ filter (`member` s) $ reverse $ range $ habitantRange s

-- lookupMin
-- lookupMax

-- ---------------------------------------------------------------------------
-- | Set operation


difference :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
difference (BitSet w0) (BitSet w1) = BitSet (w0 .&. complement w1)
{-# INLINE difference #-}


intersection :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
intersection (BitSet w0) (BitSet w1) = BitSet (w0 .&. w1)
{-# INLINE intersection #-}

(\\) :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
(\\) = intersection

union :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
union (BitSet w0) (BitSet w1) = BitSet (w0 .|. w1)
{-# INLINE union #-}

unions :: (Bits bw, Num bw) => [BitSet bw] -> BitSet bw
unions = foldr union empty
{-# INLINE unions #-}

partition :: forall bw. (FiniteBits bw, Num bw) => (Int -> Bool) -> BitSet bw -> (BitSet bw, BitSet bw)
partition f xs@(BitSet w0) = (BitSet (w0 .&. msk), BitSet (w0 .&. complement msk))
  where
    msk = foldr  (flip setBit) (0 :: bw) $ filter f $ range $ habitantRange xs


-- ---------------------------------------------------------------------------
-- | Set operation


habitantRange :: forall bw. (FiniteBits bw, Num bw) => BitSet bw -> (Int, Int)
habitantRange _ = habitantRange' (Proxy :: Proxy (BitSet bw))
{-# INLINE habitantRange #-}

habitantRange' :: forall bw proxy. (FiniteBits bw, Num bw) =>  proxy (BitSet bw) -> (Int, Int)
habitantRange' _ = (0, finiteBitSize (0 :: bw) - 1)
{-# INLINE habitantRange' #-}


isHabitant :: forall bw. (FiniteBits bw, Num bw) => Int -> BitSet bw -> Bool
isHabitant n _ = isHabitant' n (Proxy :: Proxy (BitSet bw))

isHabitant' :: forall bw proxy. (FiniteBits bw, Num bw) => Int -> proxy (BitSet bw) -> Bool
isHabitant' n pr = inRange (habitantRange' pr) n

-- ---------------------------------------------------------------------------

