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
  BitSet, BitSet8, BitSet16, BitSet32, BitSet64, SmallBitSet,
  size, empty, full, null,
  insert, delete, member, notMember,
  difference, intersection, union,
  isHabitant, isHabitant',
  habitantRange, habitantRange'
  ) where

import Prelude hiding(null)
import Data.Word
import Data.Bits (Bits, popCount, (.|.), (.&.),
  complement, setBit, clearBit, testBit, FiniteBits, finiteBitSize)
import Data.Proxy (Proxy(Proxy))
import Data.Ix (inRange)
import GHC.TypeNats (Nat)
-- import GHC.Prim

-- ---------------------------------------------------------------------------

newtype BitSet bw = BitSet { toBits::bw }
deriving instance Eq bw => Eq (BitSet bw)
deriving instance Ord bw => Ord (BitSet bw)
deriving instance Show bw => Show (BitSet bw)

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

-- ---------------------------------------------------------------------------
-- | Utilities

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


-- ---------------------------------------------------------------------------
-- | Insert / Delete

insert :: Bits bw => Int -> BitSet bw -> BitSet bw
insert n (BitSet w) = BitSet $ setBit w n
{-# INLINE insert #-}

delete :: Bits bw => Int -> BitSet bw -> BitSet bw
delete n (BitSet w) = BitSet $ clearBit w n
{-# INLINE delete #-}


-- ---------------------------------------------------------------------------
-- | Tests

member :: Bits bw => Int -> BitSet bw -> Bool
member n (BitSet w) = testBit w n
{-# INLINE member #-}

notMember :: Bits bw => Int -> BitSet bw -> Bool
notMember n = not . member n
{-# INLINE notMember #-}


-- ---------------------------------------------------------------------------
-- | Set operation


difference :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
difference (BitSet w0) (BitSet w1) = BitSet (w0 .&. complement w1)
{-# INLINE difference #-}


intersection :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
intersection (BitSet w0) (BitSet w1) = BitSet (w0 .&. w1)
{-# INLINE intersection #-}


union :: Bits bw => BitSet bw -> BitSet bw -> BitSet bw
union (BitSet w0) (BitSet w1) = BitSet (w0 .|. w1)
{-# INLINE union #-}


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

