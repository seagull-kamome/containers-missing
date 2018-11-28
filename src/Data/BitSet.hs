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
module Data.BitSet(
  module Data.Container.Class,
  BitSet, BitSet8, BitSet16, BitSet32, BitSet64, SmallBitSet
  ) where

import Data.Word
import Data.Bits
import Data.Container.Class
import GHC.TypeNats (Nat)
-- import GHC.Prim

-- ---------------------------------------------------------------------------

newtype BitSet bw = BitSet { toBits::bw }
deriving instance Eq bw => Eq (BitSet bw)
deriving instance Ord bw => Ord (BitSet bw)
deriving instance Show bw => Show (BitSet bw)

-- ---------------------------------------------------------------------------
-- Small bitset

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

instance Bits bw => Semigroup (BitSet bw) where
  (BitSet w0) <> (BitSet w1) = BitSet (w0 .|. w1)
  {-# INLINE (<>) #-}
instance (Bits bw, Num bw) => Monoid (BitSet bw) where
  mempty = BitSet 0

-- ---------------------------------------------------------------------------

instance (Bits bw, Num bw) => FiniteElements (BitSet bw) where
  size = popCount . toBits
  {-# INLINE size #-}

instance (Num bw, Eq bw) => Empty (BitSet bw) where
  empty = BitSet 0
  {-# INLINE empty #-}
  null (BitSet w) = w == 0
  {-# INLINE null #-}

instance (Num bw, Bits bw) => Full (BitSet bw) where
  full = BitSet (complement 0)
  {-# INLINE full #-}

instance (Num bw, Bits bw) => IsSet (BitSet bw) where
  type Elm (BitSet bw) = Int
  insert n (BitSet w) = BitSet $ setBit w n
  {-# INLINE insert #-}
  delete n (BitSet w) = BitSet $ clearBit w n
  {-# INLINE delete #-}
  member n (BitSet w) = testBit w n
  {-# INLINE member #-}
  difference (BitSet w0) (BitSet w1) = BitSet (w0 .&. complement w1)
  {-# INLINE difference #-}
  intersection (BitSet w0) (BitSet w1) = BitSet (w0 .&. w1)
  {-# INLINE intersection #-}
  union (BitSet w0) (BitSet w1) = BitSet (w0 .|. w1)
  {-# INLINE union #-}

instance (FiniteBits bw, Num bw, Bits bw) => PartialSet (BitSet bw) where
  insertable n = n >= 0 && n < finiteBitSize (0 :: bw)

-- ---------------------------------------------------------------------------

