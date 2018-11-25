{-# LANGUAGE StandaloneDeriving #-}
module Data.BitSet(
  BitSet, BitSet8, BitSet16, BitSet32, BitSet64, SmallBitSet
  ) where

import Data.Word
import Data.Bits
import Data.Container.Class
import GHC.TypeNats (Nat)

-- ---------------------------------------------------------------------------

newtype BitSet bw = BitSet { toBits:: bw }
deriving instance Eq bw => Eq (BitSet bw)
deriving instance Ord bw => Ord (BitSet bw)
deriving instance Show bw => Show (BitSet bw)

-- ---------------------------------------------------------------------------
-- Small bitset

type BitSet8 = BitSet Word8
type BitSet16 = BitSet Word16
type BitSet32 = BitSet Word32
type BitSet64 = BitSet Word64

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

instance FiniteBits bw => FiniteElements (BitSet bw) where
  size = finiteBitSize . toBits
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

-- ---------------------------------------------------------------------------

