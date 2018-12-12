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
module Data.TrueSet.TrueIntSet (
  TrueIntSet, toIntSet, fromIntSet,
  isInverted, null, invert,
  empty, singleton, full, fromList,
  insert, delete, member, notMember,
  difference, intersection, union
  ) where

import Prelude hiding (null)
import qualified Data.IntSet as IntSet

import qualified Data.Binary as BIN

-- --------------------------------------------------------------------------
-- | Type

data TrueIntSet = TrueIntSet { isInverted::Bool, toIntSet :: !IntSet.IntSet }
instance BIN.Binary TrueIntSet where
  put (TrueIntSet x y) = BIN.put x >> BIN.put y
  get = TrueIntSet <$> BIN.get <*> BIN.get


-- --------------------------------------------------------------------------
-- | Utility

null :: TrueIntSet -> Bool
null (TrueIntSet True _) = False
null (TrueIntSet False s) = IntSet.null s
{-# INLINE null #-}

invert :: TrueIntSet -> TrueIntSet
invert (TrueIntSet b s) = TrueIntSet (not b) s
{-# INLINE invert #-}

fromIntSet :: IntSet.IntSet -> TrueIntSet
fromIntSet s = TrueIntSet False s
{-# iNLINE fromIntSet #-}

-- --------------------------------------------------------------------------
-- | Construction

empty :: TrueIntSet
empty = TrueIntSet False IntSet.empty
{-# INLINE empty #-}

singleton :: Int -> TrueIntSet
singleton n = TrueIntSet False (IntSet.singleton n)
{-# INLINE singleton #-}


full :: TrueIntSet
full = invert empty
{-# INLINE full #-}


fromList :: [Int] -> TrueIntSet
fromList xs = TrueIntSet False (IntSet.fromList xs)
{-# INLINE fromList #-}


-- --------------------------------------------------------------------------
-- | Insert / Delete

insert :: Int -> TrueIntSet -> TrueIntSet
insert x (TrueIntSet False s) = TrueIntSet False (IntSet.insert x s)
insert x (TrueIntSet True s) = TrueIntSet False (IntSet.delete x s)
{-# INLINE insert #-}

delete :: Int -> TrueIntSet -> TrueIntSet
delete x (TrueIntSet False s) = TrueIntSet False (IntSet.delete x s)
delete x (TrueIntSet True s) = TrueIntSet False (IntSet.insert x s)
{-# INLINE delete #-}

-- --------------------------------------------------------------------------
-- | Lookup

member :: Int -> TrueIntSet -> Bool
member x (TrueIntSet inv s) = inv /= IntSet.member x s
{-# INLINE member #-}

notMember :: Int -> TrueIntSet -> Bool
notMember x = not . member x
{-# INLINE notMember #-}


-- --------------------------------------------------------------------------
-- | Set operation

difference, intersection, union :: TrueIntSet -> TrueIntSet -> TrueIntSet
difference x y = x `intersection` invert y
{-# iNLINE difference #-}
intersection (TrueIntSet inv0 x) (TrueIntSet inv1 y)
  | inv0 /= inv1 = TrueIntSet False (x `IntSet.difference` y)
  | inv0         = TrueIntSet True (x `IntSet.union` y)
  | otherwise    = TrueIntSet False (x `IntSet.intersection` y)
{-# INLINE intersection #-}
union (TrueIntSet inv0 x) (TrueIntSet inv1 y)
  | inv0 /= inv1 = TrueIntSet True (x `IntSet.difference` y)
  | inv0         = TrueIntSet True (x `IntSet.intersection` y)
  | otherwise    = TrueIntSet False (x `IntSet.union` y)
{-# INLINE union #-}

-- vim: ts=8 sw=2 expandtab :

