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



-- --------------------------------------------------------------------------
-- | Type

data TrueIntSet = TrueIntSet { toIntSet :: !IntSet.IntSet, isInverted :: Bool }

-- --------------------------------------------------------------------------
-- | Utility

null :: TrueIntSet -> Bool
null (TrueIntSet _ True) = False
null (TrueIntSet s False) = IntSet.null s
{-# INLINE null #-}

invert :: TrueIntSet -> TrueIntSet
invert (TrueIntSet s b) = TrueIntSet s (not b)
{-# INLINE invert #-}

fromIntSet :: IntSet.IntSet -> TrueIntSet
fromIntSet s = TrueIntSet s False
{-# iNLINE fromIntSet #-}

-- --------------------------------------------------------------------------
-- | Construction

empty :: TrueIntSet
empty = TrueIntSet IntSet.empty False
{-# INLINE empty #-}

singleton :: Int -> TrueIntSet
singleton n = TrueIntSet (IntSet.singleton n) False
{-# INLINE singleton #-}


full :: TrueIntSet
full = invert empty
{-# INLINE full #-}


fromList :: [Int] -> TrueIntSet
fromList xs = TrueIntSet (IntSet.fromList xs) False
{-# INLINE fromList #-}


-- --------------------------------------------------------------------------
-- | Insert / Delete

insert :: Int -> TrueIntSet -> TrueIntSet
insert x (TrueIntSet s False) = TrueIntSet (IntSet.insert x s) False
insert x (TrueIntSet s True) = TrueIntSet (IntSet.delete x s) True
{-# INLINE insert #-}

delete :: Int -> TrueIntSet -> TrueIntSet
delete x (TrueIntSet s False) = TrueIntSet (IntSet.delete x s) False
delete x (TrueIntSet s True) = TrueIntSet (IntSet.insert x s) True
{-# INLINE delete #-}

-- --------------------------------------------------------------------------
-- | Lookup

member :: Int -> TrueIntSet -> Bool
member x (TrueIntSet s inv) = inv /= IntSet.member x s
{-# INLINE member #-}

notMember :: Int -> TrueIntSet -> Bool
notMember x = not . member x
{-# INLINE notMember #-}


-- --------------------------------------------------------------------------
-- | Set operation

difference, intersection, union :: TrueIntSet -> TrueIntSet -> TrueIntSet
difference x y = x `intersection` invert y
{-# iNLINE difference #-}
intersection (TrueIntSet x inv0) (TrueIntSet y inv1)
  | inv0 /= inv1 = TrueIntSet (x `IntSet.difference` y) False
  | inv0         = TrueIntSet (x `IntSet.union` y) True
  | otherwise    = TrueIntSet (x `IntSet.intersection` y) False
{-# INLINE intersection #-}
union (TrueIntSet x inv0) (TrueIntSet y inv1)
  | inv0 /= inv1 = TrueIntSet (x `IntSet.difference` y) True
  | inv0         = TrueIntSet (x `IntSet.intersection` y) True
  | otherwise    = TrueIntSet (x `IntSet.union` y) False
{-# INLINE union #-}

-- vim: ts=8 sw=2 expandtab :

