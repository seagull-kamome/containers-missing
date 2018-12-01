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
module Data.TrueSet.TrueSet (
  TrueSet, toSet, fromIntSet,
  isInverted, null, invert,
  empty, singleton, full, fromList,
  insert, delete, member, notMember,
  difference, intersection, union
  ) where

import Prelude hiding (null)
import qualified Data.Set as Set



-- --------------------------------------------------------------------------
-- | Type

data TrueSet a = TrueSet { isInverted::Bool, toSet :: !(Set.Set a) }

-- --------------------------------------------------------------------------
-- | Utility

null :: TrueSet a -> Bool
null (TrueSet True _) = False
null (TrueSet False s) = Set.null s
{-# INLINE null #-}

invert :: TrueSet a -> TrueSet a
invert (TrueSet b s) = TrueSet (not b) s
{-# INLINE invert #-}

fromIntSet :: Set.Set a -> TrueSet a
fromIntSet = TrueSet False
{-# iNLINE fromIntSet #-}

-- --------------------------------------------------------------------------
-- | Construction

empty :: TrueSet a
empty = TrueSet False Set.empty
{-# INLINE empty #-}

singleton :: a -> TrueSet a
singleton = TrueSet False . Set.singleton
{-# INLINE singleton #-}


full :: TrueSet a
full = invert empty
{-# INLINE full #-}


fromList :: Ord a => [a] -> TrueSet a
fromList = TrueSet False . Set.fromList
{-# INLINE fromList #-}


-- --------------------------------------------------------------------------
-- | Insert / Delete

insert :: Ord a => a -> TrueSet a -> TrueSet a
insert x y@(TrueSet False s) = y { toSet = Set.insert x s }
insert x y@(TrueSet True s) = y { toSet = Set.delete x s }
{-# INLINE insert #-}

delete :: Ord a => a -> TrueSet a -> TrueSet a
delete x y@(TrueSet False s) = y { toSet = Set.delete x s }
delete x y@(TrueSet True s) = y { toSet = Set.insert x s }
{-# INLINE delete #-}

-- --------------------------------------------------------------------------
-- | Lookup

member :: Ord a => a -> TrueSet a -> Bool
member x (TrueSet inv s) = inv /= Set.member x s
{-# INLINE member #-}

notMember :: Ord a => a -> TrueSet a -> Bool
notMember x = not . member x
{-# INLINE notMember #-}


-- --------------------------------------------------------------------------
-- | Set operation

difference, intersection, union :: Ord a => TrueSet a -> TrueSet a -> TrueSet a
difference x y = x `intersection` invert y
{-# iNLINE difference #-}
intersection (TrueSet inv0 x) (TrueSet inv1 y)
  | inv0 /= inv1 = TrueSet False (x `Set.difference` y)
  | inv0         = TrueSet True (x `Set.union` y)
  | otherwise    = TrueSet False (x `Set.intersection` y)
{-# INLINE intersection #-}
union (TrueSet inv0 x) (TrueSet inv1 y)
  | inv0 /= inv1 = TrueSet True (x `Set.difference` y)
  | inv0         = TrueSet True (x `Set.intersection` y)
  | otherwise    = TrueSet False (x `Set.union` y)
{-# INLINE union #-}

-- vim: ts=8 sw=2 expandtab :

