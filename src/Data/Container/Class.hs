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
{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Container.Class (
  FiniteElements(..),
  Empty(..),
  Full(..),
  PartialSet(..), StrictSet,
  IsSet(..)
  ) where

import qualified Data.Set

-- --------------------------------------------------------------------------
--
class FiniteElements c where size :: c -> Int

instance FiniteElements (Data.Set.Set a) where
  size = Data.Set.size
  {-# INLINE size #-}

-- --------------------------------------------------------------------------
--
class Empty c where
  empty :: c
  null :: c -> Bool

instance Empty (Data.Set.Set a) where
  empty = Data.Set.empty
  {-# INLINE empty #-}
  null = Data.Set.null
  {-# INLINE null #-}

-- --------------------------------------------------------------------------
--

class Full c where full :: c

-- --------------------------------------------------------------------------
--


class IsSet c where
  type Elm c :: *
  insert :: Elm c -> c -> c
  delete :: Elm c -> c -> c
  singleton :: Elm c -> c
  member :: Elm c -> c -> Bool
  notMember :: Elm c -> c -> Bool
  difference :: c -> c -> c
  intersection :: c -> c -> c
  union :: c -> c -> c
  --
  notMember a = not . member a
  {-# INLINE notMember #-}
  default singleton :: Empty c => Elm c -> c
  singleton = flip insert empty
  {-# INLINE singleton #-}
  --
  (\\) :: c -> c -> c
  (\\) = intersection
  {-# INLINE (\\) #-}

instance Ord a => IsSet (Data.Set.Set a) where
  type Elm (Data.Set.Set a) = a
  insert = Data.Set.insert
  {-# INLINE insert #-}
  delete = Data.Set.delete
  {-# INLINE delete #-}
  singleton = Data.Set.singleton
  {-# INLINE singleton #-}
  member = Data.Set.member
  {-# INLINE member #-}
  notMember = Data.Set.member
  {-# INLINE notMember #-}
  difference = Data.Set.difference
  {-# INLINE difference #-}
  intersection = Data.Set.intersection
  {-# INLINE intersection #-}
  union = Data.Set.union
  {-# INLINE union #-}

class IsSet c => PartialSet c where insertable :: Elm c -> Bool
class IsSet c => StrictSet c where

