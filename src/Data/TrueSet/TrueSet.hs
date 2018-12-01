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
module Data.TrueSet(
    module Data.Container.Class,
    TrueSetW, TrueSet, fromList, invert, exact, inverted
    ) where

import Prelude hiding (null)
import Data.Container.Class
import qualified Data.Set


-- --------------------------------------------------------------------------
--
data TrueSetW f a = TrueSetW { exact :: !(f a), inverted :: !Bool }
type TrueSet a = TrueSetW Data.Set.Set a
instance Empty (f a) => Empty (TrueSetW f a) where
  empty = TrueSetW empty False
  -- null (TrueSetW x False) = null x
 -- null _ = False

instance Empty (f a) => Full (TrueSetW f a) where
  full = TrueSetW empty True
instance IsSet (f a) => IsSet (TrueSetW f a) where
  type Elm (TrueSetW f a) = Elm (f a)
  insert x (TrueSetW s False) = TrueSetW (insert x s) False
  insert x (TrueSetW s True) = TrueSetW (delete x s) True
  delete x (TrueSetW s False) = TrueSetW (delete x s) False
  delete x (TrueSetW s True) = TrueSetW (insert x s) True
  singleton x = TrueSetW (singleton x) False
  member x (TrueSetW s inv) = inv `xor` member x s where
      False `xor` y = y
      True `xor` y = not y
  difference x y = x `intersection` invert y
  (TrueSetW x True)  `intersection` (TrueSetW y True) = TrueSetW (x `union` y) True
  (TrueSetW x False) `intersection` (TrueSetW y False) = TrueSetW (x `intersection` y) False
  (TrueSetW x True)  `intersection` (TrueSetW y False) = TrueSetW (y \\ x) False
  (TrueSetW x False) `intersection` (TrueSetW y True) = TrueSetW (x \\ y) False
  (TrueSetW x True)  `union` (TrueSetW y True) = TrueSetW (x `intersection` y) True
  (TrueSetW x False) `union` (TrueSetW y False) = TrueSetW (x `union` y) False
  (TrueSetW x True)  `union` (TrueSetW y False) = TrueSetW (x \\ y) True
  (TrueSetW x False) `union` (TrueSetW y True) = TrueSetW (y \\ x) True


fromList :: Ord a => [a] -> TrueSet a
fromList xs = TrueSetW (Data.Set.fromList xs) False

invert :: TrueSetW f a -> TrueSetW f a
invert x = x { inverted = not (inverted x) }


