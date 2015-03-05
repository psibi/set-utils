-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set.Utils
-- Copyright   :  (c) Sibi 2014
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Sibi <sibi@psibi.in>
-- Stability   :  stable
-- Portability :  Haskell 2010

-----------------------------------------------------------------------------

module Data.Set.Utils where

import Data.Set
import qualified Data.List as List
import Prelude hiding (map)    


type Relation a = Set (a,a)

isReflexive :: Relation a -> Bool
isReflexive = undefined

isSymmetric :: Relation a -> Bool
isSymmetric = undefined

isTransitive :: Relation a -> Bool
isTransitive = undefined

powerset :: Set a -> Set (Set a)
powerset = undefined

makeReflexive :: Ord a => Set a -> Relation a
makeReflexive set = map (\a -> (a,a)) set

-- cp
