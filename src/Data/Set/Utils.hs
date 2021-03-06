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
import Prelude hiding (map, length)    
import Control.Monad (filterM)
import Data.Tuple (swap)

type Relation a = Set (a,a)

isReflexive :: Ord a => Set a -> Relation a -> Bool
isReflexive set rel = (makeReflexive set) `isSubsetOf` rel

isSymmetric :: Ord a => Relation a -> Bool
isSymmetric rel = rel == (inverseRelation rel)

isTransitive :: Ord a => Relation a -> Bool
isTransitive rel = (composition rel rel) `isSubsetOf` rel

inverseRelation :: Ord a => Relation a -> Relation a
inverseRelation rel = map swap rel

-- Relation composition: A o B 
composition :: Ord a => Relation a -> Relation a -> Relation a
composition relA relB = fromList $ [(a,c) | (a,b) <- listA, (e,c) <- listB, b == e]
    where listA = toList relA
          listB = toList relB

powerset :: Ord a => Set a -> Set [a]
powerset set = fromList $ filterM (const [True, False]) list
    where list = toList set

makeReflexive :: Ord a => Set a -> Relation a
makeReflexive set = map (\a -> (a,a)) set

makeRelation :: Ord a => Set a -> [(a -> a -> Bool)] -> Relation a
makeRelation set predicates = fromList $ [(x,y) | x <- list, 
                                                  y <- list, 
                                                  and (List.map (\pred -> pred x y) predicates)]
    where list = toList set 

length :: Set a -> Int
length = List.length . toList
