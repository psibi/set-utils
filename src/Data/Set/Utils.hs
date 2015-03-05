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
import Control.Monad (filterM)

type Relation a = Set (a,a)

isReflexive :: Ord a => Set a -> Relation a -> Bool
isReflexive set rel = (makeReflexive set) `isSubsetOf` rel

isSymmetric :: Relation a -> Bool
isSymmetric = undefined

isTransitive :: Relation a -> Bool
isTransitive = undefined

powerset :: Ord a => Set a -> Set [a]
powerset set = fromList $ filterM (const [True, False]) list
    where list = toList set

makeReflexive :: Ord a => Set a -> Relation a
makeReflexive set = map (\a -> (a,a)) set

-- cp
