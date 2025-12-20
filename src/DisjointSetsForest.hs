module DisjointSetsForest (empty, insert, memberSet, insertSet) where

import Data.Set (Set)
import qualified Data.Set as S

empty :: [Set a]
empty = []

insert :: Ord a => a -> [Set a] -> [Set a]
insert x dsf =
  if any (S.member x) dsf then dsf else S.singleton x : dsf

memberSet :: Ord a => a -> [Set a] -> Maybe (Set a)
memberSet x dsf =
  case dsf of
    [] -> Nothing
    (s:ss) -> if S.member x s then Just s else memberSet x ss

insertSet :: Ord a => Set a -> [Set a] -> [Set a]
insertSet x ss = mergedSet : remainingSets
  where
    (containingSets, remainingSets) = partition (not . S.null . S.intersection x) ss
    mergedSet = foldr S.union x containingSets

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition predFn xs = go predFn xs ([], [])
  where
    go f l (trueElems, falseElems) =
      case l of
        [] -> (trueElems, falseElems)
        (x:l') -> if f x
                    then go f l' (x:trueElems, falseElems)
                    else go f l' (trueElems, x:falseElems)
