{-# LANGUAGE BangPatterns #-}

module RangeList (RangeList, normalize, member) where

import Data.List (sortOn)
import Data.Vector (Vector)
import qualified Data.Vector as V

type RangeList = Vector (Int, Int)

-- One-time preprocessing if your list isn't already sorted/merged.
-- O(n log n) sort + O(n) merge.
normalize :: [(Int, Int)] -> RangeList
normalize = V.fromList . merge . sortOn fst
  where
    merge [] = []
    merge ((a,b):xs)
      | b < a     = merge xs
      | otherwise = go a b xs

    go !a !b [] = [(a,b)]
    go !a !b ((c,d):xs)
      | d < c     = go a b xs
      | c <= b + 1 = go a (max b d) xs
      | otherwise  = (a,b) : go c d xs

-- Fast binary search over a sorted, non-overlapping range vector.
-- O(log n)
member :: Int -> RangeList -> Bool
member x v = go 0 (V.length v - 1)
  where
    go !lo !hi
      | lo > hi   = False
      | otherwise =
          let !mid = (lo + hi) `div` 2
              (!a,!b) = V.unsafeIndex v mid
          in if x < a then go lo (mid - 1)
             else if x > b then go (mid + 1) hi
             else True
