{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Advent.Prelude
Description : Prelude extension for AoC solutions
Copyright   : (c) Jay Bromley, 2025
License     : ISC
Maintainer  : jbromley@gmail.com

Various helper functions for common operations needed in Advent of Code
problems.

-}
module Advent.Prelude
  ( listToPair
  , pairRangeToList
  , pairRangeToSet
  , textToInt
  , textRangeToPair
  ) where

import Data.IntSet (IntSet, fromList)
import Data.Text (Text, splitOn, unpack)

-- | Ensure a number is within a given range.
--
-- >>> clamp -1 0 10
-- 0
-- >>> clamp 11 0 10
-- 10
-- >>> clamp 5 0 10
-- 5
clamp :: Int -> Int -> Int -> Int
clamp n lo hi = max lo (min hi n)

-- | Convert a two-element list to a pair tuple. Raises an error if the list
-- does not have two elements.
--
-- >>> listToPair [1, 2]
-- (1, 2)
listToPair :: [a] -> (a, a)
listToPair [x1, x2] = (x1, x2)
listToPair _ = error "listToPair requires two-element list"

-- | Converts a Data.Text instance to an Int
textToInt :: Text -> Int
textToInt = read . unpack

-- | Convert Data.Text in the form "1-10" into a pair of Ints
textRangeToPair :: Text -> (Int, Int)
textRangeToPair txt = listToPair $ map textToInt $ splitOn "-" txt

-- | Convert a pair of integers into a range list
pairRangeToList :: (Integral a) => (a, a) -> [a]
pairRangeToList (x1, x2) = [x1 .. x2]

-- | Convert a pair of integers into a Data.Set containing a range of integers
pairRangeToSet :: (Int, Int) -> IntSet
pairRangeToSet (x1, x2) = fromList [x1 .. x2]
