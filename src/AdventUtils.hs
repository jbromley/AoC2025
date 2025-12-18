module AdventUtils
  ( listToPair
  , pairRangeToList
  , pairRangeToSet
  , textToInt
  , textRangeToPair
  ) where

import Data.IntSet (IntSet, fromList)
import Data.Text (Text, splitOn, unpack)

clamp :: Int -> Int -> Int -> Int
clamp n lo hi = max lo (min hi n)

listToPair :: [a] -> (a, a)
listToPair [x1, x2] = (x1, x2)
listToPair _ = error "listToPair requires two-element list"

textToInt :: Text -> Int
textToInt = read . unpack

textRangeToPair :: Text -> (Int, Int)
textRangeToPair txt = listToPair $ map textToInt $ splitOn "-" txt

pairRangeToList :: (Integral a) => (a, a) -> [a]
pairRangeToList (x1, x2) = [x1 .. x2]

pairRangeToSet :: (Int, Int) -> IntSet
pairRangeToSet (x1, x2) = fromList [x1 .. x2]
