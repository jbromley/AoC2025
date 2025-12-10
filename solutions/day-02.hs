module Main where

import qualified Data.Text as T
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [(Int, Int)]

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser input = map strRangeToPair $ T.splitOn "," $ T.pack input

toInt :: T.Text -> Int
toInt = read . T.unpack

toPair :: [a] -> (a, a)
toPair [start, end] = (start, end)
toPair _ = error "toPair requires two-element list"

strRangeToPair :: T.Text -> (Int, Int)
strRangeToPair txt = toPair $ map toInt $ T.splitOn "-" txt

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 input = sum $ map (sumInvalidIds isInvalidId1) input

sumInvalidIds :: (Int -> Bool) -> (Int, Int) -> Int
sumInvalidIds f (start, end) = sum $ filter f [start .. end]

isInvalidId1 :: Int -> Bool
isInvalidId1 n =
  let nStr = T.pack $ show n
      mid = T.length nStr `div` 2
   in isDoublePair $ T.splitAt mid nStr

isDoublePair :: Eq a => (a, a) -> Bool
isDoublePair (x, y) = x == y

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = sum $ map (sumInvalidIds isInvalidId2) input

isInvalidId2 :: Int -> Bool
isInvalidId2 num =
  let t = T.pack $ show num
      n = T.length t
      mid = n `div` 2
   in any
        (\k ->
           n `mod` k == 0
             && let chunk = T.take k t
                 in T.replicate (n `div` k) chunk == t)
        [1 .. mid]

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStr "Day 2 part 1: "
      print $ solve1 input
    else do
      putStr "Day 2 part 2: "
      print $ solve2 input
