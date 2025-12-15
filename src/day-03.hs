module Main where

import Data.Char (digitToInt)
-- import Data.List (foldl')
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [[Int]]

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser = map (map digitToInt) . lines

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . map (findJoltage 2)

findJoltage :: Int -> [Int] -> Int
findJoltage m = digitsToInteger . largestDigits m

largestDigits :: Int -> [Int] -> [Int]
largestDigits m xs = take m . reverse $ searchDigits k [] xs
  where
    k = length xs - m

    searchDigits :: Int -> [Int] -> [Int] -> [Int]
    searchDigits _ stack [] = stack
    searchDigits drops stack (d : ds)
      | drops > 0, s : stack' <- stack, s < d = searchDigits (drops - 1) stack' (d : ds)
      | otherwise = searchDigits drops (d : stack) ds

digitsToInteger :: [Int] -> Int
digitsToInteger = foldl' (\acc d -> acc * 10 + fromIntegral d) 0

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = sum . map (findJoltage 12)

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStr "Day 3 part 1: "
      print $ solve1 input
    else do
      putStr "Day 3 part 2: "
      print $ solve2 input
