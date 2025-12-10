module Main where

import Data.Char (digitToInt)
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
solve1 = sum . map findJoltage

findJoltage :: [Int] -> Int
findJoltage input =
  let (d1, xs) = findMax input True
      (d2, _) = findMax xs False
   in d1 * 10 + d2

findMax :: [Int] -> Bool -> (Int, [Int])
findMax [] _skipLast = error "findMaxAndIndex can't handle empty list"
findMax xs skipLast = findMax' nums (head xs) nums skipLast
  where
    nums = tail xs

findMax' :: [Int] -> Int -> [Int] -> Bool -> (Int, [Int])
findMax' [_x] m xsMax True = (m, xsMax)
findMax' [] m _xsMax _skipLast = (m, [])
findMax' (x:xs) m xsMax skipLast
  | x > m = findMax' xs x xs skipLast
  | otherwise = findMax' xs m xsMax skipLast

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

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
