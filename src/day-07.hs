module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [Vector Char]

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser = map V.fromList . lines

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = error "Part 1 Not implemented"

propagate :: [Vector Char] -> IntSet -> Int -> Int
propagate (nextLine:restMap) beams splits =
  case nextLine of
    [] -> splits
    _ ->
      let (nextBeams, newSplits) = splitBeams nextLine beams
       in propagate restMap nextBeams (splits + newSplits)

splitBeams :: Vector Char -> IntSet -> (IntSet, Int)
splitBeams line beams = undefined

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filePath] <- getArgs
  input <- parser <$> readFile filePath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStr "Day 07 part 1: "
      print $ solve1 input
    else do
      putStr "Day 07 part 2: "
      print $ solve2 input
