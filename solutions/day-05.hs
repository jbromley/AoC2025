module Main where

import Data.IntSet (IntSet)
import Data.Text (Text, breakOn, pack, splitOn, unpack)
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = (IntSet, [Int])

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser input =
  let (goodStr, ingredientsStr) = breakOn "\n\n" $ pack input
      goodList = parseGoodList goodStr
      ingredients = parseIngredients ingredientsStr
   in (goodList, ingredients)

parseGoodList :: Text -> IntSet
parseGoodList = undefined

parseIngredients :: Text -> [Int]
parseIngredients = undefined

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = error "Part 1 Not implemented"

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStr "Day 5 part 1: "
      print $ solve1 input
    else do
      putStr "Day 5 part 2: "
      print $ solve2 input
