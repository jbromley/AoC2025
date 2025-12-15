module Main where

import AdventUtils (textRangeToPair, textToInt)
-- import Data.Foldable (foldl')
-- import Data.List (sortOn)
import Data.Text (Text, breakOn, pack, splitOn, strip)
import qualified Data.Vector as V
import RangeList
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = (RangeList, [Int])

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser input =
  let (freshStr, ingredientsStr) = breakOn "\n\n" $ pack input
      freshList = parseFreshList freshStr
      ingredients = parseIngredients ingredientsStr
   in (freshList, ingredients)

parseFreshList :: Text -> RangeList
parseFreshList = normalize . map textRangeToPair . splitOn "\n"

parseIngredients :: Text -> [Int]
parseIngredients = map textToInt . splitOn "\n" . strip

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 (fresh, ingredients) = length $ filter id $ map (\x -> member x fresh) ingredients

-- | The function which calculates the solution for part two
solve2 :: RangeList -> Solution
solve2 = V.foldl' (\acc (x, y) -> acc + y - x + 1) 0

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
      print $ solve2 (fst input)
