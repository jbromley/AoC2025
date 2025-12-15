module Main where

import AdventUtils (textToInt)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Printf (printf)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = ([([Int] -> Int)], [[Int]])

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser s =
  let ls = reverse $ map T.words $ T.lines $ T.strip $ T.pack s
   in case ls of
        (opList:rest) ->
          let ops = map opToFn opList
              nums = transpose $ map (map textToInt) rest
           in (ops, nums)
        [] -> error "input does not contain valid lines for parser"

opToFn :: Text -> ([Int] -> Int)
opToFn opText
  | op == "+" = sum
  | op == "*" = product
  | otherwise = error (printf "opToFn invalid operator '%s'" op)
  where
    op = T.unpack opText

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xss
  | any null xss = []
  | otherwise =
    let heads = [h | (h:_) <- xss] -- Collect heads of non-empty lists
        tails = [t | (_:t) <- xss] -- Collect tails of non-empty lists
     in heads : transpose tails

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . uncurry (zipWith ($))

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStr "Day 6 part 1: "
      print $ solve1 input
    else do
      putStr "Day 6 part 2: "
      print $ solve2 input
