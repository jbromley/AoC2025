module Main where

import Data.List (transpose)
import System.Environment (getArgs)
import Text.Printf (printf)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = ([[Int] -> Int], [[Int]])

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser1 :: String -> Input
parser1 s =
  let ls = reverse $ map words $ lines s
   in case ls of
        (opList:rest) -> (map opToFn opList, transpose $ map (map read) rest)
        [] -> error "input does not contain valid lines for parser"

opToFn :: String -> ([Int] -> Int)
opToFn "+" = sum
opToFn "*" = product
opToFn op = error (printf "illegal op '%s'" op)

-- | The function which calculates the solution for part one
solve :: Input -> Solution
solve = sum . uncurry (zipWith ($))

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

finish :: Char -> [String] -> Int
finish '+' xs = sum (map read xs)
finish '*' xs = product (map read xs)
finish op _xs = error (printf "invalid op '%s'" op)

main :: IO ()
main = do
  [part, filepath] <- getArgs
  input <- parser1 <$> readFile filepath
  if read @Int part == 1
    then do
      putStr "Day 6 part 1: "
      print $ solve input
    else do
      putStr "Day 6 part 2: "
      print $ solve2 input
