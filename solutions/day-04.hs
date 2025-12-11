module Main where

import Data.Matrix (Matrix(..), fromLists, getElem, mapPos, submatrix)
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = Matrix Int

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser =
  fromLists
    . map
        (map
           (\ch ->
              if ch == '@'
                then 1
                else 0))
    . lines

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . findMovable 4

findMovable :: Int -> Matrix Int -> Matrix Int
findMovable k m = mapPos (\(r, c) _a -> isMovable r c k m) m

isMovable :: Int -> Int -> Int -> Matrix Int -> Int
isMovable r c k m =
  if getElem r c m == 1 && countNeighbors r c m < k
    then 1
    else 0

countNeighbors :: Int -> Int -> Matrix Int -> Int
countNeighbors r c m =
  let maxRows = nrows m
      maxCols = ncols m
      neighbors =
        submatrix (max 1 (r - 1)) (min maxRows (r + 1)) (max 1 (c - 1)) (min maxCols (c + 1)) m
   in sum neighbors - getElem r c m

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = findAllMovable 4

findAllMovable :: Int -> Matrix Int -> Int
findAllMovable k m = findAllMovable' k m 0

findAllMovable' :: Int -> Matrix Int -> Int -> Int
findAllMovable' k m n =
  let movable = findMovable k m
      numMovable = sum movable
   in case numMovable of
        0 -> n
        _ -> findAllMovable' k (m - movable) (n + numMovable)

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath
  if read @Int part == 1
    then do
      putStr "Day 04 part 1: "
      print $ solve1 input
    else do
      putStr "Day 04 part 2: "
      print $ solve2 input
