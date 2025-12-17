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
solve1 manifold =
  case manifold of
    (initialRow:rest) ->
      case V.findIndex (== 'S') initialRow of
        Just b -> propagate rest (S.singleton b) 0
        Nothing -> error "invalid manifold, can't find initial beam"
    [] -> error "invalid manifold"

propagate :: [Vector Char] -> IntSet -> Int -> Int
propagate manifold beams numSplits =
  case manifold of
    [] -> numSplits
    (nextRow:manifold') ->
      let (nextBeams, newSplits) = advance nextRow beams
       in propagate manifold' nextBeams (numSplits + newSplits)
  where
    advance :: Vector Char -> IntSet -> (IntSet, Int)
    advance row currentBeams =
      let splitters = S.fromList $ V.toList $ V.findIndices (== '^') row
          splits = S.intersection currentBeams splitters
          maxX = V.length row - 1
          nextBeams = S.union (splitBeams splits maxX) (S.difference currentBeams splits)
       in (nextBeams, S.size splits)
    splitBeams :: IntSet -> Int -> IntSet
    splitBeams bs width =
      S.foldl'
        (\acc b -> S.union acc (S.fromList [clamp (b - 1) 0 width, clamp (b + 1) 0 width]))
        S.empty
        bs

clamp :: Int -> Int -> Int -> Int
clamp n lo hi = max lo (min hi n)

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
