module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import qualified Data.Vector as V
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = (Int, [IntSet])

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser s =
  case map V.fromList $ lines s of
    (initialRow:rest) ->
      let beam =
            case V.findIndex (== 'S') initialRow of
              Just b -> b
              Nothing -> error "invalid manifold, can't find initial beam"
          manifold = map (S.fromList . V.toList . V.findIndices (== '^')) rest
       in (beam, manifold)
    [] -> error "invalid input"

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 (beam, manifold) = propagate manifold (S.singleton beam) 0

propagate :: [IntSet] -> IntSet -> Int -> Int
propagate manifold beams numSplits =
  case manifold of
    [] -> numSplits
    (nextRow:manifold') ->
      let (nextBeams, newSplits) = advance nextRow beams
       in propagate manifold' nextBeams (numSplits + newSplits)
  where
    advance :: IntSet -> IntSet -> (IntSet, Int)
    advance splitters currentBeams =
      let splits = S.intersection currentBeams splitters
          nextBeams = S.union (splitBeams splits) (S.difference currentBeams splits)
       in (nextBeams, S.size splits)
    splitBeams :: IntSet -> IntSet
    splitBeams = S.foldl' (\acc b -> S.union acc (S.fromList [b - 1, b + 1])) S.empty

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 (beam, manifold) = propagateTimelines manifold (M.singleton beam 1)

propagateTimelines :: [IntSet] -> IntMap Int -> Int
propagateTimelines manifold beams =
  case manifold of
    [] -> M.foldl' (+) 0 beams
    (splitters:manifold') -> propagateTimelines manifold' (adjustMapWithSet splitters beams)

adjustMapWithSet :: IntSet -> IntMap Int -> IntMap Int
adjustMapWithSet intSet = M.foldrWithKey adjustEntry M.empty
  where
    adjustEntry k v acc
      | S.member k intSet =
        M.insertWith (+) (k - 1) v $ M.insertWith (+) (k + 1) v $ M.insertWith const k 0 acc
      | otherwise = M.insertWith (+) k v acc

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
