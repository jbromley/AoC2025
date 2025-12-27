module Main where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.IntDisjointSet (IntDisjointSet)
import Data.IntDisjointSet qualified as DS
import Data.List (sortBy, tails)
import Data.Map qualified as M
import Data.Ord (comparing)
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    a list of 3-D vector with integer components, like V3
- Solution an integer
-}
data Pairing = Pairing
  { id1, id2, dist2, xDist :: !Int
  } deriving (Show)

type Input = ([Pairing], IntDisjointSet)

type Solution = Int

parser :: ByteString -> Input
parser s =
  let boxes = map parseLine $ B.lines s
      numBoxes = length boxes
   in ( sortBy
          (comparing dist2)
          [ Pairing i j d (x1 * x2)
          | (i, [x1, y1, z1]):xs <- tails (zip [1 ..] boxes)
          , (j, [x2, y2, z2]) <- xs
          , let d = sq (x1 - x2) + sq (y1 - y2) + sq (z1 - z2)
          ]
      , DS.fromList [(i, i) | i <- [1 .. numBoxes]])

parseLine :: B.ByteString -> [Int]
parseLine line = [i | Just (i, _) <- map B.readInt (B.splitWith (== ',') line)]

sq :: Int -> Int
sq x = x * x

-- | The function which calculates the solution for part one
solve1 :: Int -> Input -> (Solution, IntDisjointSet)
solve1 n (pairs, ds) =
  let ds' = makeCircuits ds $ take n pairs
      prod = product $ take 3 $ sortBy (flip compare) $ disjointSetSizes ds'
   in (prod, ds')

makeCircuits :: IntDisjointSet -> [Pairing] -> IntDisjointSet
makeCircuits ds pairs = foldl (\acc p -> DS.union (id1 p) (id2 p) acc) ds pairs

disjointSetSizes :: IntDisjointSet -> [Int]
disjointSetSizes ds =
  let (dsList, _) = DS.toList ds
   in map snd $ M.toList $ foldl (\acc (_elem, rep) -> M.insertWith (+) rep 1 acc) M.empty dsList

-- | The function which calculates the solution for part two
solve2 :: Int -> IntDisjointSet -> [Pairing] -> Solution
solve2 n ds pairs = connectAll ds $ drop n pairs

connectAll :: IntDisjointSet -> [Pairing] -> Solution
-- connectAll ds [] = DS.disjointSetSize ds
connectAll _ds [] = 0
connectAll ds (x:xs) =
  let n = DS.disjointSetSize ds
      box1 = (id1 x)
      box2 = (id2 x)
      (sameSet, ds') = DS.equivalent box1 box2 ds
      unionSuccessful = not sameSet
      ds'' = DS.union box1 box2 ds'
   in case (unionSuccessful, n) of
        (True, 2) -> (xDist x)
        (True, _) -> connectAll ds'' xs
        (False, _) -> connectAll ds'' xs

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [_part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath
  let (sol1, ds) = solve1 1000 input
  putStr "Day 8 part 1: "
  print sol1
  let (pairs, _) = input
  print $ solve2 1000 ds pairs
