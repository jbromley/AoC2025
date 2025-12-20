module Main where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.List (sortBy, subsequences, tails)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import DisjointSetsForest qualified as D
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    a list of 3-D vector with integer components, like V3
- Solution an integer
-}
data Pairing = Pairing {id1, id2, dist2 :: !Int} deriving (Show)

type Input = [Pairing]

type Solution = Int

parser :: ByteString -> Input
parser s =
  let boxes = map parseLine $ B.lines s
   in sortBy
        (comparing dist2)
        [ Pairing i j d
        | (i, [x1, y1, z1]) : xs <- tails (zip [1 ..] boxes),
          (j, [x2, y2, z2]) <- xs,
          let d = sq (x1 - x2) + sq (y1 - y2) + sq (z1 - z2)
        ]

parseLine :: B.ByteString -> [Int]
parseLine line = [i | Just (i, _) <- map B.readInt (B.splitWith (== ',') line)]

sq :: Int -> Int
sq x = x * x

-- | The function which calculates the solution for part one
solve1 :: Int -> [Pairing] -> Solution
solve1 n = product . take 3 . sortBy (flip compare) . map S.size . makeCircuits . take n

makeCircuits :: [Pairing] -> [Set Int]
makeCircuits = foldl' (\acc p -> D.insertSet (S.fromList [id1 p, id2 p]) acc) D.empty

makePair :: [a] -> (a, a)
makePair [x, y] = (x, y); makePair _ = error "pair must have two elements"

combinations :: Int -> [a] -> [(a, a)]
combinations k ns = map makePair $ filter ((k ==) . length) $ subsequences ns

pairToSet :: (Ord a) => (a, a) -> Set a
pairToSet (x, y) = S.fromList [x, y]

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStr "Day 8 part 1: "
      print $ solve1 1000 input
    else do
      putStr "Day 8 part 2: "
      print $ solve2 input
