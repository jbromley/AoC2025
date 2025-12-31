module Main where

import Data.ByteString.Char8 qualified as B
import Data.List (tails)
import System.Environment (getArgs)

type Input = [(Int, Int)]

type Solution = Int

parser :: B.ByteString -> Input
parser = map (listToPair . parseLine) . B.lines

parseLine :: B.ByteString -> [Int]
parseLine line = [i | Just (i, _) <- map B.readInt (B.splitWith (== ',') line)]

listToPair :: [Int] -> (Int, Int)
listToPair [x, y] = (x, y)
listToPair _ = error "listToPair only accepts two element lists"

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = maximum . map area . combinations

combinations :: [a] -> [(a, a)]
combinations xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

area :: ((Int, Int), (Int, Int)) -> Int
area ((x1, y1), (x2, y2)) = dx * dy
  where
    dx = abs (x2 - x1) + 1
    dy = abs (y2 - y1) + 1

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filePath] <- getArgs
  input <- parser <$> B.readFile filePath
  if read @Int part == 1
    then do
      putStr "Day 08 part 1: "
      print $ solve1 input
    else do
      putStr "Day 08 part 2: "
      print $ solve2 input
