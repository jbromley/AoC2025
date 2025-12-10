module Main where

import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [Int] -- default to Bytestring, but very likely you'll need to change it

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser input = map turnToInt $ lines input
  where
    turnToInt [] = error "no input"
    turnToInt (x:xs)
      | x == 'L' = -read xs
      | x == 'R' = read xs
      | otherwise = error "invalid turn specification"

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 turns = length $ filter (== 0) $ scanl (\acc x -> (acc + x) `mod` 100) 50 turns

step :: (Int, Int) -> Int -> (Int, Int)
step (count, pos) turn = (count + zeros, nextPos)
  where
    nextPos = (pos + turn) `mod` 100
    zeros =
      sum
        [ abs (pos + turn) `quot` 100
        , if pos > 0 && pos + turn < 0
            then 1
            else 0
        , if pos + turn == 0
            then 1
            else 0
        ]

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = fst $ foldl step (0, 50) input

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStr "Day 1 part 1: "
      print $ solve1 input
    else do
      putStr "Day 1 part 2: "
      print $ solve2 input
