module AdventOfCode.Days.Day22 (part1,part2,day22) where
import AdventOfCode.Types

part1 :: Puzzle IO ()
part1 = withTextInput $ \i -> do
  putStrLn "Day 6 - Part 1"

part2 :: Puzzle IO ()
part2 = withTextInput $ \i -> do
  putStrLn "Day 6 - Part 2"

day22 :: PuzzleDay IO
day22 = PuzzleDay part1 part2
