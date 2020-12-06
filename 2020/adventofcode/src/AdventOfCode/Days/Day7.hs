module AdventOfCode.Days.Day7 (part1,part2,day7) where
import AdventOfCode.Types

part1 :: Puzzle IO ()
part1 = withTextInput $ \i -> do
  putStrLn "Day 6 - Part 1"

part2 :: Puzzle IO ()
part2 = withTextInput $ \i -> do
  putStrLn "Day 6 - Part 2"

day7 :: PuzzleDay IO
day7 = PuzzleDay part1 part2
