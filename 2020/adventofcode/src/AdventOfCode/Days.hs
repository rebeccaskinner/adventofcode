module AdventOfCode.Days
  ( day1
  , day2
  , day3
  , day4

  , AdventOfCodeDay (..)
  , AdventOfCodePart (..)
  , lookupPuzzle
  )
where
import AdventOfCode.Day1 (day1)
import AdventOfCode.Day2 (day2)
import AdventOfCode.Day3 (day3)
import AdventOfCode.Day4 (day4)
import AdventOfCode.Types

data AdventOfCodeDay
  = Day1
  | Day2
  | Day3
  | Day4
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data AdventOfCodePart
  = Part1
  | Part2
  deriving (Read, Show, Eq)

getPuzzleForDay :: AdventOfCodeDay -> PuzzleDay IO
getPuzzleForDay d =
  case d of
    Day1 -> day1
    Day2 -> day2
    Day3 -> day3
    Day4 -> day4

puzzleForPart :: AdventOfCodePart -> PuzzleDay IO -> Puzzle IO ()
puzzleForPart (Part1) = puzzleDay1
puzzleForPart (Part2) = puzzleDay2

lookupPuzzle :: AdventOfCodeDay -> AdventOfCodePart -> Puzzle IO ()
lookupPuzzle day part = puzzleForPart part (getPuzzleForDay day)
