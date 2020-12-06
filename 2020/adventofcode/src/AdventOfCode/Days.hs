module AdventOfCode.Days
  ( day1
  , day2
  , day3
  , day4
  , day5
  , day6

  , AdventOfCodeDay (..)
  , AdventOfCodePart (..)
  , lookupPuzzle
  )
where
import           AdventOfCode.Days.Day1  (day1)
import           AdventOfCode.Days.Day10 (day10)
import           AdventOfCode.Days.Day11 (day11)
import           AdventOfCode.Days.Day12 (day12)
import           AdventOfCode.Days.Day13 (day13)
import           AdventOfCode.Days.Day14 (day14)
import           AdventOfCode.Days.Day15 (day15)
import           AdventOfCode.Days.Day16 (day16)
import           AdventOfCode.Days.Day17 (day17)
import           AdventOfCode.Days.Day18 (day18)
import           AdventOfCode.Days.Day19 (day19)
import           AdventOfCode.Days.Day2  (day2)
import           AdventOfCode.Days.Day20 (day20)
import           AdventOfCode.Days.Day21 (day21)
import           AdventOfCode.Days.Day22 (day22)
import           AdventOfCode.Days.Day23 (day23)
import           AdventOfCode.Days.Day24 (day24)
import           AdventOfCode.Days.Day25 (day25)
import           AdventOfCode.Days.Day3  (day3)
import           AdventOfCode.Days.Day4  (day4)
import           AdventOfCode.Days.Day5  (day5)
import           AdventOfCode.Days.Day6  (day6)
import           AdventOfCode.Days.Day7  (day7)
import           AdventOfCode.Days.Day8  (day8)
import           AdventOfCode.Days.Day9  (day9)

import           AdventOfCode.Types

data AdventOfCodeDay
  = Day1
  | Day2
  | Day3
  | Day4
  | Day5
  | Day6
  | Day7
  | Day8
  | Day9
  | Day10
  | Day11
  | Day12
  | Day13
  | Day14
  | Day15
  | Day16
  | Day17
  | Day18
  | Day19
  | Day20
  | Day21
  | Day22
  | Day23
  | Day24
  | Day25
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data AdventOfCodePart
  = Part1
  | Part2
  deriving (Read, Show, Eq)

getPuzzleForDay :: AdventOfCodeDay -> PuzzleDay IO
getPuzzleForDay d =
  case d of
    Day1  -> day1
    Day2  -> day2
    Day3  -> day3
    Day4  -> day4
    Day5  -> day5
    Day6  -> day6
    Day7  -> day7
    Day8  -> day8
    Day9  -> day9
    Day10 -> day10
    Day11 -> day11
    Day12 -> day12
    Day13 -> day13
    Day14 -> day14
    Day15 -> day15
    Day16 -> day16
    Day17 -> day17
    Day18 -> day18
    Day19 -> day19
    Day20 -> day20
    Day21 -> day21
    Day22 -> day22
    Day23 -> day23
    Day24 -> day24
    Day25 -> day25

puzzleForPart :: AdventOfCodePart -> PuzzleDay IO -> Puzzle IO ()
puzzleForPart (Part1) = puzzleDay1
puzzleForPart (Part2) = puzzleDay2

lookupPuzzle :: AdventOfCodeDay -> AdventOfCodePart -> Puzzle IO ()
lookupPuzzle day part = puzzleForPart part (getPuzzleForDay day)
