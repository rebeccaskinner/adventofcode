{-# LANGUAGE TupleSections #-}

module AdventOfCode.Days.Day1
  ( day1
  , part1
  , part2
  ) where

import AdventOfCode.Types
import qualified Data.IntSet        as Set
import qualified Data.List          as List
import           Text.Printf        (printf)

pairPermutations :: [Int] -> [(Int,Int)]
pairPermutations []     = []
pairPermutations (x:xs) = map (x,) xs <> pairPermutations xs

threeSum :: Int -> [Int] -> Maybe (Int,Int,Int)
threeSum goal nums =
  let
    s = Set.fromList nums
    pairs = pairPermutations nums
    hasCompliment a b = Set.member (goal - (a + b)) s
    res = List.find (uncurry hasCompliment) pairs
  in case res of
    Just (a,b) -> Just (a,b,goal - (a + b))
    Nothing    -> Nothing

twoSum :: Int -> [Int] -> Maybe (Int,Int)
twoSum goal nums =
  List.find (\(a,b) -> a + b == goal) (pairPermutations nums)

part1 :: Puzzle IO ()
part1 = withStringInput $ \contents -> do
  let nums = (map read . lines) contents
  case twoSum 2020 nums of
    Nothing    -> putStrLn "no pair found"
    Just (a,b) -> putStrLn $ printf "%d * %d = %d" a b (a*b)

part2 :: Puzzle IO ()
part2 = withStringInput $ \contents -> do
  let nums = (map read . lines) contents
  case threeSum 2020 nums of
    Nothing      -> putStrLn "no pair found"
    Just (a,b,c) -> putStrLn $ printf "%d * %d * %d = %d" a b c (a*b*c)

day1 :: PuzzleDay IO
day1 = PuzzleDay part1 part2
