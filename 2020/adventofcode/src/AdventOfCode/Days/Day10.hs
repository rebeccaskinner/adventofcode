{-# LANGUAGE BangPatterns #-}
module AdventOfCode.Days.Day10 where
import AdventOfCode.Types
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set

contentsToList :: String -> [Int]
contentsToList = sort . map read . lines

breakAt :: (a -> Bool) -> [a] -> ([a],[a])
breakAt p as =
  splitAt (fromMaybe 0 $ findIndex p as) as

indexes :: (a -> Bool) -> [a] -> [Int]
indexes p as =
  let l = pred $ length as
  in filter (\idx -> p $ as !! idx) [0..l]

extractAt :: Int -> [a] -> (a, [a])
extractAt idx vals =
  let val = vals !! idx
      hd = init $ take (succ idx) vals
      tl = drop (succ idx) vals
  in (val, hd<>tl)

stepJolts :: Int -> [Int] -> [[Int]]
stepJolts startingJoltage [] = [[startingJoltage + 3]]
stepJolts startingJoltage adapters =
  let
    fanout :: Int -> [Int] -> [[Int]]
    fanout adapter rest =
      (adapter:) <$> stepJolts adapter rest
    applyAdapterAt :: Int -> [[Int]]
    applyAdapterAt n =
      let (val, rest) = extractAt n adapters
      in fanout val rest
    adapterIndexes = indexes (\j ->
                                let d = (j - startingJoltage)
                                in d > 0 && d <= 3) adapters
  in concatMap applyAdapterAt adapterIndexes

loadAdjacencyMatrix :: [Int] -> Map.Map (Set.IntSet)
loadAdjacencyMatrix =
  let s = fromList


testPart2 = pure 0

calcPath = ((0:) <$>) . stepJolts 0

calcDiffs :: [Int] -> (Int,Int,Int)
calcDiffs [] = (0,0,0)
calcDiffs [n] = (0,0,0)
calcDiffs (x:y:rest) =
  let (a,b,c) = calcDiffs (y:rest)
  in case (y - x) of
       1 -> (a + 1, b, c)
       2 -> (a, b + 1, c)
       3 -> (a, b, c + 1)
       _ -> error "unexpected gap"

part1 :: Puzzle IO ()
part1 = withStringInput $ \i ->
  let nums = contentsToList i
      p = head $ calcPath nums
      (a,_b,c) = calcDiffs p
  in putStrLn $ printf "%d * %d = %d" a c (a * c)

part2 :: Puzzle IO ()
part2 = withStringInput $ \i -> do
  let nums = contentsToList i
      p = nextPaths 0 nums
  putStrLn . show $ length p

day10 :: PuzzleDay IO
day10 = PuzzleDay part1 part2
