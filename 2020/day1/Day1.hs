{-# LANGUAGE TupleSections #-}
module Main where
import qualified Data.IntSet        as Set
import qualified Data.List          as List
import           System.Environment (getArgs)
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

part1 :: FilePath -> IO ()
part1 path = do
  nums <- (map read . lines) <$> readFile path
  case twoSum 2020 nums of
    Nothing    -> putStrLn "no pair found"
    Just (a,b) -> putStrLn $ printf "%d * %d = %d" a b (a*b)

part2 :: FilePath -> IO ()
part2 path = do
  nums <- (map read . lines) <$> readFile path
  case threeSum 2020 nums of
    Nothing      -> putStrLn "no pair found"
    Just (a,b,c) -> putStrLn $ printf "%d * %d * %d = %d" a b c (a*b*c)

main :: IO ()
main = do
  (part:input:_) <- getArgs
  case part of
    "part1" -> part1 input
    "part2" -> part2 input
    _       -> error "unknown command"
