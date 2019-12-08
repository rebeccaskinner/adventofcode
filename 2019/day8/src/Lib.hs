module Lib
    ( someFunc
    ) where

import qualified System.IO as IO
import Data.Ord (comparing)
import Data.List (sortBy, transpose)

getInput :: IO [Int]
getInput =
  map (read . pure) . head . lines <$> readFile "input.txt"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = (take n l ) : (chunksOf n (drop n l))

getLayers :: Int -> Int -> [a] -> [[[a]]]
getLayers x y l =
  let rawLayers = chunksOf (x * y) l
  in map (chunksOf x) rawLayers

count :: Eq a => a -> [a] -> Int
count needle =
  foldl (\n hay ->if needle == hay then succ n else n) 0

countLayer :: Eq a => a -> [[a]] -> Int
countLayer needle =
  sum . map (count needle)

part1 :: Int -> Int -> [Int] -> Int
part1 x y l =
  let
    layers :: [[[Int]]]
    layers = getLayers x y l
    layers' :: [(Int,[[Int]])]
    layers' = map (\a -> (countLayer 0 a, a)) layers
    z = snd . head . sortBy (comparing fst) $ layers'
  in (countLayer 1 z) * (countLayer 2 z)

foldColors :: [Int] -> Int
foldColors = foldl1 joinColors
  where
    joinColors 2 n = n
    joinColors n _ = n

toInterstitial :: Int -> Int -> [Int] -> [[[Int]]]
toInterstitial x y l =
  map transpose . transpose . map (chunksOf x) . chunksOf (x * y) $ l

part2' :: Int -> Int -> [Int] -> [[Int]]
part2' x y l =
  map (map foldColors) $ toInterstitial x y l

part2 :: [Int] -> Int -> Int -> IO ()
part2 i x y=
  let rows = part2' x y i
      printRows = map (concatMap showBW) rows
  in mapM_ putStrLn printRows

showBW 2 = " "
showBW 1 = "."
showBW 0 = "#"

someFunc :: IO ()
someFunc = do
  i <- getInput
  print $ part1 25 6 i
  part2 i 25 6
  pure ()
