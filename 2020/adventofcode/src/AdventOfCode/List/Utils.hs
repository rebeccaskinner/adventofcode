module AdventOfCode.List.Utils where
import Data.List

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs =
  let (hd,tl) = splitAt n xs
  in hd : chunksOf n tl
