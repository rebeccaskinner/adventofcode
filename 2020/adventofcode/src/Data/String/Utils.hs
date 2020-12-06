module Data.String.Utils where

groupByNonEmpty :: [String] -> [[String]]
groupByNonEmpty = groupByNonEmpty' [] []
  where
    groupByNonEmpty' :: [String] -> [[String]] -> [String] -> [[String]]
    groupByNonEmpty' buff acc [] = reverse ((reverse buff) : acc)
    groupByNonEmpty' buff acc (x:xs)
      | null x = groupByNonEmpty' [] ((reverse buff):acc) xs
      | otherwise = groupByNonEmpty' (x:buff) acc xs

breakAtEmptyLines :: String -> [String]
breakAtEmptyLines = map unlines . groupByNonEmpty . lines

segmentsByEmptyLine :: String -> [String]
segmentsByEmptyLine = map concatLines . filter (not . null) . breakAtEmptyLines

concatLines = concat . lines
