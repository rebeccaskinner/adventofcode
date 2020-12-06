module AdventOfCode.Days.Day3
  ( part1
  , part2
  , day3
  ) where
import System.Environment (getArgs)
import AdventOfCode.Types


data Square = Open | Tree deriving (Eq, Show)

parseSquare :: Char -> Square
parseSquare c =
  case c of
    '.' -> Open
    '#' -> Tree
    _ -> error "unsupported square type"

parseRow :: String -> [Square]
parseRow = cycle . map parseSquare

parsePart1Input :: String -> [[Square]]
parsePart1Input = map parseRow . lines

countTrees :: [[Square]] -> Int
countTrees = countTrees' 3 1

dropEvery :: Int -> [a] -> [a]
dropEvery _n [] = []
dropEvery n xs =
  let (hd,tl) = splitAt n xs
  in (head hd) : dropEvery n tl

countTrees' :: Int -> Int -> [[Square]] -> Int
countTrees' rightCnt downCnt =
  length . filter (==Tree) . map head . zipWith drop [0,rightCnt..] . dropEvery downCnt

part1 :: Puzzle IO ()
part1 = withStringInput $ putStrLn . show . countTrees . parsePart1Input

part2 :: Puzzle IO ()
part2 = withStringInput $ \input -> do
  let parsed = parsePart1Input input
      a = countTrees' 1 1 parsed
      b = countTrees' 3 1 parsed
      c = countTrees' 5 1 parsed
      d = countTrees' 7 1 parsed
      e = countTrees' 1 2 parsed

  putStrLn $ "Right 1, Down 1: " <> show (a)
  putStrLn $ "Right 3, Down 1: " <> show (b)
  putStrLn $ "Right 5, Down 1: " <> show (c)
  putStrLn $ "Right 7, Down 1: " <> show (d)
  putStrLn $ "Right 1, Down 2: " <> show (e)
  putStrLn $ "product: " <> show (foldr (*) 1 [a,b,c,d,e])

day3 :: PuzzleDay IO
day3 = PuzzleDay part1 part2
