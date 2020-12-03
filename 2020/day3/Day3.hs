module Main where
import System.Environment (getArgs)
import Data.List

data Square = Open | Tree deriving (Eq, Show)

parseSquare :: Char -> Square
parseSquare c =
  case c of
    '.' -> Open
    '#' -> Tree
    _ -> error "unsupported square type"

showSquare :: Square -> Char
showSquare Open = '.'
showSquare Tree = '#'

parseRow :: String -> [Square]
parseRow = cycle . map parseSquare

parsePart1Input :: String -> [[Square]]
parsePart1Input = map parseRow . lines

countTrees :: [[Square]] -> Int
countTrees  = length . filter (==Tree) . map head . zipWith drop [0,3..]

dropEvery :: Int -> [a] -> [a]
dropEvery n [] = []
dropEvery n xs =
  let (hd,tl) = splitAt n xs
  in (head hd) : dropEvery n tl

countTrees' :: Int -> Int -> [[Square]] -> Int
countTrees' rightCnt downCnt squares =
  let
    withDownCount = dropEvery downCnt squares
    withRightCount = zipWith drop [0,rightCnt..] withDownCount
  in length . filter (==Tree) . map head $ withRightCount
--  length . filter (==Tree) . map head . dropEvery downCnt . zipWith drop [0,rightCnt..]

part1 :: String -> IO ()
part1 = putStrLn . show . countTrees . parsePart1Input

part2 :: String -> IO ()
part2 input = do
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

main :: IO ()
main = do
  [part, fname] <- getArgs
  contents <- readFile fname
  case part of
    "part1" -> part1 contents
    "part2" -> part2 contents
    _ -> fail $ "unknown part: " <> part
