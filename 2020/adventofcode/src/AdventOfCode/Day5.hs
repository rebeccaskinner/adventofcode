module AdventOfCode.Day5 where
import           AdventOfCode.Types
import           Control.Monad
import           Data.Ord
import           Text.Printf
import Control.Monad.ST
import GHC.Arr
import qualified Data.List as List

binSearch :: (a -> Ordering) -> Int -> Int -> [a] -> Int
binSearch cmp l u [] = u
binSearch cmp l u (x:xs) =
  let
    center = ((l + u) `div` 2)
  in case cmp x of
    GT -> binSearch cmp center u xs
    LT -> binSearch cmp l center xs
    _  -> error "binSearch got an unexpected comparison"

seatOrdering :: Char -> Ordering
seatOrdering c =
  case c of
    'F' -> LT
    'L' -> LT
    'B' -> GT
    'R' -> GT
    _   -> error "invalid input"

findSeatPart1 :: String -> (Int,Int)
findSeatPart1 s =
  let (row,col) = splitAt 7 s
  in (binSearch seatOrdering 0 127 row, binSearch seatOrdering 0 7 col)

seatId :: Int -> Int -> Int
seatId row col = (row * 8) + col

part1 :: Puzzle IO ()
part1 = withStringInput $ \i -> do
  let seats = lines i
  ids <- forM seats $ \seat -> do
          let (row,col) = findSeatPart1 seat
              i = seatId row col
          putStrLn $ printf "%s: row %d, column %d, seat ID: %d" seat row col i
          pure i
  let maxid = maximum ids
  putStrLn $ printf "maximum seat id: %d" maxid

maxSeatId = (127 * 8 + 7)

seatSet :: [Int] -> Array Int Bool
seatSet seats = runST $ do
  arr <- thawSTArray $ listArray (0,maxSeatId) (repeat False)
  let
    addSeatId idx = writeSTArray arr idx True
  forM_ seats $ \seat -> addSeatId seat
  freezeSTArray arr

findEmptySeat :: Array Int Bool -> Maybe Int
findEmptySeat arr =
  let
    (min,max) = bounds arr
    populatedNeighbors n
      | n <= min || n >= max = False
      | otherwise = (arr ! (n - 1)) && (arr ! (n + 1))
    isEmpty n =
      (not $ arr ! n) && (populatedNeighbors n)
  in List.find isEmpty [min..max]

part2 :: Puzzle IO ()
part2 = withStringInput $ \i ->
  let
    seats = seatSet $ map (uncurry seatId . findSeatPart1) (lines i)
  in
    case findEmptySeat seats of
       Nothing -> putStrLn "no empty seat available"
       Just seatId -> putStrLn $ printf "empty seat id: %d" seatId

day5 :: PuzzleDay IO
day5 = PuzzleDay part1 part2
