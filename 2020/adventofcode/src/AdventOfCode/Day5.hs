module AdventOfCode.Day5 (part1, part2, day5) where
import           AdventOfCode.Types
import           Control.Monad
import           Control.Monad.ST
import qualified Data.List          as List
import Data.Bits
import           GHC.Arr
import           Text.Printf

strToSeatId :: String -> Int
strToSeatId =
  List.foldl' (\seatId n -> shiftL seatId 1 .|. seatToBit n) 0
  where
    seatToBit :: Char -> Int
    seatToBit c =
      case c of
        'F' -> 0
        'L' -> 0
        'B' -> 1
        'R' -> 1
        _ -> error "invalid input"

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
    seats = seatSet $ map strToSeatId (lines i)
  in
    case findEmptySeat seats of
       Nothing     -> putStrLn "no empty seat available"
       Just seatId -> putStrLn $ printf "empty seat id: %d" seatId

part1 :: Puzzle IO ()
part1 = withStringInput $ \i -> do
  let seats = map strToSeatId (lines i)
      maxid = maximum seats
  putStrLn $ printf "maximum seat id: %d" maxid

day5 :: PuzzleDay IO
day5 = PuzzleDay part1 part2
