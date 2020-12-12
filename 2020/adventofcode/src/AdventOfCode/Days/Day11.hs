{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module AdventOfCode.Days.Day11 (part1,part2,day11) where
import AdventOfCode.Types
import AdventOfCode.List.Utils
import Data.List (unfoldr, foldl')
import GHC.Arr
import Control.Monad
import Data.Foldable
import Data.Function
import qualified Data.Map.Strict as Map

data Tile = Floor | OpenSeat | OccupiedSeat deriving (Eq, Show)
data Board = Board { boardRows  :: !Int
                   , boardCols  :: !Int
                   , boardData  :: !(Array Int Tile) -- ![Tile]
                   , boardCache :: Map.Map Int [Int]
                   } deriving (Eq, Show)

data BoardStats = BoardStats { totalTiles    :: !Int
                             , totalFloor    :: !Int
                             , totalOpen     :: !Int
                             , totalOccupied :: !Int
                             } deriving Show

boardStats :: Board -> BoardStats
boardStats = foldl' (flip updateBoardStats) (BoardStats 0 0 0 0) . boardData
  where
    updateBoardStats :: Tile -> BoardStats -> BoardStats
    updateBoardStats t BoardStats{..} =
      case t of
        Floor ->
          BoardStats (succ totalTiles) (succ totalFloor) totalOpen totalOccupied
        OpenSeat ->
          BoardStats (succ totalTiles) totalFloor (succ totalOpen) totalOccupied
        OccupiedSeat ->
          BoardStats (succ totalTiles) totalFloor totalOpen (succ totalOccupied)

parseTile :: Char -> Maybe Tile
parseTile c =
  case c of
    'L' -> Just OpenSeat
    '#' -> Just OccupiedSeat
    '.' -> Just Floor
    _   -> Nothing

showTileChar :: Tile -> Char
showTileChar t =
  case t of
    Floor -> '.'
    OccupiedSeat -> '#'
    OpenSeat -> 'L'

parseBoard :: String -> Board
parseBoard contents =
  let
    contents' = lines contents
    (Just tiles) = mapM (mapM parseTile) contents'
    cols = length . head $ contents'
    rows = length contents'
    l = pred (rows * cols)
  in Board
     { boardRows = rows
     , boardCols = cols
     , boardData = listArray (0,l) (concat tiles)
     , boardCache = Map.empty
     } & updateBoardCache

showBoard :: Board -> String
showBoard (Board _rows cols tiles _cache) =
  let
    chars = map showTileChar (elems tiles)
  in unlines $ chunksOf cols chars

tileToNum :: Tile -> Int
tileToNum t = if (t == OccupiedSeat) then 1 else 0

toIdx :: Num a => a -> a -> a -> a
toIdx stride row col = (row * stride) + col

updateBoardCache :: Board -> Board
updateBoardCache b =
  let l = pred $ (boardRows b) * (boardCols b)
      cache = foldl' (updateNeighborCache b) (boardCache b) [0..l]
  in b { boardCache = cache }
  where
    updateNeighborCache :: Board -> Map.Map Int [Int] -> Int -> Map.Map Int [Int]
    updateNeighborCache b cache idx =
      Map.insert idx (neighbors idx b) cache

    neighbors :: Int -> Board -> [Int]
    neighbors idx Board{..} =
      let
        (row, col) = idx `divMod` boardCols
      in [ (toIdx boardCols r c)
         | r <- [row - 1 .. row + 1]
         , c <- [col - 1 .. col + 1]
         , toIdx boardCols r c /= idx
         , r >= 0 && r < boardRows
         , c >= 0 && c < boardCols
         ]

neighbors :: Int -> Board -> [Tile]
neighbors idx b =
  map (\idx -> (boardData b) ! idx) $ (boardCache b) Map.! idx

occupiedNeighbors :: Int -> Board -> Int
occupiedNeighbors idx b =
  sum . map tileToNum $ neighbors idx b

stepTile :: Board -> Map.Map Int [Tile] -> Int -> Map.Map Int [Tile]
stepTile b m idx  =
  let n = (boardCache b) Map.! idx
      c = (boardData b) ! idx
      addTileToNeighbor nList =
        case nList of
          Nothing -> Just [c]
          Just items -> Just (c:items)
  in foldl' (\m' idx -> Map.alter addTileToNeighbor idx m') m n

stepBoard :: Board -> Board
stepBoard b =
  let
    l = pred $ (boardRows b) * (boardCols b)
    neighborMap = foldl' (stepTile b) Map.empty [0..l]
    tiles = amap (updateTile b) (listArray (0,l) $ Map.toList neighborMap)
  in b { boardData = tiles }
  where
    updateTile :: Board -> (Int, [Tile]) -> Tile
    updateTile b (idx, neighbors) =
      let cnt = sum . map tileToNum $ neighbors
      in case (boardData b ! idx) of
           Floor -> Floor
           OpenSeat ->
             if cnt == 0 then OccupiedSeat else OpenSeat
           OccupiedSeat ->
             if cnt >= 4 then OpenSeat else OccupiedSeat

fixBoard :: Board -> Board
fixBoard = fix $ \rec b ->
  let b' = stepBoard b
  in if b == b' then b else rec b'

part1 :: Puzzle IO ()
part1 = withStringInput $ \i -> do
  let b = fixBoard $ parseBoard i
  putStrLn . show . boardStats $ b

part2 :: Puzzle IO ()
part2 = withStringInput $ \i -> do
  putStrLn "Day 6 - Part 2"

day11 :: PuzzleDay IO
day11 = PuzzleDay part1 part2
