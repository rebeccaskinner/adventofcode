{-# LANGUAGE BangPatterns #-}
module AdventOfCode.Days.Day10 where
import           AdventOfCode.Types
import           Control.Monad.ST
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet        as Set
import Debug.Trace
import           Data.List
import           Data.Maybe
import           GHC.STRef
import           Text.Printf

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

setElems :: [Int] -> Set.IntSet -> [Int]
setElems items s =
  filter (`Set.member` s) items

loadAdjacencyMatrix :: [Int] -> Map.IntMap Set.IntSet
loadAdjacencyMatrix items =
  let s = Set.fromList items
  in Set.foldl (addEdges s) Map.empty s
  where
    addEdges :: Set.IntSet -> Map.IntMap (Set.IntSet) -> Int -> Map.IntMap (Set.IntSet)
    addEdges refLst m n =
      let candidateEdges = Set.fromList [n + 1, n + 2, n + 3]
          edges = Set.intersection refLst candidateEdges
      in Map.insert n edges m

-- calculate incoming edges rather than outgoing
inverseAdjacencyMatrix :: [Int] -> Map.IntMap (Set.IntSet)
inverseAdjacencyMatrix items =
  let s = Set.fromList items
  in Set.foldl (addEdges s) Map.empty s
  where
    addEdges :: Set.IntSet -> Map.IntMap (Set.IntSet) -> Int -> Map.IntMap (Set.IntSet)
    addEdges refLst m n =
      let candidateEdges = Set.fromList [n - 1, n - 2, n - 3]
          edges = Set.intersection refLst candidateEdges
      in Map.insert n edges m

countPaths :: Map.IntMap Set.IntSet -> Int -> Int -> Int
countPaths gr start end = runST $ do
  cache <- newSTRef Map.empty
  let
    step start'
      | start' == end = pure 1
      | otherwise = do
          cache' <- readSTRef cache
          case Map.lookup start' cache' of
            Just cachedValue -> pure cachedValue
            Nothing -> do
              case Map.lookup start' gr of
                Nothing -> pure 0
                Just elems -> do
                  v <- sum <$> (mapM step (Set.toList elems))
                  writeSTRef cache (Map.insert start' v cache')
                  pure v
  step start

countPaths' :: [Int] -> Int
countPaths' adapters =
  let m = maximum adapters
      cache = (Map.singleton m 1, 0)
      s = Set.fromList adapters
  in snd $ foldr (f s) cache (init adapters)
  where
    edges :: Int -> Set.IntSet -> [Int]
    edges idx s =
      let
        s' = Set.fromList [idx + 1, idx + 2, idx + 3]
      in Set.toList $ Set.intersection s s'

    fetchEdgeWeight :: Map.IntMap Int -> Int -> Int
    fetchEdgeWeight m k = fromMaybe 0 $ Map.lookup k m

    f :: Set.IntSet -> Int -> (Map.IntMap Int,Int) -> (Map.IntMap Int,Int)
    f adj joltage (cache, _s) =
      let
        e = edges joltage adj
        w = sum $ map (fetchEdgeWeight cache) e
        cache' = Map.insert joltage w cache
      in (cache', w)

calcPath = ((0:) <$>) . stepJolts 0

calcDiffs :: [Int] -> (Int,Int,Int)
calcDiffs [] = (0,0,0)
calcDiffs [_] = (0,0,0)
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
  let l = 0:(contentsToList i)
      gr = inverseAdjacencyMatrix l
  putStrLn . show $ countPaths gr (maximum l) 0
--  putStrLn . show $ countPaths' l

day10 :: PuzzleDay IO
day10 = PuzzleDay part1 part2
