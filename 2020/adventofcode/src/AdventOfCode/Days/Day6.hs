module AdventOfCode.Days.Day6 (part1,part2,day6) where
import AdventOfCode.Types
import Data.String.Utils
import GHC.Arr
import GHC.STRef
import Control.Monad
import Control.Monad.ST


countGroupAnswers :: Int -> String -> Int
countGroupAnswers groupSize t = runST $ do
  cnt <- newSTRef 0
  arr <- thawSTArray $ listArray ('a','z') (repeat 0)
  forM_ t $ \c -> do
    seen <- readSTArray arr c
    let seen' = succ seen
    writeSTArray arr c seen'
    when (seen' == groupSize) $ do
      c <- readSTRef cnt
      writeSTRef cnt (succ c)
  readSTRef cnt

groupsWithLineCount :: String -> [(Int,String)]
groupsWithLineCount s =
  let
    groups = map lines . breakAtEmptyLines $ s
  in map (\g -> (length g, concat g)) groups

part1 :: Puzzle IO ()
part1 = withStringInput $ \i -> do
  let groups = segmentsByEmptyLine i
      total = sum $ map (countGroupAnswers 1) groups
  putStrLn $ "total: " <> show total

part2 :: Puzzle IO ()
part2 = withStringInput $ \i -> do
  let
    groups = groupsWithLineCount i
    total = sum $ map (uncurry countGroupAnswers) groups
  putStrLn $ "total: " <> show total

day6 :: PuzzleDay IO
day6 = PuzzleDay part1 part2
