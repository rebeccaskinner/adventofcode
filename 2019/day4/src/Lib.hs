module Lib
    ( someFunc
    ) where

import qualified Control.Monad       as Monad
import qualified Control.Monad.ST    as ST
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import qualified Debug.Trace         as Debug

checkNumber :: [Int] -> Bool
checkNumber (n:ns) =
  let (_, hasDouble, isIncreasing) = foldl checkDigit (n,False,True) ns
  in hasDouble && isIncreasing
  where
    checkDigit (prevNum, hasDouble, isIncreasing) nextNum =
      ( nextNum
      , hasDouble || prevNum == nextNum
      , isIncreasing && nextNum >= prevNum
      )

checkNumber' :: [Int] -> Bool
checkNumber' (n:ns) = ST.runST $ do
  stVec <- MVec.new 10
  MVec.set stVec 0
  MVec.write stVec n 1
  let baseState = (n, True)
  (_, isIncreasing) <- Monad.foldM (checkDigit stVec) baseState ns
  v <- Vec.freeze stVec
  pure $ isIncreasing && Vec.elem 2 v
  where
    checkDigit stVec (prevNum, isIncreasing) nextNum = do
      if prevNum == nextNum
        then MVec.modify stVec succ prevNum
        else MVec.write stVec nextNum 1
      pure $ (nextNum, isIncreasing && nextNum >= prevNum)

arrayify :: Int -> [Int]
arrayify =
  map (read . pure) . show

validate :: Int -> Bool
validate = checkNumber . arrayify

validate' :: Int -> Bool
validate' = checkNumber' . arrayify

validateRange :: Int -> Int -> [Int]
validateRange start end =
  filter validate [start..end]

someFunc :: IO ()
someFunc = do
  let r1 = validateRange 171309 643603
      r2 = filter validate' r1
  putStrLn $ "Part 1: " <> (show . length $ r1)
  putStrLn $ "Part 1: " <> (show . length $ r2)
