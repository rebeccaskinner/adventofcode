{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IO
import qualified System.IO.Unsafe as Unsafe

growPut :: a -> Int -> MVec.IOVector a -> IO (MVec.IOVector a)
growPut newVal newIdx vec = do
  let
    oldLen = MVec.length vec
    growBy = oldLen - (newIdx + 1)
    newLen = max oldLen (newIdx + 1)
  vec' <- if newLen > oldLen
          then MVec.grow vec growBy
          else pure vec
  MVec.write vec' newIdx newVal
  pure vec'

data OpState where
  Cont :: MVec.IOVector Int -> OpState
  Term :: Int -> OpState

step :: MVec.IOVector Int -> Int -> IO OpState
step oldstate at = do
  let
    op :: (Int -> Int -> Int) -> IO (MVec.IOVector Int)
    op f = do
        op1Loc <- MVec.read oldstate (at + 1)
        in1    <- MVec.read oldstate op1Loc
        op2Loc <- MVec.read oldstate (at + 2)
        in2    <- MVec.read oldstate op2Loc
        out <- MVec.read oldstate (at + 3)
        growPut (f in1 in2) out oldstate

  instruction <- MVec.read oldstate at

  case instruction of
    99 -> Term <$> MVec.read oldstate 0
    1  -> Cont <$> op (+)
    2  -> Cont <$> op (*)
    i  -> Exception.throw . userError $ "Invalid instruction: " <> show i

memoDict =
  Unsafe.unsafePerformIO $ IO.newIORef Map.empty

toList :: MVec.IOVector Int -> IO [Int]
toList = (Vec.toList <$>) . Vec.freeze

memoStep :: MVec.IOVector Int -> Int -> IO OpState
memoStep st n = do
  d <- IO.readIORef memoDict
  l <- toList st
  let k = (n, l)
  case Map.lookup k d of
    Just st -> pure st
    Nothing -> do
      st' <- step st n
      let d' = Map.insert k st' d
      IO.writeIORef memoDict d'
      pure st'

toVec :: [Int] -> IO (MVec.IOVector Int)
toVec = Vec.thaw . Vec.fromList

run' l = run l 12 2

run :: [Int] -> Int -> Int -> IO Int
run l noun verb =
  toVec l >>= prep noun verb >>= run' 0
  where
    run' :: Int -> MVec.IOVector Int -> IO Int
    run' n state = do
      opSt <- memoStep state n
      case opSt of
        Term n -> pure n
        Cont state' -> run' (n + 4) state'

prep :: Int -> Int -> MVec.IOVector Int -> IO (MVec.IOVector Int)
prep noun verb oldState =
  growPut noun 1 oldState >>= growPut verb 2

prep' :: MVec.IOVector Int -> IO (MVec.IOVector Int)
prep' = prep 12 2

mkArgs :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
mkArgs (minNoun, minVerb) (maxNoun,maxVerb) =
  [(noun,verb) | noun <- [minNoun..maxNoun], verb <- [minVerb..maxVerb]]

argPerms = Vec.fromList $ mkArgs (0,0) (99,99)

findInputs :: Int -> Int -> IO (Maybe (Int,Int))
findInputs n expected
  | n > Vec.length argPerms = pure Nothing
  | otherwise = do
      let (a,b) = argPerms Vec.! n
      actual <- run input a b
      if actual == expected
        then pure $ Just (a,b)
        else findInputs (succ n) expected

findSolution =
  ((\(a,b) -> 100 * a + b) <$>) <$> findInputs 0 19690720

input :: [Int]
input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0]

someFunc :: IO ()
someFunc = findSolution >>= putStrLn . show
