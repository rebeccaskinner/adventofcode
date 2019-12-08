{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Part2
    ( someFunc
    ) where

import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import qualified Data.IORef          as IO
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe          as Maybe
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import qualified System.IO.Unsafe    as Unsafe

growPut :: Int -> Int -> MVec.IOVector Int -> IO (MVec.IOVector Int)
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

listInput :: [Int] -> IO Int
listInput l = do
  let
    r :: IO.IORef [Int]
    r = Unsafe.unsafePerformIO $ IO.newIORef (cycle l)
  l' <- IO.readIORef r
  IO.modifyIORef r tail
  pure $ head l'

data OpState = OpState
  { vmState           :: MVec.IOVector Int
  , vmOutputs         :: IO.IORef [OutputData]
  , vmOutputInterrupt :: Int -> IO ()
  , vmFramePtr        :: Maybe Int
  , vmInput           :: Maybe Int
  , vmBlocked         :: Bool
  }

showOpState :: OpState -> IO String
showOpState (OpState{..}) = do
  out <- show <$> IO.readIORef vmOutputs
  st  <- show <$> Vec.freeze vmState
  let framePointer = case vmFramePtr of
                       Nothing -> "<TERM>"
                       Just n  -> "n"
  pure . unlines $ [ "framePointer: " <> framePointer , "vmState:" , st , "outputs:" , out]

data AccessMode = Position
                | Immediate deriving (Eq, Show)

parseAccessMode :: Char -> AccessMode
parseAccessMode c =
  case c of
    '0' -> Position
    '1' -> Immediate

atAddr :: AccessMode -> MVec.IOVector Int -> Int -> (MVec.IOVector Int -> Int -> IO a) -> IO a
atAddr Immediate vec idx f = f vec idx
atAddr Position vec idx f  = MVec.read vec idx >>= f vec

data Op = Add
        | Mult
        | Exit
        | Input
        | Output
        | JumpTrue
        | JumpFalse
        | StoreLT
        | StoreEq
        deriving (Eq, Show, Enum)

data Instruction = Instruction
  { _instrOp           :: Op
  , _instrArity        :: Int
  , _instrOperandModes :: [AccessMode]
  } deriving (Eq, Show)

parseInstruction :: Int -> Instruction
parseInstruction instr =
  let [a,b,c,d,e] = take 5 $ (reverse $ show instr) <> repeat '0'
      operModes = map parseAccessMode [c,d,e]
      mkInstr op arity = Instruction { _instrOp = op, _instrArity = arity, _instrOperandModes = operModes }
  in case [b,a] of
       "99" -> mkInstr Exit 0
       "01" -> mkInstr Add 3
       "02" -> mkInstr Mult 3
       "03" -> mkInstr Input 1
       "04" -> mkInstr Output 1
       "05" -> mkInstr JumpTrue 2
       "06" -> mkInstr JumpFalse 2
       "07" -> mkInstr StoreLT 3
       "08" -> mkInstr StoreEq 3
       e    -> error $ "unexpected instruction: " <> e

readLoc :: MVec.IOVector Int -> AccessMode -> Int -> IO Int
readLoc vec mode idx = do
  case mode of
    Immediate -> MVec.read vec idx
    Position  -> MVec.read vec idx >>= MVec.read vec

writeLoc :: MVec.IOVector Int -> AccessMode -> Int -> Int ->  IO (MVec.IOVector Int)
writeLoc vec mode idx val =
  case mode of
    Immediate -> growPut val idx vec
    Position  -> do
      idx' <- MVec.read vec idx
      writeLoc vec Immediate idx' val

data OutputData = OutputData
  { framePointer :: Int
  , outputValue  :: Int
  , programState :: Vec.Vector Int
  } deriving (Eq, Show)

outputs :: IO (IO.IORef [OutputData])
outputs = IO.newIORef []

outputSuccessful :: OutputData -> Bool
outputSuccessful = (0 ==) . outputValue

step :: OpState -> IO OpState
step opState@(OpState _ _ _ Nothing _ _) = pure opState
step (opState@OpState{..}) = do
  let
    (Just framePtr) = vmFramePtr
    binOp :: (Int -> Int -> Int)
          -> (AccessMode, AccessMode, AccessMode)
          -> IO (MVec.IOVector Int)

    binOp f (mode1, mode2, mode3) = do
        in1 <- readLoc vmState mode1 (framePtr + 1)
        in2 <- readLoc vmState mode2 (framePtr + 2)
        writeLoc vmState mode3 (framePtr + 3) (f in1 in2)

    handleInstruction :: Instruction -> IO OpState
    handleInstruction instruction@Instruction{..} = do
      case _instrOp of
        Exit  -> pure $ opState{vmFramePtr = Nothing}
        Add   -> do
          binOp (+) (_instrOperandModes !! 0, _instrOperandModes !! 1, _instrOperandModes !! 2)
          pure $ opState{vmFramePtr = Just (framePtr + 4)}
        Mult  -> do
          binOp (*) (_instrOperandModes !! 0, _instrOperandModes !! 1, _instrOperandModes !! 2)
          pure $ opState{vmFramePtr = Just (framePtr + 4)}
        Input -> do
          case vmInput of
            Nothing -> do
              pure opState{vmBlocked = True}
            Just input -> do
              writeLoc vmState (_instrOperandModes !! 0) (framePtr + 1) input
              pure $ opState{vmFramePtr = Just (framePtr + 2), vmInput = Nothing, vmBlocked = False}
        Output -> do
          outVal <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          vmOutputInterrupt outVal
          st <- Vec.freeze vmState
          let outputData = OutputData { framePointer = framePtr
                                      , outputValue  = outVal
                                      , programState =  st}
          IO.modifyIORef' vmOutputs (outputData :)
          pure $ opState{vmFramePtr = Just (framePtr + 2)}
        JumpTrue -> do
          test      <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          framePtr' <- if test == 0
                       then pure $ framePtr + 3
                       else readLoc vmState (_instrOperandModes !! 1) (framePtr + 2)
          pure $ opState{vmFramePtr = Just framePtr'}
        JumpFalse -> do
          test      <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          framePtr' <- if test /= 0
                       then pure $ framePtr + 3
                       else readLoc vmState (_instrOperandModes !! 1) (framePtr + 2)
          pure $ opState{vmFramePtr = Just framePtr'}
        StoreLT -> do
          a <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          b <- readLoc vmState (_instrOperandModes !! 1) (framePtr + 2)
          let outputVal = if a < b then 1 else 0
          writeLoc vmState (_instrOperandModes !! 2) (framePtr + 3) outputVal
          pure opState{vmFramePtr = Just (framePtr + 4)}
        StoreEq -> do
          a <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          b <- readLoc vmState (_instrOperandModes !! 1) (framePtr + 2)
          let outputVal = if a == b then 1 else 0
          writeLoc vmState (_instrOperandModes !! 2) (framePtr + 3) outputVal
          pure opState{vmFramePtr = Just (framePtr + 4)}

  opCode <- MVec.read vmState framePtr
  let parsed = parseInstruction opCode
  handleInstruction parsed

toVec :: [Int] -> IO (MVec.IOVector Int)
toVec = Vec.thaw . Vec.fromList

runAmps' :: [Int] -> Int -> [Int] -> IO OpState
runAmps' prog initialInput ampVals  = do
  let
    runUntilOutput :: OpState -> IO OpState
    runUntilOutput st
      | (Nothing == vmFramePtr st) = pure st
      | otherwise = do
          st' <- step st
          if vmBlocked st'
            then pure st'
            else runUntilOutput st'

    run :: OpState -> OpState -> IO OpState
    run oldSt st = do
      !oVal <- Maybe.listToMaybe . map outputValue <$> IO.readIORef (vmOutputs oldSt)
      newState <- runUntilOutput st{vmInput = oVal}
      pure newState

    loopBody :: Int -> [OpState] -> IO OpState
    loopBody input (c:cs) = do
      c' <- runUntilOutput c{vmInput = pure input}
      Monad.foldM run c' cs

    runLoop :: Int -> [OpState] -> IO OpState
    runLoop seedInput vals = do
      out@OpState{..} <- loopBody seedInput vals
      case vmFramePtr of
        Nothing -> pure out
        Just _  -> do
          !lastOutput <- head <$> IO.readIORef vmOutputs
          runLoop (outputValue lastOutput) vals

    emptyOpState :: [Int] -> Int -> IO OpState
    emptyOpState prog seedVal = do
      stVec          <- toVec prog
      outputRegister <- outputs
      runUntilOutput $ OpState { vmState = stVec
                     , vmOutputs = outputRegister
                     , vmOutputInterrupt = const (pure ())
                     , vmFramePtr = Just 0
                     , vmInput = pure seedVal
                     , vmBlocked = False
                     }

  states <- mapM (emptyOpState prog) ampVals
  runLoop initialInput states

t :: [Int]
t = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
i :: [Int]
i = [9,8,7,6,5]

t' :: [Int]
t' = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
i' :: [Int]
i' = [9,7,8,5,6]

part2' :: [Int] -> [Int] -> IO Int
part2' prog args = do
  OpState{..} <- runAmps' prog 0 args
  outVal <- IO.readIORef vmOutputs
  pure (outputValue . head $  outVal)

someFunc :: IO ()
someFunc = pure ()


input :: MVec.IOVector Int
input = Unsafe.unsafePerformIO $ toVec [3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99]
