{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import qualified Data.IORef          as IO
import qualified Data.Map.Strict     as Map
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

data OpState = OpState
  { vmState     :: MVec.IOVector Int
  , vmOutputs   :: IO.IORef [OutputData]
  , vmFramePtr  :: Maybe Int
  , vmInput     :: IO Int
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
       e -> error $ "unexpected instruction: " <> e

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
step opState@(OpState _ _ Nothing _ ) = pure opState
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
          input <- vmInput
          writeLoc vmState (_instrOperandModes !! 0) (framePtr + 1) input
          pure $ opState{vmFramePtr = Just (framePtr + 2)}
        Output -> do
          outVal <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          st <- Vec.freeze vmState
          let outputData = OutputData { framePointer = framePtr
                                      , outputValue  = outVal
                                      , programState =  st}
          IO.modifyIORef vmOutputs (outputData :)
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

testProg :: [Int] -> IO Int -> IO [OutputData]
testProg l input = do
  v <- toVec l
  o <- outputs
  eval $ OpState { vmState = v
                 , vmOutputs = o
                 , vmFramePtr = Just 0
                 , vmInput = input }
  l' <- Vec.freeze v
  putStrLn . show $ l'
  IO.readIORef o >>= putStrLn . show . reverse
  IO.readIORef o

eval :: OpState -> IO Int
eval initialState = do
  (opState@OpState{..}) <- step initialState
  case vmFramePtr of
    Nothing -> MVec.read vmState 0
    Just n  -> do
      Monad.guard (n <= MVec.length vmState)
      eval opState

input :: [Int]
input = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,71,224,1001,224,-111,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1102,66,6,225,1102,22,54,225,1,65,35,224,1001,224,-86,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,20,80,225,101,92,148,224,101,-162,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1102,63,60,225,1101,32,48,225,2,173,95,224,1001,224,-448,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,1001,91,16,224,101,-79,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1101,13,29,225,1101,71,70,225,1002,39,56,224,1001,224,-1232,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,14,59,225,102,38,143,224,1001,224,-494,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,30,28,224,1001,224,-840,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,359,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,434,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,449,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,599,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]

someFunc :: IO ()
someFunc = pure ()
