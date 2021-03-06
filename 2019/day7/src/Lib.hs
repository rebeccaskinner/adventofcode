{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.List as List

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
  , vmInput           :: IO Int
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
step opState@(OpState _ _ _ Nothing _ ) = pure opState
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
      putStrLn "calling handleInstruction"
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
          putStrLn ("input---: " <> show input)
          writeLoc vmState (_instrOperandModes !! 0) (framePtr + 1) input
          pure $ opState{vmFramePtr = Just (framePtr + 2)}
        Output -> do
          outVal <- readLoc vmState (_instrOperandModes !! 0) (framePtr + 1)
          vmOutputInterrupt outVal
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

runAmps :: MVec.IOVector Int -> Int -> [Int] -> IO Int
runAmps prog initialInput (amp:amps)  = do
  let
    run :: OpState -> Int -> IO OpState
    run (OpState{..}) amp = do
      oVal <- outputValue . head <$> IO.readIORef vmOutputs
      o <- outputs
      eval' OpState { vmState = prog
                   , vmFramePtr = Just 0
                   , vmOutputs = o
                   , vmInput = listInput [amp, oVal]
                   , vmOutputInterrupt = const (pure ())
                   }

  o <- outputs
  r <- eval' OpState { vmState    = prog
                    , vmOutputs  = o
                    , vmFramePtr = Just 0
                    , vmInput    = listInput [amp,initialInput]
                    , vmOutputInterrupt = const (pure ())
                    }

  OpState{..} <- Monad.foldM run r amps
  outputValue . head <$> IO.readIORef vmOutputs


runAmps' :: [Int] -> Int -> [Int] -> IO OpState
runAmps' prog initialInput ampVals  = do
  let
    runUntilOutput :: OpState -> IO OpState
    runUntilOutput (st@OpState{..})
      | Nothing == vmFramePtr = pure st
      | otherwise = do
          r     <- IO.newIORef False
          st'   <- step st{vmOutputInterrupt = \_ -> IO.writeIORef r True}
          hadIO <- IO.readIORef r
          if hadIO
            then pure st'
            else runUntilOutput st'

    run :: OpState -> OpState -> IO OpState
    run (OpState{..}) st = do
      oVal <- outputValue . head <$> IO.readIORef vmOutputs
      newState <- runUntilOutput st{vmInput = pure oVal}
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
          lastOutput <- head <$> IO.readIORef vmOutputs
          runLoop (outputValue lastOutput) vals

    emptyOpState :: [Int] -> Int -> IO OpState
    emptyOpState prog seedVal = do
      stVec          <- toVec prog
      outputRegister <- outputs
      runUntilOutput OpState { vmState = stVec
                             , vmOutputs = outputRegister
                             , vmOutputInterrupt = \o -> putStrLn $ "Output: " <>  show o
                             , vmFramePtr = Just 0
                             , vmInput = pure seedVal
                             }

  states <- mapM (emptyOpState prog) ampVals
  putStrLn $ "length of states: " <> show (length states)
  runLoop initialInput states

t :: [Int]
t = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

t' :: [Int]
t' = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

part1 :: IO Int
part1 =
  let inputs = List.permutations [0..4]
  in maximum <$> mapM (runAmps input 0) inputs

part2' :: [Int] -> [Int] -> IO Int
part2' prog args = do
  OpState{..} <- runAmps' prog 0 args
  outVal <- IO.readIORef vmOutputs
  mapM (putStrLn . show) outVal
  pure (outputValue . head $  outVal)

testProg :: [Int] -> IO Int -> IO [OutputData]
testProg l input = do
  v <- toVec l
  o <- outputs
  eval $ OpState { vmState = v
                 , vmOutputs = o
                 , vmFramePtr = Just 0
                 , vmInput = input
                 , vmOutputInterrupt = const (pure ())
                 }
  l' <- Vec.freeze v
  putStrLn . show $ l'
  IO.readIORef o >>= putStrLn . show . reverse
  IO.readIORef o

eval' :: OpState -> IO OpState
eval' initialState = do
  (opState@OpState{..}) <- step initialState
  case vmFramePtr of
    Nothing -> pure opState
    Just n  -> do
      Monad.guard (n <= MVec.length vmState)
      eval' opState

eval :: OpState -> IO Int
eval initialState = do
  (opState@OpState{..}) <- step initialState
  case vmFramePtr of
    Nothing -> MVec.read vmState 0
    Just n  -> do
      Monad.guard (n <= MVec.length vmState)
      eval opState

someFunc :: IO ()
someFunc = pure ()


input :: MVec.IOVector Int
input = Unsafe.unsafePerformIO $ toVec [3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99]
