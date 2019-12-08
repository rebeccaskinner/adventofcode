{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Part2
    ( someFunc
    ) where

import qualified Data.IORef          as IO
import qualified Control.Exception   as Exception
import qualified Control.Monad       as Monad
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe          as Maybe
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import qualified System.IO.Unsafe    as Unsafe
import qualified Data.IntMap.Strict as IntMap

data OpState = OpState
  { vmState           :: IntMap.IntMap Int
  , vmOutputs         :: Maybe Int
  , vmFramePtr        :: Maybe Int
  , vmInput           :: Maybe Int
  , vmBlocked         :: Bool
  } deriving (Eq, Show)

data AccessMode = Position
                | Immediate deriving (Eq, Show)

parseAccessMode :: Char -> AccessMode
parseAccessMode c =
  case c of
    '0' -> Position
    '1' -> Immediate

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
parseInstruction !instr =
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

readLoc :: IntMap.IntMap Int -> AccessMode -> Int -> Int
readLoc vec mode idx = do
  case mode of
    Immediate -> vec IntMap.! idx
    Position  -> vec IntMap.! (vec IntMap.! idx)

writeLoc :: IntMap.IntMap Int -> AccessMode -> Int -> Int ->  IntMap.IntMap Int
writeLoc vec mode idx val =
  case mode of
    Immediate -> IntMap.insert idx val vec
    Position  -> IntMap.insert (vec IntMap.! idx) val vec

step :: OpState -> OpState
step opState@(OpState _ _ Nothing _ _) = opState
step opState = do
  let
    (Just framePtr) = vmFramePtr opState
    binOp :: (Int -> Int -> Int)
          -> IntMap.IntMap Int
          -> (AccessMode, AccessMode, AccessMode)
          -> IntMap.IntMap Int
    binOp f v (mode1, mode2, mode3) =
        let in1 = readLoc v mode1 (framePtr + 1)
            in2 = readLoc v mode2 (framePtr + 2)
        in writeLoc v mode3 (framePtr + 3) (f in1 in2)

    handleInstruction :: Instruction -> OpState
    handleInstruction instruction@Instruction{..} =
      case _instrOp of
        Exit  -> opState{vmFramePtr = Nothing}
        Add   ->
          let
            st = binOp (+) (vmState opState) (_instrOperandModes !! 0,_instrOperandModes !! 1,_instrOperandModes !! 2)
          in opState{vmFramePtr = Just (framePtr + 4), vmState = st}
        Mult  ->
          let
            st = binOp (*) (vmState opState) (_instrOperandModes !! 0,_instrOperandModes !! 1,_instrOperandModes !! 2)
          in opState{vmFramePtr = Just (framePtr + 4), vmState = st}
        Input ->
          case vmInput opState of
            Nothing ->
              opState{vmBlocked = True}
            Just input ->
              let st = writeLoc (vmState opState) (_instrOperandModes !! 0) (framePtr + 1) input
              in opState{vmFramePtr = Just (framePtr + 2), vmInput = Nothing, vmBlocked = False, vmState = st}
        Output ->
          let outputVal = readLoc (vmState opState) (_instrOperandModes !! 0) (framePtr + 1)
          in opState{vmFramePtr = Just (framePtr + 2), vmOutputs = Just outputVal}
        JumpTrue ->
          let
            test      = readLoc (vmState opState) (_instrOperandModes !! 0) (framePtr + 1)
            framePtr' = if test == 0
                        then framePtr + 3
                        else readLoc (vmState opState) (_instrOperandModes !! 1) (framePtr + 2)
          in opState{vmFramePtr = Just framePtr'}
        JumpFalse ->
          let
            test      = readLoc (vmState opState) (_instrOperandModes !! 0) (framePtr + 1)
            framePtr' = if test /= 0
                        then framePtr + 3
                        else readLoc (vmState opState) (_instrOperandModes !! 1) (framePtr + 2)
          in opState{vmFramePtr = Just framePtr'}
        StoreLT ->
          let
            a = readLoc (vmState opState) (_instrOperandModes !! 0) (framePtr + 1)
            b = readLoc (vmState opState) (_instrOperandModes !! 1) (framePtr + 2)
            outputVal = if a < b then 1 else 0
            st = writeLoc (vmState opState) (_instrOperandModes !! 2) (framePtr + 3) outputVal
          in opState{vmFramePtr = Just (framePtr + 4), vmState = st}
        StoreEq ->
          let
            a = readLoc (vmState opState) (_instrOperandModes !! 0) (framePtr + 1)
            b = readLoc (vmState opState) (_instrOperandModes !! 1) (framePtr + 2)
            outputVal = if a == b then 1 else 0
            st = writeLoc (vmState opState) (_instrOperandModes !! 2) (framePtr + 3) outputVal
          in opState{vmFramePtr = Just (framePtr + 4), vmState = st}

    opCode = (vmState opState) IntMap.! framePtr
    parsed = parseInstruction opCode
    in handleInstruction parsed

toVec :: [Int] -> IntMap.IntMap Int
toVec prog = IntMap.fromList $ zip [0..] prog

runAmps :: [Int] -> Int -> [Int] -> OpState
runAmps prog initialInput ampVals = do
  let
    newState :: [Int] -> Int -> OpState
    newState program initialInput =
      runUntilBlocked $ OpState { vmState = toVec program
                   , vmOutputs = Nothing
                   , vmFramePtr = Just 0
                   , vmInput = Just initialInput
                   , vmBlocked = False
                   }

    runUntilBlocked :: OpState -> OpState
    runUntilBlocked st =
      let st' = step st
      in if (vmBlocked st' || Nothing == (vmFramePtr st'))
         then st'
         else runUntilBlocked st'

    transferIO :: OpState -> OpState -> OpState
    transferIO oldSt newSt =
      let (Just o) = vmOutputs oldSt
      in newSt { vmInput = Just o }

    ampStep :: [OpState] -> Int -> [OpState]
    ampStep (a:rest) !n =
      let
        a' = runUntilBlocked $ a{vmInput = Just n}
      in reverse $ List.foldl' go [a'] rest
      where
        go :: [OpState] -> OpState -> [OpState]
        go st@(prev:_) !next =
          (runUntilBlocked $ transferIO prev next) : st

    loop amps n =
      let
        stepState = ampStep amps n
        nextInput = (Maybe.fromJust . vmOutputs . last $ stepState)
      in
        case (vmFramePtr . last $ stepState) of
          Nothing -> stepState
          _ -> loop stepState nextInput

    amplifiers = map (newState prog) ampVals
    in last $ loop amplifiers 0

part2' :: [Int] -> [Int] -> Int
part2' prog args =
  let OpState{..} = runAmps prog 0 args
  in (Maybe.fromJust vmOutputs)

part2 :: [Int] -> Int
part2 prog =
  maximum . map (part2' prog) $ List.permutations [5..9]

someFunc :: IO ()
someFunc =
  putStrLn . show $ part2 input

input :: [Int]
input = [3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99]
