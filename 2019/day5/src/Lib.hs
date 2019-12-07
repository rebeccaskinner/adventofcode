{-# LANGUAGE GADTs #-}
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

data OpState where
  Cont :: Int -> MVec.IOVector Int -> OpState
  Term :: Int -> OpState

instance Show OpState where
  show (Cont _ _) = "Cont"
  show (Term _)   = "Term"


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
        | Output deriving (Eq, Show, Enum)

data Instruction = Instruction
  { _instrOp           :: Op
  , _instrArity        :: Int
  , _instrOperandModes :: [AccessMode]
  } deriving (Eq, Show)

parseInstruction :: Int -> Instruction
parseInstruction instr =
  let [a,b,c,d,e] = take 5 $ (reverse $ show instr) <> repeat '0'
      operModes = map parseAccessMode [c,d,e]
  in case [b,a] of
       ['9','9'] -> Instruction { _instrOp = Exit,   _instrArity = 0, _instrOperandModes = operModes }
       ['0','1'] -> Instruction { _instrOp = Add,    _instrArity = 3, _instrOperandModes = operModes }
       ['0','2'] -> Instruction { _instrOp = Mult,   _instrArity = 3, _instrOperandModes = operModes }
       ['0','3'] -> Instruction { _instrOp = Input,  _instrArity = 1, _instrOperandModes = operModes }
       ['0','4'] -> Instruction { _instrOp = Output, _instrArity = 1, _instrOperandModes = operModes }
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

step :: MVec.IOVector Int -> Int -> IO.IORef [OutputData] -> IO OpState
step oldstate at outputs = do
  let
    binOp :: (Int -> Int -> Int)
          -> (AccessMode, AccessMode, AccessMode)
          -> IO (MVec.IOVector Int)

    binOp f (mode1, mode2, mode3) = do
        in1 <- readLoc oldstate mode1 (at + 1)
        in2 <- readLoc oldstate mode2 (at + 2)
        writeLoc oldstate mode3 (at + 3) (f in1 in2)

    getInput :: IO Int
    getInput = pure 1

    handleInstruction :: Instruction -> IO OpState
    handleInstruction instruction@Instruction{..} = do
      case _instrOp of
        Exit  -> Term <$> MVec.read oldstate 0
        Add   -> Cont 3 <$> binOp (+) (_instrOperandModes !! 0, _instrOperandModes !! 1, _instrOperandModes !! 2)
        Mult  -> Cont 3 <$> binOp (*) (_instrOperandModes !! 0, _instrOperandModes !! 1, _instrOperandModes !! 2)
        Input -> Cont 1 <$> do
          input <- getInput
          v <- writeLoc oldstate (_instrOperandModes !! 0) (at + 1) input
          pure v
        Output -> Cont 1 <$> do
          outVal <- readLoc oldstate (_instrOperandModes !! 0) (at + 1)
          st <- Vec.freeze oldstate
          let outputData = OutputData { framePointer = at
                                      , outputValue  = outVal
                                      , programState =  st}
          IO.modifyIORef outputs (outputData :)
          pure oldstate

  opCode <- MVec.read oldstate at
  let parsed = parseInstruction opCode
  handleInstruction parsed

toVec :: [Int] -> IO (MVec.IOVector Int)
toVec = Vec.thaw . Vec.fromList

testProg :: [Int] -> IO [OutputData]
testProg l = do
  v <- toVec l
  o <- outputs
  eval 0 v o
  l' <- Vec.freeze v
  putStrLn . show $ l'
  IO.readIORef o >>= putStrLn . show . reverse
  IO.readIORef o

eval :: Int -> MVec.IOVector Int -> IO.IORef [OutputData] -> IO (Maybe Int)
eval n state o = do
  opSt <- step state n o
  case opSt of
    Term n -> pure $ Just n
    Cont arity state' -> do
      let n' = n + succ arity
      Monad.guard (n <= MVec.length state)
      eval (n + succ arity) state' o

run' l = run l 12 2

run :: [Int] -> Int -> Int -> IO (Maybe Int)
run l noun verb = do
  o <- outputs
  v <- prep noun verb =<< toVec l
  eval 0 v o

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
      case actual of
        Just expected -> pure $ Just (a,b)
        otherwise -> findInputs (succ n) expected

findSolution =
  ((\(a,b) -> 100 * a + b) <$>) <$> findInputs 0 19690720

input :: [Int]
input = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,71,224,1001,224,-111,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1102,66,6,225,1102,22,54,225,1,65,35,224,1001,224,-86,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,20,80,225,101,92,148,224,101,-162,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1102,63,60,225,1101,32,48,225,2,173,95,224,1001,224,-448,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,1001,91,16,224,101,-79,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1101,13,29,225,1101,71,70,225,1002,39,56,224,1001,224,-1232,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,14,59,225,102,38,143,224,1001,224,-494,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,30,28,224,1001,224,-840,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,359,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1008,226,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,419,1001,223,1,223,1108,677,226,224,102,2,223,223,1006,224,434,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,449,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,8,226,677,224,1002,223,2,223,1005,224,479,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,569,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,599,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,629,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,644,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1107,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]


someFunc :: IO ()
someFunc = findSolution >>= putStrLn . show
