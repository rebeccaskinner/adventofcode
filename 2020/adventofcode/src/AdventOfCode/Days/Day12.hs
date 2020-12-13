{-# LANGUAGE RecordWildCards #-}
module AdventOfCode.Days.Day12 (part1,part2,day12) where
import           AdventOfCode.Types
import Data.List
import GHC.Arr

data Heading
  = North
  | East
  | South
  | West deriving (Eq, Ord, Enum, Show)

rotateHeading :: Int -> Heading -> Heading
rotateHeading deg heading =
  let
    currentRotation = 90 * fromEnum heading
    deg' = deg `mod` 360
    rotation = ((currentRotation + deg') `mod` 360) `div` 90
  in toEnum rotation

data Movement
  = Forward Int
  | RotateRight Int
  | RotateLeft Int
  | MoveNorth Int
  | MoveSouth Int
  | MoveEast  Int
  | MoveWest  Int
  deriving (Eq, Show)

parseMovement :: String -> Movement
parseMovement movement =
  let (instr, amnt) = splitAt 1 movement
      amnt' = read amnt
  in case instr of
       "F" -> Forward amnt'
       "R" -> RotateRight amnt'
       "L" -> RotateLeft amnt'
       "N" -> MoveNorth amnt'
       "S" -> MoveSouth amnt'
       "E" -> MoveEast amnt'
       "W" -> MoveWest amnt'

data Ship = Ship { shipX        :: !Int
                 , shipY        :: !Int
                 , shipHeading  :: !Heading
                 , waypointX    :: !Int
                 , waypointY    :: !Int
                 } deriving (Eq, Show)

taxicabShip :: Ship -> Int
taxicabShip Ship{..} = (abs shipX) + (abs shipY)

applyMovement :: Ship -> Movement -> Ship
applyMovement s m =
  case m of
    Forward n ->
      case shipHeading s of
        North -> applyMovement s (MoveNorth n)
        South -> applyMovement s (MoveSouth n)
        East  -> applyMovement s (MoveEast n)
        West  -> applyMovement s (MoveWest n)
    RotateRight n ->
      let heading = rotateHeading n (shipHeading s)
      in s { shipHeading = heading }
    RotateLeft n  ->
      let heading = rotateHeading (-n) (shipHeading s)
      in s { shipHeading = heading }
    MoveNorth n ->
      let shipY' = (shipY s) - n
      in s{ shipY = shipY' }
    MoveSouth n   ->
      let shipY' = (shipY s) + n
      in s { shipY = shipY' }
    MoveEast  n   ->
      let shipX' = (shipX s) + n
      in s { shipX = shipX' }
    MoveWest  n   ->
      let shipX' = (shipX s) - n
      in s { shipX = shipX' }

-- | We're only concerned with rotations in multiples of 90 degrees,
-- and we're working with integers, so use a lookup table rather than
-- doing floating point calculations
intSin, intCos :: Int -> Int
intSin n =
  let idx = (n `mod` 360) `div` 90
  in sinTable ! idx
  where
    sinTable = listArray (0,3) [0,1,0,-1]
{-# INLINE intSin #-}

intCos n =
  let idx = (n `mod` 360) `div` 90
  in cosTable ! idx
  where
    cosTable = listArray (0,3) [1,0,-1,0]
{-# INLINE intCos #-}

intRotate :: (Int,Int) -> Int -> (Int,Int)
intRotate (x,y) deg =
  let
    x' = (x * intCos deg) - (y * intSin deg)
    y' = (x * intSin deg) + (y * intCos deg)
  in (x',y')
{-# INLINE intRotate #-}

rotateWaypoint :: Ship -> Int -> Ship
rotateWaypoint ship deg =
  let
    wx = waypointX ship
    wy = waypointY ship
    (wx',wy') = intRotate (wx,wy) deg
  in ship { waypointX = wx'
          , waypointY = wy'}

applyWaypointMovement :: Ship -> Movement -> Ship
applyWaypointMovement s m =
  case m of
    Forward n ->
      let
        sx = shipX s
        sy = shipY s
        wx = waypointX s
        wy = waypointY s
        sx' = sx + (n * wx)
        sy' = sy + (n * wy)
      in s { shipX = sx'
           , shipY = sy'}
    RotateRight n ->
      rotateWaypoint s n
    RotateLeft n  ->
      rotateWaypoint s (-n)
    MoveNorth n ->
      let waypointY' = (waypointY s) - n
      in s{ waypointY = waypointY' }
    MoveSouth n   ->
      let waypointY' = (waypointY s) + n
      in s { waypointY = waypointY' }
    MoveEast  n   ->
      let waypointX' = (waypointX s) + n
      in s { waypointX = waypointX' }
    MoveWest  n   ->
      let waypointX' = (waypointX s) - n
      in s { waypointX = waypointX' }

parseMovements :: String -> [Movement]
parseMovements = map parseMovement . lines

part1 :: Puzzle IO ()
part1 = withStringInput $ \i -> do
  let s = Ship 0 0 East 0 0
      m = parseMovements i
      s' = foldl' applyMovement s m
      d = taxicabShip s'
  print d

part2 :: Puzzle IO ()
part2 = withStringInput $ \i -> do
  let s = Ship
        { shipX = 0
        , shipY = 0
        , shipHeading = East
        , waypointX = 10
        , waypointY = -1
        }
      m = parseMovements i
      s' = foldl' applyWaypointMovement s m
      d = taxicabShip s'
  print d

day12 :: PuzzleDay IO
day12 = PuzzleDay part1 part2
