module Tetromino
  ( Tetromino(..)
  , Shape(..)
<<<<<<< HEAD
  , Rotation(..)
  , initialPosition
  , tetrominoBlocks
  , randomShape
  , rotateTetromino
=======
  , Orientation
  , initialPosition
  , tetrominoBlocks
  , randomShape
  , moveBy
  , rotateRight
  , rotateLeft
>>>>>>> main.josh
  ) where

import System.Random (randomRIO)

-- Position on the board: (x, y)
type Position = (Int, Int)

-- 0, 1, 2, 3 (clockwise quarter-turns)
type Orientation = Int

<<<<<<< HEAD
data Rotation = R0 | R90 | R180 | R270
  deriving (Eq, Show, Enum)

=======
-- Seven classic tetris shapes
data Shape = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum, Bounded)

-- Tetromino has a shpae, position, and orientation
>>>>>>> main.josh
data Tetromino = Tetromino
  { shape    :: Shape
  , position :: Position
<<<<<<< HEAD
  , rotation :: Rotation
=======
  , orientation :: Orientation
>>>>>>> main.josh
  } deriving (Eq, Show)

-- Initital spawn position near center of board
initialPosition :: Position
initialPosition = (5, 0)  -- roughly center top

<<<<<<< HEAD
-- Rotate a point around (0,0)
rotatePoint :: Rotation -> (Int, Int) -> (Int, Int)
rotatePoint rot (x, y) = case rot of
  R0   -> (x, y)
  R90  -> (y, -x)
  R180 -> (-x, -y)
  R270 -> (-y, x)

-- Base shape offsets (unrotated)
shapeOffsets :: Shape -> [Position]
shapeOffsets I = [(-2, 0), (-1, 0), (0, 0), (1, 0)]
shapeOffsets O = [(0, 0), (1, 0), (0, 1), (1, 1)]
shapeOffsets T = [(-1, 0), (0, 0), (1, 0), (0, 1)]
shapeOffsets S = [(0, 0), (1, 0), (-1, 1), (0, 1)]
shapeOffsets Z = [(-1, 0), (0, 0), (0, 1), (1, 1)]
shapeOffsets J = [(-1, 0), (-1, 1), (0, 0), (1, 0)]
shapeOffsets L = [(1, 0), (-1, 0), (0, 0), (1, 1)]

-- Compute actual block positions based on rotation and position
tetrominoBlocks :: Tetromino -> [Position]
tetrominoBlocks (Tetromino s (cx, cy) rot) =
  map (\(x, y) -> let (rx, ry) = rotatePoint rot (x, y)
                  in (cx + rx, cy + ry)) $
    shapeOffsets s
=======
-- Relative block layouts for shape in orientation O
baseCoords :: Shape -> [Position]
baseCoords I = [(-2, 0), (-1, 0), (0, 0), (1, 0)]
baseCoords O = [(0, 0), (1, 0), (0, 1), (1, 1)]
baseCoords T = [(-1, 0), (0, 0), (1, 0), (0,1)]
baseCoords S = [(-1, 1), (0, 1), (0, 0), (1, 0)]
baseCoords Z = [(-1, 0), (0, 0), (0, 1), (1, 1)]
baseCoords J = [(-1, 0), (-1, 1), (0, 0), (1, 0)]
baseCoords L = [(-1, 0), (0, 0), (1, 0), (1, 1)]

-- Rotate a coordinate 90 degrees clockwise around (0, 0)
rotCW :: Position -> Position
rotCW (x, y) = (-y, x)

-- Apply n clockwise rotations
applyRot :: Int -> Position -> Position
applyRot 0 p = p
applyRot n p = applyRot (n - 1) (rotCW p)

-- Absolute board coordinates occupied by a tetromino
tetrominoBlocks :: Tetromino -> [Position]
tetrominoBlocks (Tetromino s (px, py) orig) = map toAbs rels
  where
    rels = map (applyRot orig) (baseCoords s)
    toAbs (dx, dy) = (px + dx, py + dy)
>>>>>>> main.josh

-- Movement/Rotation helpers

moveBy :: (Int, Int) -> Tetromino -> Tetromino
moveBy (dx, dy) tetromino = tetromino { position = (x + dx, y + dy) } where (x, y) = position tetromino

rotateRight :: Tetromino -> Tetromino
rotateRight tetromino = tetromino { orientation = (orientation tetromino + 1) `mod` 4 }

rotateLeft :: Tetromino -> Tetromino
rotateLeft tetromino = tetromino { orientation = (orientation tetromino + 3) `mod` 4 }

-- Random shape
randomShape :: IO Shape
randomShape = do
<<<<<<< HEAD
  idx <- randomRIO (0, length allShapes - 1)
  return (allShapes !! idx)

-- Rotate tetromino clockwise
rotateTetromino :: Tetromino -> Tetromino
rotateTetromino t@(Tetromino s p rot) =
  let nextRot = toEnum ((fromEnum rot + 1) `mod` 4)
  in t { rotation = nextRot }
=======
  i <- randomRIO (fromEnum (minBound::Shape), fromEnum (maxBound::Shape))
  return (toEnum i)
>>>>>>> main.josh
