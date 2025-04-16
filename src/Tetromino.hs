module Tetromino
  ( Tetromino(..)
  , Shape(..)
  , Rotation(..)
  , initialPosition
  , tetrominoBlocks
  , randomShape
  , rotateTetromino
  ) where

import System.Random (randomRIO)

type Position = (Int, Int)

data Shape = I | O | T | S | Z | J | L
  deriving (Eq, Show)

data Rotation = R0 | R90 | R180 | R270
  deriving (Eq, Show, Enum)

data Tetromino = Tetromino
  { shape    :: Shape
  , position :: Position
  , rotation :: Rotation
  } deriving (Eq, Show)

initialPosition :: Position
initialPosition = (4, 0)  -- roughly center top

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

-- List of all possible shapes
allShapes :: [Shape]
allShapes = [I, O, T, S, Z, J, L]

-- Generates a random Shape
randomShape :: IO Shape
randomShape = do
  idx <- randomRIO (0, length allShapes - 1)
  return (allShapes !! idx)

-- Rotate tetromino clockwise
rotateTetromino :: Tetromino -> Tetromino
rotateTetromino t@(Tetromino s p rot) =
  let nextRot = toEnum ((fromEnum rot + 1) `mod` 4)
  in t { rotation = nextRot }
