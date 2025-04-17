module Tetromino
  ( Tetromino(..)
  , Shape(..)
  , Orientation
  , initialPosition
  , tetrominoBlocks
  , tetrominoPositions
  , randomShape
  , moveBy
  , rotateRight
  , rotateLeft
  ) where

import System.Random (randomRIO)
import Graphics.Gloss (Color, makeColorI)

-- Position on the board: (x, y)
type Position = (Int, Int)

-- 0, 1, 2, 3 (clockwise quarter-turns)
type Orientation = Int

-- Seven classic tetris shapes
data Shape = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum, Bounded)

-- Tetromino has a shape, position, and orientation
data Tetromino = Tetromino
  { shape       :: Shape
  , position    :: Position
  , orientation :: Orientation
  } deriving (Eq, Show)

-- Initial spawn position near center of board
initialPosition :: Position
initialPosition = (5, 0)

-- Relative block layouts for shape in orientation 0
baseCoords :: Shape -> [Position]
baseCoords I = [(-2, 0), (-1, 0), (0, 0), (1, 0)]
baseCoords O = [(0, 0), (1, 0), (0, 1), (1, 1)]
baseCoords T = [(-1, 0), (0, 0), (1, 0), (0, 1)]
baseCoords S = [(-1, 1), (0, 1), (0, 0), (1, 0)]
baseCoords Z = [(-1, 0), (0, 0), (0, 1), (1, 1)]
baseCoords J = [(-1, 0), (-1, 1), (0, 0), (1, 0)]
baseCoords L = [(-1, 0), (0, 0), (1, 0), (1, 1)]

-- Rotate a coordinate 90 degrees clockwise
rotCW :: Position -> Position
rotCW (x, y) = (-y, x)

-- Apply N clockwise rotations
applyRot :: Int -> Position -> Position
applyRot 0 p = p
applyRot n p = applyRot (n - 1) (rotCW p)

-- Convert shape to color
shapeColor :: Shape -> Color
shapeColor s = case s of
  I -> makeColorI 0 255 255 255   -- Cyan
  O -> makeColorI 255 255 0 255   -- Yellow
  T -> makeColorI 160 32 240 255  -- Purple
  S -> makeColorI 0 255 0 255     -- Green
  Z -> makeColorI 255 0 0 255     -- Red
  J -> makeColorI 0 0 255 255     -- Blue
  L -> makeColorI 255 165 0 255   -- Orange

-- Absolute board coordinates + color for rendering
tetrominoBlocks :: Tetromino -> [(Int, Int, Color)]
tetrominoBlocks (Tetromino s (px, py) ori) =
  [ (px + dx, py + dy, shapeColor s)
  | (dx, dy) <- map (applyRot ori) (baseCoords s)
  ]

-- Pure positions for collision / merging
tetrominoPositions :: Tetromino -> [Position]
tetrominoPositions (Tetromino s (px, py) ori) =
  [ (px + dx, py + dy)
  | (dx, dy) <- map (applyRot ori) (baseCoords s)
  ]

-- Movement/Rotation helpers

moveBy :: (Int, Int) -> Tetromino -> Tetromino
moveBy (dx, dy) t = t { position = (x + dx, y + dy) }
  where (x, y) = position t

rotateRight, rotateLeft :: Tetromino -> Tetromino
rotateRight t = t { orientation = (orientation t + 1) `mod` 4 }
rotateLeft  t = t { orientation = (orientation t + 3) `mod` 4 }

-- Random shape generator
randomShape :: IO Shape
randomShape = do
  i <- randomRIO (fromEnum (minBound :: Shape), fromEnum (maxBound :: Shape))
  return (toEnum i)
