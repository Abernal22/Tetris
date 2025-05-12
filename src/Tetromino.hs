module Tetromino
  ( Tetromino(..)
  , Shape(..)
  , Orientation
  , initialPosition
  , tetrominoBlocks
  , randomShape
  , moveBy
  , rotateRight
  , rotateLeft
  ) where

import System.Random     (randomRIO)
import Brillo.Data.Color (Color, cyan, yellow, magenta, green, red, blue, orange)

-- | Seven standard Tetris shapes
data Shape = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum, Bounded)

-- | 0–3 possible orientations
type Orientation = Int

-- | A Tetromino has a shape, a grid position (x,y), and an orientation
data Tetromino = Tetromino
  { shape       :: Shape
  , position    :: (Int, Int)
  , orientation :: Orientation
  }
  deriving (Eq, Show)

-- | Starting spawn position centered at top
type Point = (Int,Int)
initialPosition :: Point
initialPosition = (4, 0)

-- | Compute the colored block cells for rendering
tetrominoBlocks :: Tetromino -> [(Int, Int, Color)]
tetrominoBlocks (Tetromino s (px,py) ori) =
  [ (px + dx, py + dy, shapeColor s)
  | (dx, dy) <- map (rotateOffset ori) (baseCoords s)
  ]

-- | Translate by (dx, dy)
moveBy :: (Int, Int) -> Tetromino -> Tetromino
moveBy (dx, dy) tet = tet { position = (x + dx, y + dy) }
  where (x, y) = position tet

-- | Rotate clockwise
rotateRight :: Tetromino -> Tetromino
rotateRight tet = tet { orientation = (orientation tet + 1) `mod` 4 }

-- | Rotate counterclockwise
rotateLeft :: Tetromino -> Tetromino
rotateLeft tet = tet { orientation = (orientation tet + 3) `mod` 4 }

-- | Pick a random shape
randomShape :: IO Shape
randomShape = toEnum <$> randomRIO (fromEnum (minBound :: Shape), fromEnum (maxBound :: Shape))

-- Helpers -----------------------------------------------------

type Offset = (Int, Int)

-- | Rotate a coordinate offset by orientation
rotateOffset :: Orientation -> Offset -> Offset
rotateOffset 0 p         = p
rotateOffset 1 (x,y)     = (-y, x)
rotateOffset 2 (x,y)     = (-x, -y)
rotateOffset 3 (x,y)     = (y, -x)
rotateOffset _ p         = p

-- | Base (unrotated) block positions for each Shape
baseCoords :: Shape -> [Offset]
baseCoords I = [(-2,0),(-1,0),(0,0),(1,0)]
baseCoords O = [(0,0),(1,0),(0,1),(1,1)]
baseCoords T = [(-1,0),(0,0),(1,0),(0,1)]
baseCoords S = [(-1,1),(0,1),(0,0),(1,0)]
baseCoords Z = [(-1,0),(0,0),(0,1),(1,1)]
baseCoords J = [(-1,0),(-1,1),(0,0),(1,0)]
baseCoords L = [(-1,0),(0,0),(1,0),(1,1)]

-- | Map each shape to its fill Color
shapeColor :: Shape -> Color
shapeColor I = cyan
shapeColor O = yellow
shapeColor T = magenta
shapeColor S = green
shapeColor Z = red
shapeColor J = blue
shapeColor L = orange
