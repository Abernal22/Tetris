module Tetromino
  ( Tetromino(..)
  , Shape(..)
  , initialPosition
  , tetrominoBlocks
  , randomShape
  ) where

import System.Random (randomRIO)

type Position = (Int, Int)

data Shape = I | O | T | S | Z | J | L
  deriving (Eq, Show)

data Tetromino = Tetromino
  { shape :: Shape
  , position :: Position
  } deriving (Eq, Show)

initialPosition :: Position
initialPosition = (4, 0)  -- roughly center top

tetrominoBlocks :: Tetromino -> [Position]
tetrominoBlocks (Tetromino shape (x, y)) =
  case shape of
    I -> [(x, y), (x, y+1), (x, y+2), (x, y+3)]
    O -> [(x, y), (x+1, y), (x, y+1), (x+1, y+1)]
    T -> [(x, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    S -> [(x, y+1), (x+1, y+1), (x, y), (x-1, y)]
    Z -> [(x-1, y+1), (x, y+1), (x, y), (x+1, y)]
    J -> [(x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    L -> [(x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

-- List of all possible shapes
allShapes :: [Shape]
allShapes = [I, O, T, S, Z, J, L]

-- Generates a random Shape
randomShape :: IO Shape
randomShape = do
  idx <- randomRIO (0, length allShapes - 1)
  return (allShapes !! idx)
