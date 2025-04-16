module Game (runGame) where

import Control.Concurrent (threadDelay)
import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(NoBuffering), hReady)
import Board (Board, emptyBoard, drawBoard, updateBoardWithTetromino)
import Tetromino (Tetromino(..), initialPosition, tetrominoBlocks, randomShape, Rotation(..), rotateTetromino)

data GameState = GameState
  { board :: Board
  , tetromino :: Tetromino
  }

runGame :: IO ()
runGame = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  shape <- randomShape
  let initialState = GameState
        { board = emptyBoard
        , tetromino = Tetromino shape initialPosition R0
        }
  gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
  putStrLn "\ESC[2J" -- clear screen
  let GameState b t = state
      b' = updateBoardWithTetromino b t
  drawBoard b'

  hasInput <- hReady stdin
  dir <- if hasInput then getChar else return ' '

  let tMoved = case dir of
                 'a' -> moveLeft t
                 'd' -> moveRight t
                 'w' -> rotateTetromino t
                 _   -> t

  let t' = moveDown (if isValidPosition tMoved then tMoved else t)

  threadDelay 500000  -- half a second

  if isValidPosition t'
    then gameLoop state { tetromino = t' }
    else do
      let lockedBoard = updateBoardWithTetromino b t
      newShape <- randomShape
      let newTetromino = Tetromino newShape initialPosition R0
      if isValidPosition newTetromino
        then gameLoop GameState { board = lockedBoard, tetromino = newTetromino }
        else putStrLn "Game Over!"

moveDown :: Tetromino -> Tetromino
moveDown (Tetromino s (x, y) r) = Tetromino s (x, y + 1) r

moveLeft :: Tetromino -> Tetromino
moveLeft (Tetromino s (x, y) r) = Tetromino s (x - 1, y) r

moveRight :: Tetromino -> Tetromino
moveRight (Tetromino s (x, y) r) = Tetromino s (x + 1, y) r

isValidPosition :: Tetromino -> Bool
isValidPosition tetromino = all inBounds (tetrominoBlocks tetromino)
  where
    inBounds (x, y) = x >= 0 && x < 10 && y >= 0 && y < 20
