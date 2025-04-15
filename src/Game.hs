module Game (runGame) where

import Control.Concurrent (threadDelay)
import Board (Board, emptyBoard, drawBoard, updateBoardWithTetromino)
import Tetromino (Tetromino(..), Shape(..), initialPosition, tetrominoBlocks)

data GameState = GameState
  { board :: Board
  , tetromino :: Tetromino
  }

initialState :: GameState
initialState = GameState
  { board = emptyBoard
  , tetromino = Tetromino T initialPosition
  }

runGame :: IO ()
runGame = gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state = do
  putStrLn "\ESC[2J" -- clear screen
  let GameState b t = state
      b' = updateBoardWithTetromino b t
  drawBoard b'
  threadDelay 500000  -- half a second
  let t' = moveDown t
  gameLoop state { tetromino = t' }

moveDown :: Tetromino -> Tetromino
moveDown t@(Tetromino s (x, y)) = Tetromino s (x, y + 1)
