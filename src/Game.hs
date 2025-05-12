module Game (gameMain) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (randomRIO)

import Board
  ( Board
  , emptyBoard
  , isValidPosition
  , mergeTetromino
  , clearFullLines
  , boardToBlocks
  )

import Tetromino
  ( Tetromino(..)
  , moveBy
  , rotateRight
  , rotateLeft
  , randomShape
  , initialPosition
  , tetrominoBlocks
  , moveBy
  , rotateRight
  , rotateLeft
  )

import Text.Printf (printf)

-- | One block is 20×20 pixels
blockSize :: Float
blockSize = 20

-- | Left panel width in blocks (for Hold/Next & status)
panelWidthBlocks :: Int
panelWidthBlocks = 6

panelOffset :: Float
panelOffset = fromIntegral panelWidthBlocks * blockSize

-- | Board dimensions (in blocks)
boardWidth, boardHeight :: Int
boardWidth  = 10
boardHeight = 20

-- | Window size in pixels (board + left panel)
windowWidth, windowHeight :: Float
windowWidth  = fromIntegral boardWidth  * blockSize + panelOffset
windowHeight = fromIntegral boardHeight * blockSize

-- | Game state, including hold/next logic
data GameState = GameState
  { board      :: Board
  , current    :: Tetromino
  , score      :: Int
  , level      :: Int
  , linesTotal :: Int
  , timeAccum  :: Float
  , gameOver   :: Bool
  }

-- Constants
blockSize :: Float
blockSize = 20

boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- Entry point
gameMain :: IO ()
gameMain = do
  shape <- randomShape
  let initTetro = Tetromino shape initialPosition 0
      initState = GameState emptyBoard initTetro 0 0 0 0 False
  playIO
    (InWindow "TETRIS" (round windowWidth, round windowHeight) (100,100))
    black
    60
    initSt
    drawGame
    handleEvent
    updateGame

-- Dimensions
windowWidth, windowHeight :: Float
windowWidth  = fromIntegral boardWidth * blockSize
windowHeight = fromIntegral boardHeight * blockSize

-- Drawing
drawGame :: GameState -> IO Picture
drawGame st =
  if gameOver st
    then return $ Pictures
      [ Color red
          $ Translate (-80) 0
          $ Scale 0.3 0.3
          $ Text "Game Over"
      , Color white
          $ Translate (-85) (-30)
          $ Scale 0.2 0.2
          $ Text ("Final Score: " ++ show (score st))
      ]
  | otherwise = return $ Pictures $
      map drawBlock allBlocks ++
      [ Translate (-windowWidth / 2 + 10) (windowHeight / 2 - 30)
          $ Scale 0.08 0.08
          $ Color white
          $ Text
            $ printf
                "Score: %d  Level: %d  Speed: %.2fs"
                (score state)
                (level state)
                (dropInterval (level state))

      ]
  where
    tetroBlocks = tetrominoBlocks (current state)
    boardBlocks = boardToBlocks (board state)
    allBlocks   = tetroBlocks ++ boardBlocks

    drawBlock (x, y, color) =
      Translate (fromIntegral x * blockSize - windowWidth / 2 + blockSize / 2)
                (-(fromIntegral y * blockSize - windowHeight / 2 + blockSize / 2))
      $ Color color
      $ rectangleSolid (blockSize - 2) (blockSize - 2)


-- Handle keyboard input
handleEvent :: Event -> GameState -> IO GameState
handleEvent event state
  | gameOver state = return state
  | otherwise = case event of
      EventKey (Char 'a') Down _ _ -> return $ tryMoveCurrent (-1, 0) state
      EventKey (Char 'd') Down _ _ -> return $ tryMoveCurrent (1, 0) state
      EventKey (Char 's') Down _ _ -> return $ tryMoveCurrent (0, 1) state
      EventKey (Char 'w') Down _ _ -> return $ tryRotateCurrent rotateRight state
      EventKey (Char 'q') Down _ _ -> return $ tryRotateCurrent rotateLeft state
      EventKey (SpecialKey KeySpace) Down _ _ -> return $ hardDrop state
      _ -> return state


-- Time-based game updates (gravity)
updateGame :: Float -> GameState -> IO GameState
updateGame _ state | gameOver state = return state
updateGame dt state
  | timeAccum state + dt >= interval = do
      let afterMove = tryMoveCurrent (0, 1) state
      if position (current afterMove) == position (current state)
        then lockAndSpawn state { timeAccum = 0 }
        else return afterMove { timeAccum = 0 }
  | otherwise = return state { timeAccum = timeAccum state + dt }
 where
  interval = max 0.05 (0.4 - fromIntegral (level state) * 0.03)

-- Movement
tryMoveCurrent :: (Int, Int) -> GameState -> GameState
tryMoveCurrent delta state
  | isValidPosition moved (board state) = state { current = moved }
  | otherwise                           = state
 where
  moved = moveBy delta (current state)

-- Rotation with kicks
tryRotateCurrent :: (Tetromino -> Tetromino) -> GameState -> GameState
tryRotateCurrent rotate state = case firstValid rotated of
    Just t  -> state { current = t }
    Nothing -> state
 where
  raw     = rotate (current state)
  kicks   = [(0, 0), (1, 0), (-1, 0), (0, -1)]
  rotated = [ moveBy off raw | off <- kicks ]
  firstValid = foldr (\t acc -> if isValidPosition t (board state) then Just t else acc) Nothing

-- Lock tetromino and spawn new one
lockAndSpawn :: GameState -> IO GameState
lockAndSpawn state = do
  let lockedBoard                  = mergeTetromino (current state) (board state)
      (clearedBoard, linesCleared) = clearFullLines lockedBoard
      totalLines'                  = linesTotal state + linesCleared
      level'                       = totalLines' `div` 10
      score'                       = score state + scoreForLines linesCleared (level state)
  shape <- randomShape
  let spawn = Tetromino shape initialPosition 0
  if isValidPosition spawn clearedBoard
    then return state { board = clearedBoard
                      , current = spawn
                      , linesTotal = totalLines'
                      , level = level'
                      , score = score'
                      , timeAccum = 0
                      }
    else return state { gameOver = True }

-- | Scoring per lines cleared
scoreForLines :: Int -> Int -> Int
scoreForLines n lvl = case n of
  1 -> 100 * (lvl + 1)
  2 -> 300 * (lvl + 1)
  3 -> 500 * (lvl + 1)
  4 -> 800 * (lvl + 1)
  _ -> 0

-- | Hard drop until collision
hardDrop :: GameState -> GameState
hardDrop st =
  let go t
        | isValidPosition (moveBy (0,1) t) (board st) = go (moveBy (0,1) t)
        | otherwise                                   = t
  in st { current = go (current st) }

-- | Seconds between automatic drops (decreases with level)
dropInterval :: Int -> Float
dropInterval lvl =
  max 0.05 (0.4 - fromIntegral lvl * 0.03)