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
  , Shape(..)
  , moveBy
  , rotateRight
  , rotateLeft
  , randomShape
  , initialPosition
  , tetrominoBlocks
  )

import Text.Printf (printf)

-- Game state
data GameState = GameState
  { board      :: Board
  , current    :: Tetromino
  , nextShape  :: Shape
  , holdPiece  :: Maybe Tetromino
  , holdUsed   :: Bool
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

holdPanelWidth :: Float
holdPanelWidth = 4 * blockSize

holdBoxBlocks :: Int
holdBoxBlocks = 4

holdBoxSize :: Float
holdBoxSize = fromIntegral holdBoxBlocks * blockSize

gapY :: Float
gapY = blockSize

nextBoxSize :: Float
nextBoxSize = holdBoxSize

-- Entry point
gameMain :: IO ()
gameMain = do
  cur <- randomShape
  nxt <- randomShape
  let initTetro = Tetromino cur initialPosition 0
      initState = GameState emptyBoard initTetro nxt Nothing False 0 0 0 0 False
  playIO
    (InWindow "TETRIS" (round windowWidth, round windowHeight) (100, 100))
    black
    60
    initState
    drawGame
    handleEvent
    updateGame

-- Dimensions
windowWidth, windowHeight :: Float
windowWidth  = fromIntegral boardWidth * blockSize + holdPanelWidth
windowHeight = fromIntegral boardHeight * blockSize

-- Drawing
drawGame :: GameState -> IO Picture
drawGame state
  | gameOver state = return $ Pictures
      [ Color red $ Translate (-80) 0 $ Scale 0.2 0.2 $ Text "Game Over"
      , Color white $ Translate (-85) (-30) $ Scale 0.1 0.1 $ Text ("Final Score: " ++ show (score state))
      ]
  | otherwise = return $ Pictures $
      map drawBlock allBlocks ++ holdPicture state ++ nextPicture state ++
      [ Translate (-windowWidth / 2 + holdPanelWidth + 10) (windowHeight / 2 - 30)
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
      Translate (fromIntegral x * blockSize - windowWidth / 2 + holdPanelWidth + blockSize / 2)
                (-(fromIntegral y * blockSize - windowHeight / 2 + blockSize / 2))
      $ Color color
      $ rectangleSolid (blockSize - 2) (blockSize - 2)

-- Holding areas block
holdPicture :: GameState -> [Picture]
holdPicture st =
  let offsetX = -windowWidth / 2 + holdPanelWidth / 2
      offsetY =  windowHeight / 2 - holdBoxSize / 2   -- centre vertically
      factor  = holdBoxSize / (fromIntegral holdBoxBlocks * blockSize)

      mini (x,y,col) =
        Translate (fromIntegral x * blockSize * factor)
                  (-(fromIntegral y * blockSize * factor))
          $ Scale factor factor
          $ Color col
          $ rectangleSolid (blockSize - 2) (blockSize - 2)

      blockPics = case holdPiece st of
                    Nothing -> []
                    Just h  -> map mini $
                               tetrominoBlocks (h { position = (0,0)
                                                  , orientation = 0 })
  in  [ Translate offsetX offsetY
          $ Color white $ rectangleWire holdBoxSize holdBoxSize
      , Translate (offsetX + 4) (offsetY - holdBoxSize/2 - 16)
          $ Scale 0.08 0.08 $ Color white $ Text "HOLD"
      ] ++
      [ Translate offsetX offsetY $ Pictures blockPics ]

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
      EventKey (Char 'c') Down _ _ -> tryHold state
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
  let shape = nextShape state
      spawn = Tetromino shape initialPosition 0
  if isValidPosition spawn clearedBoard
    then do
    nxt <- randomShape
    pure state { board = clearedBoard
               , current = spawn
               , nextShape = nxt
               , linesTotal = totalLines'
               , level = level'
               , score = score'
               , timeAccum = 0
               , holdUsed = False
               }
    else pure state { gameOver = True }

-- Scoring logic
scoreForLines :: Int -> Int -> Int
scoreForLines n level = case n of
  1 -> 100 * (level + 1)
  2 -> 300 * (level + 1)
  3 -> 500 * (level + 1)
  4 -> 800 * (level + 1)
  _ -> 0

-- Hard drop logic (spacebar)
hardDrop :: GameState -> GameState
hardDrop state = state { current = dropDown (current state) }
  where
    dropDown t
      | isValidPosition (moveBy (0, 1) t) (board state) = dropDown (moveBy (0, 1) t)
      | otherwise = t

-- How long (in seconds) between automatic drops at a given level
dropInterval :: Int -> Float
dropInterval lvl =
  max 0.05 (0.4 - fromIntegral lvl * 0.03)

-- If the slot is empty, store the current piece and spawn a fresh one
tryHold :: GameState -> IO GameState
tryHold st
  | holdUsed st = pure st
  | otherwise =
    case holdPiece st of
      Nothing -> do
        shape <- randomShape
        let spawn   = Tetromino shape initialPosition 0
            newCurr = reset (current st)
        pure st { current   = spawn
                , holdPiece = Just newCurr
                , holdUsed  = True }
      Just h  -> do
        let newCurr = reset (current st)
        pure st { current   = reset h
                , holdPiece = Just newCurr
                , holdUsed  = True }
  where
    reset t = t { position = initialPosition, orientation = 0 }   

-- UI for previewing next block
nextPicture :: GameState -> [Picture]
nextPicture st =
  let offsetX = -windowWidth / 2 + holdPanelWidth / 2
      offsetY = windowHeight / 2 - holdBoxSize - gapY - nextBoxSize / 2
      factor = nextBoxSize / (fromIntegral holdBoxBlocks * blockSize)
      mini (x, y, col) =
        Translate (fromIntegral x * blockSize * factor)
                  (-(fromIntegral y * blockSize * factor))
          $ Scale factor factor
          $ Color col
          $ rectangleSolid (blockSize - 2) (blockSize - 2)
      tetro = Tetromino (nextShape st) (0, 0) 0
      blocks = map mini $ tetrominoBlocks tetro
  in [ Translate offsetX offsetY $ Pictures blocks
        , Translate offsetX offsetY
          $ Color white $ rectangleWire nextBoxSize nextBoxSize
        , Translate (offsetX + 4) (offsetY - nextBoxSize/2 - 16)
          $ Scale 0.08 0.08 $ Color white $ Text "NEXT"
      ]