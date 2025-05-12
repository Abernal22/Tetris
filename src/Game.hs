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
  , randomShape
  , initialPosition
  , tetrominoBlocks
  , moveBy
  , rotateRight
  , rotateLeft
  )

import Text.Printf (printf)

-- One block is 20×20 pixels
blockSize :: Float
blockSize = 20

-- Left panel width in blocks (for Hold/Next & status)
panelWidthBlocks :: Int
panelWidthBlocks = 6

-- Left panel offsets
panelOffset :: Float
panelOffset = fromIntegral panelWidthBlocks * blockSize

-- Board dimensions (in blocks)
boardWidth, boardHeight :: Int
boardWidth  = 10
boardHeight = 20

-- Window dimensions (board + left panel)
windowWidth, windowHeight :: Float
windowWidth  = fromIntegral boardWidth  * blockSize + panelOffset
windowHeight = fromIntegral boardHeight * blockSize

-- Full game state with hold/next
data GameState = GameState
  { board      :: Board
  , current    :: Tetromino
  , nextT      :: Tetromino
  , holdPiece  :: Maybe Tetromino
  , usedHold   :: Bool       -- one hold per drop
  , score      :: Int
  , level      :: Int
  , linesTotal :: Int
  , timeAccum  :: Float
  , gameOver   :: Bool
  , paused     :: Bool
  }

-- Start with two random pieces
gameMain :: IO ()
gameMain = do
  s1 <- randomShape
  s2 <- randomShape
  let t1 = Tetromino s1 initialPosition 0
      t2 = Tetromino s2 initialPosition 0
      initSt = GameState
        { board      = emptyBoard
        , current    = t1
        , nextT      = t2
        , holdPiece  = Nothing
        , usedHold   = False
        , score      = 0
        , level      = 0
        , linesTotal = 0
        , timeAccum  = 0
        , gameOver   = False
        , paused     = False
        }
  playIO
    (InWindow "TETRIS" (round windowWidth, round windowHeight) (100,100))
    black
    60
    initSt
    drawGame
    handleEvent
    updateGame

-- Draw one frame
drawGame :: GameState -> IO Picture
drawGame st
  | gameOver st = return $ Pictures
      [ Color red
          $ Translate (-80) 0
          $ Scale 0.3 0.3
          $ Text "Game Over"
      , Color white
          $ Translate (-85) (-30)
          $ Scale 0.2 0.2
          $ Text ("Final Score: " ++ show (score st))
      ]
  | paused st   = return $ Pictures (basePics ++ [pauseOverlay])
  | otherwise   = return $ Pictures basePics
  where
    -- Common pictures (board, pieces, boxes, labels, status)
    basePics :: [Picture]
    basePics =
      --  Main board border
      [ Color white
          $ Translate boardCenterX 0
          $ rectangleWire
              (fromIntegral boardWidth  * blockSize)
              (fromIntegral boardHeight * blockSize)
      ]
      -- Locked cells and falling tetromino
      ++ map drawBlock (boardToBlocks (board st))
      ++ map drawBlock (tetrominoBlocks (current st))
      -- Hold box border, held piece, label
      ++ [ Color white
             $ Translate panelCenterX holdCenterY
             $ rectangleWire
                 (fromIntegral holdBoxWidth * blockSize)
                 (fromIntegral holdBoxWidth * blockSize)
         , drawBoxedTetromino (holdPiece st) panelCenterX holdCenterY
         , Translate boxTextX holdLabelY
             $ Scale 0.08 0.08
             $ Color white
             $ Text "Hold"
         ]
      -- Next box border, preview piece, label
      ++ [ Color white
             $ Translate panelCenterX previewCenterY
             $ rectangleWire
                 (fromIntegral previewBoxWidth * blockSize)
                 (fromIntegral previewBoxWidth * blockSize)
         , drawBoxedTetromino (Just (nextT st)) panelCenterX previewCenterY
         , Translate boxTextX previewLabelY
             $ Scale 0.08 0.08
             $ Color white
             $ Text "Next"
         ]
      -- Status text stacked under Next box
      ++ [ Translate statusStartX statusLine1Y
             $ Scale 0.08 0.08
             $ Color white
             $ Text (printf "Score: %d" (score st))
         , Translate statusStartX statusLine2Y
             $ Scale 0.08 0.08
             $ Color white
             $ Text (printf "Level: %d" (level st))
         , Translate statusStartX statusLine3Y
             $ Scale 0.08 0.08
             $ Color white
             $ Text (printf "Speed: %.2fs" (dropInterval (level st)))
         ]

    -- overlay for paused state
    pauseOverlay :: Picture
    pauseOverlay =
      Color white
        $ Translate (-60) 0
        $ Scale 0.2 0.2
        $ Text "Paused"

    -- box dimensions in blocks
    holdBoxWidth, previewBoxWidth :: Int
    holdBoxWidth    = 5
    previewBoxWidth = 5

    -- spacing parameters
    boxSpacing        = 20    -- px between Hold and Next boxes
    statusPadding     = 20    -- px below Next box before status
    statusLineSpacing = 30    -- px between status lines

    -- panel and board centers
    panelCenterX = -windowWidth/2 + panelOffset/2
    boardCenterX = panelOffset
                   - windowWidth/2
                   + (fromIntegral boardWidth * blockSize)/2

    -- Y centers of Hold and Next boxes
    holdCenterY    = windowHeight/2
                     - (fromIntegral holdBoxWidth * blockSize)/2
                     - 10
    previewCenterY = holdCenterY
                     - fromIntegral holdBoxWidth * blockSize
                     - boxSpacing

    -- Y positions for the “Hold” and “Next” labels
    holdLabelY    = holdCenterY
                    - (fromIntegral holdBoxWidth * blockSize)/2
                    - 15
    previewLabelY = previewCenterY
                    - (fromIntegral previewBoxWidth * blockSize)/2
                    - 15

    statusStartX :: Float
    statusStartX = (-windowWidth) / 2 + 26

    boxTextX :: Float
    boxTextX = (-windowWidth) / 2 + 50

    -- starting Y and offsets for status text
    statusStartY  = previewCenterY
                    - (fromIntegral previewBoxWidth * blockSize)/2
                    - statusPadding - 40
    statusLine1Y  = statusStartY
    statusLine2Y  = statusStartY - statusLineSpacing
    statusLine3Y  = statusStartY - 2 * statusLineSpacing

    -- helper to draw a single board block
    drawBlock (x,y,col) =
      Translate
        ( fromIntegral x * blockSize
          - windowWidth/2
          + blockSize/2
          + panelOffset
        )
        (-( fromIntegral y * blockSize
            - windowHeight/2
            + blockSize/2
          ))
      $ Color col
      $ rectangleSolid (blockSize - 2) (blockSize - 2)

    -- helper to draw a tetromino inside a box
    drawBoxedTetromino :: Maybe Tetromino -> Float -> Float -> Picture
    drawBoxedTetromino Nothing _ _ = Blank
    drawBoxedTetromino (Just t) cx cy =
      let t0 = t { position = (0,0) }
      in Pictures
         [ Translate (cx + fromIntegral x*blockSize)
                     (cy + fromIntegral y*blockSize)
             $ Color col
             $ rectangleSolid (blockSize - 2) (blockSize - 2)
         | (x,y,col) <- tetrominoBlocks t0
         ]


-- | Handle key events
handleEvent :: Event -> GameState -> IO GameState
handleEvent ev st
  | gameOver st = return st
  | otherwise   = case ev of
      EventKey (SpecialKey KeyEsc) Down _ _ -> 
        return st { paused = not (paused st)}
      EventKey (Char 'a') Down _ _ -> return $ tryMove (-1,0) st
      EventKey (Char 'd') Down _ _ -> return $ tryMove (1,0) st
      EventKey (Char 's') Down _ _ -> return $ tryMove (0,1) st
      EventKey (Char 'w') Down _ _ -> return $ tryRotate rotateRight st
      EventKey (Char 'q') Down _ _ -> return $ tryRotate rotateLeft  st
      EventKey (Char 'c') Down _ _ -> holdCurrent st
      EventKey (SpecialKey KeySpace) Down _ _ -> return $ hardDrop st
      _ -> return st

-- | Move the current piece if valid
tryMove :: (Int,Int) -> GameState -> GameState
tryMove d st =
  let m = moveBy d (current st)
  in if isValidPosition m (board st) then st { current = m } else st

-- | Rotate with simple wall-kicks
tryRotate :: (Tetromino -> Tetromino) -> GameState -> GameState
tryRotate f st =
  let raw = f (current st)
      kicks = [ moveBy off raw | off <- [(0,0),(1,0),(-1,0),(0,-1)] ]
      vs = filter (`isValidPosition` board st) kicks
  in case vs of (t':_) -> st { current = t' }
                []     -> st

-- | Hold logic on 'c'
holdCurrent :: GameState -> IO GameState
holdCurrent st
  | usedHold st = return st
  | otherwise   = case holdPiece st of
      Nothing -> do
        s <- randomShape
        let newHold = current st
            newCurr = (nextT st){ position = initialPosition, orientation = 0 }
            newNext = Tetromino s initialPosition 0
        return st
          { holdPiece = Just newHold
          , current   = newCurr
          , nextT     = newNext
          , usedHold  = True
          }
      Just h -> do
        let newCurr = h { position = initialPosition, orientation = 0 }
        return st
          { holdPiece = Just (current st)
          , current   = newCurr
          , usedHold  = True
          }

-- | Gravity tick; lock & spawn when needed
updateGame :: Float -> GameState -> IO GameState
updateGame _ st | gameOver st || paused st = return st
updateGame dt st
  | gameOver st = return st
  | timeAccum st + dt >= dropInterval (level st) = do
      let st'   = st { timeAccum = 0 }
          moved = tryMove (0,1) st'
      if position (current moved) == position (current st)
        then lockAndNext st'
        else return moved
  | otherwise = return st { timeAccum = timeAccum st + dt }

-- | Lock piece, clear lines, update score/level, spawn next piece
lockAndNext :: GameState -> IO GameState
lockAndNext st = do
  let merged   = mergeTetromino (current st) (board st)
      (bd',n)  = clearFullLines merged
      tlines   = linesTotal st + n
      lvl      = tlines `div` 10
      sc'      = score st + scoreForLines n (level st)
      nextCur  = (nextT st){ position = initialPosition, orientation = 0 }
  s <- randomShape
  let newNext = Tetromino s initialPosition 0
  if isValidPosition nextCur bd'
    then return st
      { board      = bd'
      , current    = nextCur
      , nextT      = newNext
      , usedHold   = False
      , score      = sc'
      , linesTotal = tlines
      , level      = lvl
      , timeAccum  = 0
      }
    else return st { gameOver = True }

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

-- | Seconds between automatic drops (decrements with level)
dropInterval :: Int -> Float
dropInterval lvl = max 0.05 (0.4 - fromIntegral lvl * 0.03)
