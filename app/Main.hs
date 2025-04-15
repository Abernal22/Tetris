module Main where

import Game (runGame)

main :: IO ()
main = do
    putStrLn "Welcome to Tetris in Haskell!"
    runGame
