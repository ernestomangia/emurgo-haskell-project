module Main where

import DataTypes
import GameLogic

import Control.Monad.Reader
import Control.Monad.State

main :: IO ()
main = do
    putStrLn ""
    putStrLn "********** EMURGO Batch 64 | Final project | 2022 **********"
    putStrLn "**********            Minesweeper Game            **********"
    putStrLn ""
    config <- makeConfig
    game   <- makeGame config
    runStateT (runReaderT runGame config) game
    return ()