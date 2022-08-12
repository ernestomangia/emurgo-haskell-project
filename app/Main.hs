module Main where

import DataTypes
import GameLogic

main :: IO ()
main = do
    putStrLn ""
    putStrLn "********** EMURGO Batch 64 | Final project | 2022 **********"
    putStrLn "**********            Minesweeper Game            **********"
    putStrLn ""
    playerName <- putStrGetLine "Please, enter your name: "
    putStrLn ""
    config <- makeConfig
    board  <- makeBoard $ difficulty config
    runGame $ makeGame board playerName