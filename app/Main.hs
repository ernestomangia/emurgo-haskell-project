module Main where

import GameLogic

main :: IO ()
main = do
    putStrLn ""
    putStrLn "********** EMURGO Batch 64 | Final project | 2022 **********"
    putStrLn "**********            Minesweeper Game            **********"
    putStrLn ""
    playerName <- putStrGetLine "Please, enter your name: "
    board <- makeBoard 5 5
    runGame $ makeGame board playerName