module Main where

import GameLogic

main :: IO ()
main = do
    putStrLn ""
    putStrLn "********** EMURGO Batch 64 | Final project | 2022 **********"
    putStrLn "**********            Minesweeper Game            **********"
    putStrLn ""
    playerName <- putStrGetLine "Please, enter your name: "
    runGame $ makeGame (makeBoard 9 9) playerName