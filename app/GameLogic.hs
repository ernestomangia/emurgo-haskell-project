module GameLogic where

import DataTypes

-- ************************* Game *************************

{-
    5x5 board example:      	

    1) Board init

    Generated board:        Board displayed to user:
       1 2 3 4 5  			   1 2 3 4 5
    1  0 0 1 1 1            1  - - - - -
    2  0 0 1 * 1            2  - - - - -
    3  0 1 1 2 1            3  - - - - -
    4  0 1 * 1 0            4  - - - - -
    5  0 1 1 1 0            5  - - - - -


    Cell value      Display value       Description
    [0,9]           '-'                 Covered cell
    0               ' '                 Uncovered empty cell with no surrounding mines
    [1,8] 		    [1,8]			    Uncovered empty cell with N surrounding mines (up to 8)
    9               '*'				    Uncovered mine cell

    2) User selects cell (1, 5)

       1 2 3 4 5
    1  - - - - 1
    2  - - - - -
    3  - - - - -
    4  - - - - -
    5  - - - - -
    
    3) User selects cell (2, 1)

       1 2 3 4 5
    1  - - - - 1
    2    - - - -
    3  - - - - -
    4  - - - - -
    5  - - - - -

    4) ...

-}

makeGame :: Board -> String -> Game
makeGame board playerName = Game 
    { 
        playerName = playerName,
        gameState  = On,
        board      = board,
        maxRow     = length board,
        maxCol     = length . head $ board
    }

runGame :: Game -> IO ()
runGame (Game player state board maxRow maxCol) = do
    putStrLn ("\nPlayer: " ++ player ++ "\n")
    showBoard board
    putStrLn "Select a cell (row, col):"
    cellPosition <- getLine
    putStrLn "\n"
    runGame (Game player state board maxRow maxCol)

-- ************************* Board *************************

-- Inits cells as Covered and grouped by rows [ [(1,1), (1,2)], [(2,1), (2,2)] ... ]
makeBoard :: Int -> Int -> Board
makeBoard row col = [ [Cell 
    {
        position         = (r, c),
        cellDisplayState = Covered,
        cellState        = AdjacentMine 0
    } | c <- [1..col] ] | r <- [1..row] ]
    where 
        minePositions = [(1,1), (1,2)]

showBoard :: Board -> IO ()
showBoard board = do
  putStrLn $ putStrPadding " " ++ showColNumbers board
  putStrLn ""
  mapM_ (\x -> putStrLn x >> putStrLn "") $ map showRow board
  putStrLn ""

-- Show column numbers as top-header
showColNumbers :: Board -> String
showColNumbers board = concat [putStrPadding $ show col | (row, col) <- (map (position) (head board))]

-- ************************* Row *************************

showRow :: [Cell] -> String
showRow row = showRowNumber row ++ (concat . map (putStrPadding . showCell) $ row)

showRowNumber :: [Cell] -> String
showRowNumber row = putStrPadding $ show $ fst $ position $ head row

-- ************************* Cell *************************

showCell :: Cell -> String
showCell (Cell _ Covered _)                  = "-"
showCell (Cell _ Uncovered Mine)             = "*"
showCell (Cell _ Uncovered (AdjacentMine 0)) = " "
showCell (Cell _ Uncovered (AdjacentMine n)) = (show n)

-- ************************* Helper functions *************************

putStrPadding :: String -> String
putStrPadding s = s ++ (concat $ replicate 3 " ")