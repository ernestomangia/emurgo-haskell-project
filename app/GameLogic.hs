module GameLogic where

import DataTypes

import System.IO (hFlush, stdout)
import Data.List (intersect)
import Text.Read (readMaybe)

---------------------------------------------------------------- Game ----------------------------------------------------------------

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
    cellPosition <- getCellPosition maxRow maxCol                                       -- Get position (row, col)
    putStrLn "\n"
    let updatedBoard = updateBoard board cellPosition                                   -- Update board at the selected position
    case checkGameState updatedBoard of                                                 -- Check if Lost, Won or On
        Lost -> do
                showBoard $ uncoverBoard board                                          -- Show board uncovered
                putStrLn "******************** GAME OVER! ******************** \n"
        Won  -> do
                showBoard $ uncoverBoard board                                          -- Show board uncovered
                putStrLn "******************** YOU WIN! ******************** \n"
        On   -> runGame (Game player state updatedBoard maxRow maxCol)                  -- Game continues

checkGameState :: Board -> GameState
checkGameState board = do
    let concatBoard = concat board
    let anyMineUncovered = any (\x -> cellState x == Mine 
                                      && cellDisplayState x == Uncovered) $ concatBoard
    case anyMineUncovered of
        True    -> Lost
        _       -> do 
                   let anyNonMineCovered = any (\x -> (case cellState x of AdjacentMine _ -> True; _ -> False) 
                                                       && cellDisplayState x == Covered) $ concatBoard
                   case anyNonMineCovered of
                        True    -> On
                        _       -> Won

---------------------------------------------------------------- /Game ----------------------------------------------------------------

---------------------------------------------------------------- Board ----------------------------------------------------------------

-- Inits cells as Covered and grouped by rows [ [(1,1), (1,2)], [(2,1), (2,2)] ... ]
makeBoard :: Int -> Int -> Board
makeBoard row col = [ [Cell 
    {
        position         = (r, c),
        cellDisplayState = Covered,
        cellState        = case elem (r, c) minePositions of 
                                True  -> Mine
                                False -> AdjacentMine (calculateAdjacentMines (r, c) minePositions)
    } | c <- [1..col] ] | r <- [1..row] ]
    where 
        minePositions = [(1,1), (1,2)]  -- TODO make this random

showBoard :: Board -> IO ()
showBoard board = do
    putStrLn $ putStrPadding " " ++ showColNumbers board
    putStrLn ""
    mapM_ (\x -> putStrLn x >> putStrLn "") $ map showRow board
    putStrLn ""

-- Show column numbers as top-header
showColNumbers :: Board -> String
showColNumbers board = concat [putStrPadding $ show col | (row, col) <- (map (position) (head board))]

uncoverBoard :: Board -> Board
uncoverBoard board = map (map (\x -> x { cellDisplayState = Uncovered })) $ board

updateBoard :: Board -> Position -> Board
updateBoard board pos = map (map (\c -> if position c == pos 
                                            then c { cellDisplayState = Uncovered }
                                            else c )) $ board

---------------------------------------------------------------- /Board ----------------------------------------------------------------

---------------------------------------------------------------- Row ----------------------------------------------------------------

showRow :: [Cell] -> String
showRow row = showRowNumber row ++ (concat . map (putStrPadding . showCell) $ row)

showRowNumber :: [Cell] -> String
showRowNumber row = putStrPadding $ show $ fst $ position $ head row

---------------------------------------------------------------- /Row ----------------------------------------------------------------

---------------------------------------------------------------- Cell ----------------------------------------------------------------

showCell :: Cell -> String
showCell (Cell _ Covered _)                  = "-"
showCell (Cell _ Uncovered Mine)             = "*"
showCell (Cell _ Uncovered (AdjacentMine 0)) = " "
showCell (Cell _ Uncovered (AdjacentMine n)) = (show n)

-- Read position: (Int, Int)
getCellPosition :: Int -> Int -> IO Position
getCellPosition maxRow maxCol = do
    position <- map readMaybe . words <$> putStrGetLine "Select a cell (row, col): "
    case position of
        [Just row, Just col] -> if validateCellPosition row col maxRow maxCol
                                    then return (row, col)
                                    else putStrLn "Invalid cell! \n" >> getCellPosition maxRow maxCol
        _                    -> putStrLn "Invalid cell! \n" >> getCellPosition maxRow maxCol

validateCellPosition :: Int -> Int -> Int -> Int -> Bool
validateCellPosition row col maxRow maxCol = row > 0 && row <= maxRow && col > 0 && col <= maxCol

calculateAdjacentMines :: Position -> [Position] -> Int
calculateAdjacentMines pos mines = length $ intersect mines (getAdjacentPositions pos)

getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (r, c) = filter (\(x, y) -> x > 0 && y > 0) 
                              [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1), 
                               (r, c - 1),  {- (r, c) -}   (r, c + 1),
                               (r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]

---------------------------------------------------------------- /Cell ----------------------------------------------------------------

---------------------------------------------------------------- Helper functions ----------------------------------------------------------------

putStrPadding :: String -> String
putStrPadding s = s ++ (concat $ replicate 3 " ")

putStrGetLine :: String -> IO String
putStrGetLine text = putStr text >> hFlush stdout >> getLine

---------------------------------------------------------------- /Helper functions ----------------------------------------------------------------