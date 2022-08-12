module GameLogic where

import DataTypes

import Control.Monad    (replicateM)
import Data.List        (intersect)
import System.IO        (hFlush, stdout)
import System.Random
import Text.Read        (readMaybe)

---------------------------------------------------------------- Config ----------------------------------------------------------------

makeConfig :: IO Config
makeConfig = getDifficulty >>= \d ->
    return Config 
            { 
                difficulty = d,
                size       = getBoardSize d
            }
    
getDifficulty :: IO Difficulty
getDifficulty = do
    difficulty <- readMaybe <$> putStrGetLine "Please, select difficulty:\n 1) Easy\n 2) Medium\n 3) Hard\nYour choise? "
    case difficulty of
        Just 1  -> return Easy
        Just 2  -> return Medium
        Just 3  -> return Hard
        _       -> putStrLn "Invalid selection! \n" >> getDifficulty

---------------------------------------------------------------- /Config ----------------------------------------------------------------

---------------------------------------------------------------- Game ----------------------------------------------------------------

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

getBoardSize :: Difficulty -> Size
getBoardSize Easy   = (5, 5)
getBoardSize Medium = (15, 15)
getBoardSize Hard   = (16, 30)

-- Inits cells as Covered and grouped by rows [ [(1,1), (1,2), ...], [(2,1), (2,2), ...], ... ]
makeBoard :: Difficulty -> IO Board
makeBoard difficulty = do
    let (row, col) = getBoardSize difficulty 
    minePositions <- replicateM (calculateMineCount row col) $ getRandomPosition row col
    return $ [ [Cell 
            {
                position         = (r, c),
                cellDisplayState = Covered,
                cellState        = case elem (r, c) minePositions of 
                                        True  -> Mine
                                        False -> AdjacentMine (calculateAdjacentMines (r, c) minePositions)
            } | c <- [1..col] ] | r <- [1..row] ]

-- Assign 15% of mines
calculateMineCount :: Row -> Col -> Int
calculateMineCount row col = ceiling $ fromIntegral (row * col) * 0.15

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
getCellPosition :: Row -> Col -> IO Position
getCellPosition maxRow maxCol = do
    position <- map readMaybe . words <$> putStrGetLine "Select a cell (row, col): "
    case position of
        [Just row, Just col] -> if validateCellPosition row col maxRow maxCol
                                    then return (row, col)
                                    else putStrLn "Invalid cell! \n" >> getCellPosition maxRow maxCol
        _                    -> putStrLn "Invalid cell! \n" >> getCellPosition maxRow maxCol




validateCellPosition :: Row -> Col -> Row -> Col -> Bool
validateCellPosition row col maxRow maxCol = row > 0 && row <= maxRow && col > 0 && col <= maxCol

calculateAdjacentMines :: Position -> [Position] -> Int
calculateAdjacentMines pos mines = length $ intersect mines (getAdjacentPositions pos)

getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (row, col) = filter (\(r, c) -> r > 0 && c > 0) 
                              [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1), 
                               (row    , col - 1),                 (row    , col + 1),
                               (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]

getRandomPosition :: Row -> Col -> IO Position
getRandomPosition maxRow maxCol = do
    row <- randomRIO (1, maxRow)
    col <- randomRIO (1, maxCol)
    return $ (row, col)

---------------------------------------------------------------- /Cell ----------------------------------------------------------------

---------------------------------------------------------------- Helper functions ----------------------------------------------------------------

putStrPadding :: String -> String
putStrPadding s = s ++ (concat $ replicate 3 " ")

putStrGetLine :: String -> IO String
putStrGetLine text = putStr text >> hFlush stdout >> getLine

---------------------------------------------------------------- /Helper functions ----------------------------------------------------------------