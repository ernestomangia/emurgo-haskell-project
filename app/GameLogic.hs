{-# LANGUAGE FlexibleContexts #-}

module GameLogic where

import DataTypes

import Control.Monad    (replicateM)
import Control.Monad.Reader
import Control.Monad.State
import Data.List        (intersect)
import System.Console.ANSI
import System.IO        (hFlush, stdout)
import System.Random
import Text.Read        (readMaybe)

---------------------------------------------------------------- Config ----------------------------------------------------------------

makeConfig :: IO Config
makeConfig = do
    playerName <- putStrGetLine "Please, enter your name: "
    putStrLn ""
    difficulty <- getDifficulty
    putStrLn ""
    return $ Config 
            { 
                playerName = playerName,
                difficulty = difficulty,
                boardSize  = getBoardSize difficulty,
                mineRatio  = getMineRatio difficulty
            }
    
getDifficulty :: IO Difficulty
getDifficulty = do
    difficulty <- readMaybe <$> putStrGetLine "Please, select difficulty:\n 1) Easy\n 2) Medium\n 3) Hard\nYour choise? "
    case difficulty of
        Just 1  -> return Easy
        Just 2  -> return Medium
        Just 3  -> return Hard
        _       -> putStrLnError "Invalid selection! \n" >> getDifficulty

showPlayer :: String -> IO ()
showPlayer name = putStrLn $ "\nPlayer: " ++ name ++ "\n"

getBoardSize :: Difficulty -> BoardSize
getBoardSize Easy   = (4, 4)
getBoardSize Medium = (15, 15)
getBoardSize Hard   = (16, 30)

getMineRatio :: Difficulty -> MineRatio
getMineRatio Easy   = 0.10
getMineRatio Medium = 0.15
getMineRatio Hard   = 0.25

---------------------------------------------------------------- /Config ----------------------------------------------------------------

---------------------------------------------------------------- Game ----------------------------------------------------------------

makeGame :: Config -> IO Game
makeGame config = do
    board  <- makeBoard config 
    return $ Game 
        { 
            gameState  = On,
            gameBoard  = board
        }

runGame :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
runGame = do
    config <- ask                                                                            -- Get config from ReaderT
    game   <- get                                                                            -- Get game from StateT
    let name  = playerName config
        size  = boardSize config
        board = gameBoard game
        state = gameState game
    liftIO $ showPlayer name
    case state of                                                                            -- Check if Lost, Won or On
        Lost -> do
                liftIO $ showBoard . uncoverBoard $ board                                    -- Show board uncovered
                liftIO $ putStrLn "******************** GAME OVER! ******************** \n"
        Won  -> do
                liftIO $ showBoard . uncoverBoard $ board                                    -- Show board uncovered
                liftIO $ putStrLn "******************** YOU WIN! ******************** \n"
        On   -> do
                liftIO $ showBoard board
                liftIO $ putStrLn ("-----------------------------------------\n")
                cellPosition <- liftIO $ getCellPosition size                                -- Get position (row, col)
                updateBoard cellPosition                                                     -- Update board with the selected position
                updateGameState
                runGame                                                                      -- Game continues

updateGameState :: (MonadState Game m) => m ()
updateGameState = do
    game   <- get
    put ( game { gameState = getGameState $ gameBoard game } )

getGameState :: GameBoard -> GameState
getGameState board 
    | anyMineUncovered board = Lost
    | anyEmptyCovered board  = On
    | otherwise              = Won

anyMineUncovered :: GameBoard -> Bool
anyMineUncovered board = any (\c -> cellState c == Mine  
                                    && cellDisplayState c == Uncovered) $ concat board

anyEmptyCovered :: GameBoard -> Bool
anyEmptyCovered board = any (\c -> (case cellState c of AdjacentMine _ -> True; _ -> False) 
                                    && cellDisplayState c == Covered) $ concat board

---------------------------------------------------------------- /Game ----------------------------------------------------------------

---------------------------------------------------------------- Board ----------------------------------------------------------------

-- Inits cells as Covered and grouped by rows [ [(1,1), (1,2), ...], [(2,1), (2,2), ...], ... ]
makeBoard :: Config -> IO GameBoard
makeBoard config = do
    let (maxRow, maxCol) = boardSize config
        mRatio           = mineRatio config
        mineCount        = calculateMineCount (maxRow, maxCol) mRatio
    mines <- replicateM mineCount $ getRandomPosition (maxRow, maxCol)
    return $ [ [Cell 
            {
                position         = (r, c),
                cellDisplayState = Covered,
                cellState        = getCellState (r, c) mines
            } | c <- [1..maxCol] ] | r <- [1..maxRow] ]

calculateMineCount :: BoardSize -> MineRatio -> Int
calculateMineCount (maxRow, maxCol) mineRatio = ceiling $ fromIntegral (maxRow * maxCol) * mineRatio

showBoard :: GameBoard -> IO ()
showBoard board = do
    putStrLn $ putStrPadding " " ++ showColNumbers board
    putStrLn ""
    mapM_ (\x -> putStrLn x >> putStrLn "") $ map showRow board
    putStrLn ""

-- Show column numbers as top-header
showColNumbers :: GameBoard -> String
showColNumbers board = concat [putStrPadding $ show col | (row, col) <- (map (position) (head board))]

uncoverBoard :: GameBoard -> GameBoard
uncoverBoard board = map (map (\x -> x { cellDisplayState = Uncovered })) $ board

updateBoard :: (MonadIO m, MonadReader Config m, MonadState Game m) => Position -> m ()
updateBoard pos = do
    game   <- get 
    let updatedBoard = map (map (\c -> if position c == pos 
                                            then c { cellDisplayState = Uncovered }
                                            else c )) $ gameBoard game
    put ( game { gameBoard = updatedBoard } )

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
getCellPosition :: BoardSize -> IO Position
getCellPosition (maxRow, maxCol) = do
    position <- map readMaybe . words <$> putStrGetLine "Select a cell (row, col): "
    case position of
        [Just row, Just col] -> if validateCellPosition row col (maxRow, maxCol)
                                    then return (row, col)
                                    else putStrLnError "Position out of boundaries! \n" >> getCellPosition (maxRow, maxCol)
        _                    -> putStrLnError "Invalid format! \n" >> getCellPosition (maxRow, maxCol)

validateCellPosition :: Row -> Col -> BoardSize -> Bool
validateCellPosition row col (maxRow, maxCol) = row > 0 && row <= maxRow && col > 0 && col <= maxCol

getCellState :: Position -> [Position] -> CellState
getCellState pos mines = case elem pos mines of 
                            True  -> Mine
                            False -> AdjacentMine (calculateAdjacentMines pos mines)

calculateAdjacentMines :: Position -> [Position] -> Int
calculateAdjacentMines pos mines = length $ intersect mines (getAdjacentPositions pos)

getAdjacentPositions :: Position -> [Position]
getAdjacentPositions (row, col) = filter (\(r, c) -> r > 0 && c > 0) 
                              [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1), 
                               (row    , col - 1),                 (row    , col + 1),
                               (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]

getRandomPosition :: BoardSize -> IO Position
getRandomPosition (maxRow, maxCol) = do
    row <- randomRIO (1, maxRow)
    col <- randomRIO (1, maxCol)
    return $ (row, col)

---------------------------------------------------------------- /Cell ----------------------------------------------------------------

---------------------------------------------------------------- Helper functions ----------------------------------------------------------------

putStrPadding :: String -> String
putStrPadding s = s ++ (concat $ replicate 3 " ")

putStrGetLine :: String -> IO String
putStrGetLine text = putStr text >> hFlush stdout >> getLine

putStrLnError :: String -> IO ()
putStrLnError text = do
    setSGR [ SetColor Foreground Vivid Red ]
    putStrLn text
    setSGR [ Reset ]

---------------------------------------------------------------- /Helper functions ----------------------------------------------------------------