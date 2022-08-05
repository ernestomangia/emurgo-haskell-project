module DataTypes where

type Board            = [ [Cell] ]

type Position         = (Int, Int)

data Cell             = Cell 
                        {
                            position          :: Position,
                            cellDisplayState  :: CellDisplayState,
                            cellState         :: CellState
                        } deriving (Show)

data CellDisplayState  = Covered
                         | Uncovered deriving (Show, Eq)

data CellState         = Mine
                         | AdjacentMine Int deriving (Show, Eq)

data GameState         = On 
                         | Won
                         | Lost

data Game              = Game 
                         { 
                           playerName :: String,
                           gameState  :: GameState,
                           board      :: Board,
                           maxRow     :: Int,
                           maxCol     :: Int
                         }