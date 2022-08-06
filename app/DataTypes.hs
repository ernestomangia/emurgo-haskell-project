module DataTypes where

type Row                = Int

type Col                = Int

type Position           = (Row, Col)

type Board              = [ [Cell] ]

data Cell               = Cell 
                        {
                            position          :: Position,
                            cellDisplayState  :: CellDisplayState,
                            cellState         :: CellState
                        } deriving (Show)

data CellDisplayState   = Covered
                            | Uncovered deriving (Show, Eq)

data CellState          = Mine 
                            | AdjacentMine Int deriving (Show, Eq)

data GameState          = On 
                         | Won
                         | Lost

data Game               = Game 
                        {
                            playerName :: String,
                            gameState  :: GameState,
                            board      :: Board,
                            maxRow     :: Row,
                            maxCol     :: Col
                        }