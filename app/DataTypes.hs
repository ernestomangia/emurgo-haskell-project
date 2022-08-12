module DataTypes where

type Row                = Int
type Col                = Int
type Size               = (Row, Col)
type Position           = (Row, Col)

data CellDisplayState   = Covered
                            | Uncovered deriving (Show, Eq)

data CellState          = Mine 
                            | AdjacentMine Int deriving (Show, Eq)

data Cell               = Cell 
                          {
                            position          :: Position,
                            cellDisplayState  :: CellDisplayState,
                            cellState         :: CellState
                          } deriving (Show)

type Board              = [ [Cell] ]

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

data Difficulty         = Easy
                            | Medium 
                            | Hard 

data Config             = Config 
                          {
                            difficulty :: Difficulty,
                            size       :: Size
                          }