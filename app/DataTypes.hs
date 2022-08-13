module DataTypes where

type Row                = Int
type Col                = Int
type Count              = Int
type BoardSize          = (Row, Col)
type Position           = (Row, Col)
type PlayerName         = String
type MineRatio          = Float

data CellDisplayState   = Covered
                            | Uncovered deriving (Show, Eq)

data CellState          = Mine 
                            | AdjacentMine Count deriving (Show, Eq)

data Cell               = Cell 
                          {
                            position          :: Position,
                            cellDisplayState  :: CellDisplayState,
                            cellState         :: CellState
                          } deriving (Show)

type GameBoard          = [ [Cell] ]

data GameState          = On 
                            | Won
                            | Lost

data Game               = Game 
                          {
                            gameState  :: GameState,
                            gameBoard  :: GameBoard
                          }

data Difficulty         = Easy
                            | Medium 
                            | Hard 

data Config             = Config 
                          {
                            playerName :: PlayerName,
                            difficulty :: Difficulty,
                            boardSize  :: BoardSize,
                            mineRatio  :: MineRatio
                          }