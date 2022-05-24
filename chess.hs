data PieceType = 
  Pawn | 
  Rook |
  Knight |
  Bishop |
  Queen |
  King deriving (Show, Eq)

data Team =
  Black |
  White deriving (Show, Eq)

data Piece = Piece {
  typeOf :: PieceType,
  teamOf :: Team
} deriving (Show, Eq)

type File = Char
type Rank = Integer

data Square = Square {
  file :: File,
  rank :: Rank,
  piece :: Maybe Piece
} deriving (Show, Eq)

type Board = [Square]
genBoard :: Board
genBoard = [Square file rank Nothing | file <- ['a'..'h'], rank <- [1..8]]

main :: IO ()
main = do
  print(genBoard)