import Data.Char

data PieceType = 
  Pawn | 
  Rook |
  Knight |
  Bishop |
  Queen |
  King |
  Empty deriving (Show, Eq)

data Team =
  Black |
  White |
  Blank deriving (Show, Eq)

data Piece = Piece {
  typeOf :: PieceType,
  teamOf :: Team
} deriving (Show, Eq)

type File = Char
type Rank = Integer

data Square = Square {
  file :: File,
  rank :: Rank,
  piece :: Piece
} deriving (Show, Eq)

getLetter :: Piece -> Char
getLetter piece
  | typeOf piece == Pawn = 'p'
  | typeOf piece == Rook = 'r'
  | typeOf piece == Knight = 'n'
  | typeOf piece == Bishop = 'b'
  | typeOf piece == Queen = 'q'
  | typeOf piece == King = 'k'
  | typeOf piece == Empty = ' '

convertCase :: Piece -> Char -> Char
convertCase piece ch
  | teamOf piece == White = toUpper ch
  | teamOf piece == Black = toLower ch
  | teamOf piece == Blank = ch

getPrintable :: Piece -> Char
getPrintable piece = convertCase piece $ getLetter piece

type Board = [Square]
genBoard :: Board
genBoard = [Square file rank (Piece Empty Blank) | file <- ['a'..'h'], rank <- [1..8]]

main :: IO ()
main = do
  putChar $ getPrintable (Piece Pawn Black)
  putChar $ getPrintable (Piece Knight White)
  putChar $ getPrintable (Piece Empty Blank)