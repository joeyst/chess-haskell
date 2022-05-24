import Data.Char
import Data.List

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

getPieceAtSquare :: Board -> Char -> Integer -> Piece
getPieceAtSquare board f r = head [piece square | (square) <- board, (file square) == f, (rank square) == r]

getRank :: Board -> Integer -> [Square]
getRank board n = [square | square <- board, rank square == n]

getFile :: Board -> Char -> [Square]
getFile board ch = [square | square <- board, file square == ch]

sortRank :: [Square] -> [Square]
sortRank [] = []
sortRank (s:ss) = sortRank smaller ++ [s] ++ sortRank larger
  where
    smaller = [a | a <- ss, file a < file s]
    larger = [b | b <- ss, file b > file s]

printRank :: [Square] -> IO ()
printRank rank = putStrLn $ map getPrintable $ map piece $ sortRank rank

main :: IO ()
main = do
  printRank $ getRank genBoard 4