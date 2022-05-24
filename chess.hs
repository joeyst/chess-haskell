import Data.Char
import Data.List
import Data.Bool
import Data.Typeable

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
  typeOfPiece :: PieceType,
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
  | typeOfPiece piece == Pawn = 'p'
  | typeOfPiece piece == Rook = 'r'
  | typeOfPiece piece == Knight = 'n'
  | typeOfPiece piece == Bishop = 'b'
  | typeOfPiece piece == Queen = 'q'
  | typeOfPiece piece == King = 'k'
  | typeOfPiece piece == Empty = ' '

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

printRank :: Board -> Integer -> IO ()
printRank board n = putStrLn $ map getPrintable $ map piece $ sortRank $ getRank board n

getMaxRank :: Board -> Integer
getMaxRank board = maximum [rank square | square <- board]

filterBoardByRank :: Board -> Integer -> Board
filterBoardByRank board n = [square | square <- board, rank square /= n]

printBoard :: Board -> IO ()
printBoard [] = putStrLn("")
printBoard board = do {
  printRank board $ getMaxRank board
  ; printBoard $ filterBoardByRank board $ getMaxRank board
}

filterBoardBySquare :: Board -> Char -> Integer -> Board
filterBoardBySquare board f r = [square | square <- board, ((file square /= f) || (rank square /= r))]

assignPieceToSquare :: Board -> Char -> Integer -> Piece -> Board
assignPieceToSquare board f r piece = (filterBoardBySquare board f r) ++ [Square f r piece]

removePieceFromSquare :: Board -> Char -> Integer -> Board
removePieceFromSquare board f r = (filterBoardBySquare board f r) ++ assignPieceToSquare board f r (Piece Empty Blank)

updateBoardWithCoords :: Board -> Char -> Integer -> Char -> Integer -> Board
updateBoardWithCoords board ff fr lf lr = removePieceFromSquare (assignPieceToSquare board lf lr $ getPieceAtSquare board ff fr) ff fr

isHeadFile :: String -> String
isHeadFile [] = "invalid"
isHeadFile (x:xs) = if (elem x ['a'..'h']) then xs else "invalid"

isHeadRank :: String -> String
isHeadRank [] = "invalid"
isHeadRank (x:xs) = if (elem x ['1'..'8']) then xs else "invalid"

isParsable :: String -> Bool
isParsable inp = (isHeadRank $ isHeadFile $ isHeadRank $ isHeadFile inp) == ""

isMoving :: String -> Bool
isMoving inp = (inp!!0 /= inp!!2 || inp!!1 /= inp!!3)

findHorizontalChange :: String -> Int
findHorizontalChange inp = ((ord $ inp!!2) - (ord $ inp!!0))

findVerticalChange :: String -> Int
findVerticalChange inp = ((digitToInt $ inp!!3) - (digitToInt $ inp!!1))

isCorrectTeam :: Board -> Int -> File -> Rank -> Bool
isCorrectTeam board turn f r = (not (teamOf (getPieceAtSquare board f r) == Blank) && ((teamOf $ getPieceAtSquare board f r) == Black) == (even turn))

main :: IO ()
main = do
  let board = genBoard
  let updatedBoard = (assignPieceToSquare board 'd' 4 (Piece Pawn Black))
  printBoard updatedBoard
  printBoard board
  inp <- getLine
  print(typeOf inp)
  print(isHeadRank inp)
  print(isHeadFile inp)
  print(isParsable inp)
  print(isMoving inp)
  print(findHorizontalChange inp)
  print(findVerticalChange inp)
  print(isCorrectTeam updatedBoard 2 'e' 4)
  