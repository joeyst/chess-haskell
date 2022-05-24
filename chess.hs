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
type Rank = Int

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

getPieceAtSquare :: Board -> Char -> Int -> Piece
getPieceAtSquare board f r = head [piece square | (square) <- board, (file square) == f, (rank square) == r]

getRank :: Board -> Int -> [Square]
getRank board n = [square | square <- board, rank square == n]

getFile :: Board -> Char -> [Square]
getFile board ch = [square | square <- board, file square == ch]

sortRank :: [Square] -> [Square]
sortRank [] = []
sortRank (s:ss) = sortRank smaller ++ [s] ++ sortRank larger
  where
    smaller = [a | a <- ss, file a < file s]
    larger = [b | b <- ss, file b > file s]

printRank :: Board -> Int -> IO ()
printRank board n = putStrLn $ map getPrintable $ map piece $ sortRank $ getRank board n

getMaxRank :: Board -> Int
getMaxRank board = maximum [rank square | square <- board]

filterBoardByRank :: Board -> Int -> Board
filterBoardByRank board n = [square | square <- board, rank square /= n]

printBoard :: Board -> IO ()
printBoard [] = putStrLn("")
printBoard board = do {
  printRank board $ getMaxRank board
  ; printBoard $ filterBoardByRank board $ getMaxRank board
}

filterBoardBySquare :: Board -> Char -> Int -> Board
filterBoardBySquare board f r = [square | square <- board, ((file square /= f) || (rank square /= r))]

assignPieceToSquare :: Board -> Char -> Int -> Piece -> Board
assignPieceToSquare board f r piece = (filterBoardBySquare board f r) ++ [Square f r piece]

removePieceFromSquare :: Board -> Char -> Int -> Board
removePieceFromSquare board f r = (filterBoardBySquare board f r) ++ assignPieceToSquare board f r (Piece Empty Blank)

updateBoardWithCoords :: Board -> Char -> Int -> Char -> Int -> Board
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

type Move = (File, Rank, File, Rank)

parseToMove :: String -> Move
parseToMove str = (str!!0, digitToInt $ str!!1, str!!2, digitToInt $ str!!3)

findHorizontalChange :: Move -> Int
findHorizontalChange (ff,fr,lf,lr) = ((ord lf) - (ord ff))

findVerticalChange :: Move -> Int
findVerticalChange (ff,fr,lf,lr) = (lr - fr)

isCorrectTeam :: Board -> Int -> File -> Rank -> Bool
isCorrectTeam board turn f r = (not (teamOf (getPieceAtSquare board f r) == Blank) && ((teamOf $ getPieceAtSquare board f r) == Black) == (even turn))

getPieceTypeAtSquare :: Board -> File -> Rank -> PieceType
getPieceTypeAtSquare board f r = typeOfPiece $ getPieceAtSquare board f r

filterBoardByRankAndFile :: Board -> Char -> Int -> [Square]
filterBoardByRankAndFile board f r = [square | square <- board, (file square == f), (rank square == r)]

squareExists :: Board -> Char -> Int -> Bool
squareExists board f r = (filterBoardByRankAndFile board f r) /= []

onCross :: Move -> Bool
onCross str = ((findHorizontalChange str == 0) /= (findVerticalChange str == 0))

onDiagonal :: Move -> Bool
onDiagonal str = (((abs $ findHorizontalChange str) == (abs $ findVerticalChange str)) && findHorizontalChange str /= 0)

onL :: Move -> Bool
onL str = ((((abs $ findHorizontalChange str) == 1) && ((abs $ findVerticalChange str) == 2)) || (((abs $ findHorizontalChange str) == 2) && ((abs $ findVerticalChange str) == 1)))

adjustOnBool :: Bool -> Int
adjustOnBool direction = if direction then 1 else -1

inRange :: Int -> Int -> Int -> Bool
inRange outerOne candidate outerTwo = (((outerOne < candidate) && (candidate < outerTwo)) || ((outerOne > candidate) && (candidate > outerTwo)))

collectSquaresOnDiagonal :: Board -> Move -> [Square]
collectSquaresOnDiagonal board (ff,fr,lf,lr) = [square | square <- board, onDiagonal (ff,fr,(file square),(rank square)), onDiagonal (lf,lr,(file square),(rank square)), (inRange (ord ff) (ord (file square)) (ord lf)), (inRange fr (rank square) lr)]

collectSquaresOnHorizontal :: Board -> Move -> [Square]
collectSquaresOnHorizontal board (ff,fr,lf,lr) = [square | square <- board, onCross (ff,fr,(file square),(rank square)), onCross (lf,lr,(file square),(rank square)), ((inRange (ord ff) (ord (file square)) (ord lf)) || (inRange fr (rank square) lr))]

diagPathFree :: Board -> Move -> Bool
diagPathFree board (ff,fr,lf,lr) = [occSquares | occSquares <- (collectSquaresOnDiagonal board (ff,fr,lf,lr)), ((teamOf $ piece occSquares) /= Blank)] == []

crossPathFree :: Board -> Move -> Bool
crossPathFree board (ff,fr,lf,lr) = [occSquares | occSquares <- (collectSquaresOnHorizontal board (ff,fr,lf,lr)), ((teamOf $ piece occSquares) /= Blank)] == []

getTeamAtSquare :: Board -> Char -> Int -> Team
getTeamAtSquare board f r = teamOf $ getPieceAtSquare board f r

crossPathCapturable :: Board -> Move -> Bool
crossPathCapturable board (ff,fr,lf,lr) = ((crossPathFree board (ff,fr,lf,lr)) && ((getTeamAtSquare board ff fr) /= (getTeamAtSquare board lf lr)))

diagPathCapturable :: Board -> Move -> Bool
diagPathCapturable board (ff,fr,lf,lr) = ((diagPathFree board (ff,fr,lf,lr)) && ((getTeamAtSquare board ff fr) /= (getTeamAtSquare board lf lr)))

lPathCapturable :: Board -> Move -> Bool
lPathCapturable board (ff,fr,lf,lr) = (getTeamAtSquare board ff fr) /= (getTeamAtSquare board lf lr)

adjustPawnOnTeam :: Team -> Int
adjustPawnOnTeam team = if team == White then 2 else 7

capturable :: Board -> PieceType -> Move -> Bool
capturable board Rook move = crossPathCapturable board move && onCross move
capturable board Bishop move = diagPathCapturable board move && onDiagonal move
capturable board Knight move = lPathCapturable board move && onL move
capturable board Queen move = ((onDiagonal move && diagPathCapturable board move) || (onCross move && crossPathCapturable board move))
capturable board Pawn (ff,fr,lf,lr) 
  | getTeamAtSquare board ff fr == Blank = False
  | ((abs $ findHorizontalChange (ff,fr,lf,lr)) == 1) && (findVerticalChange (ff,fr,lf,lr) == (adjustOnBool (getTeamAtSquare board ff fr == White))) && (getTeamAtSquare board lf lr /= getTeamAtSquare board ff fr) && (getTeamAtSquare board lf lr /= Blank) = True
  | ((abs $ findHorizontalChange (ff,fr,lf,lr)) == 0) && (findVerticalChange (ff,fr,lf,lr) == (adjustOnBool (getTeamAtSquare board ff fr == White))) && (getTeamAtSquare board lf lr == Blank) = True
  | ((abs $ findHorizontalChange (ff,fr,lf,lr)) == 0) && (findVerticalChange (ff,fr,lf,lr) == (2 * (adjustOnBool (getTeamAtSquare board ff fr == White)))) && (getTeamAtSquare board lf (fr + (adjustOnBool (getTeamAtSquare board ff fr == White))) == Blank) && (getTeamAtSquare board lf lr == Blank) && (adjustPawnOnTeam $ getTeamAtSquare board ff fr) == fr = True
  | otherwise = False
capturable board King (ff,fr,lf,lr)
  | ((abs $ findHorizontalChange (ff,fr,lf,lr)) <= 1) && ((abs $ findVerticalChange (ff,fr,lf,lr)) <= 1) && ((getTeamAtSquare board lf lr) /= (getTeamAtSquare board ff fr)) = True
  | otherwise = False
capturable board Empty move = False

addRankOfPieces :: Rank -> PieceType -> Team -> Board
addRankOfPieces rank pieceType team = [(Square file rank (Piece pieceType team)) | file <- ['a'..'h']]

setUpRank :: Board -> Rank -> PieceType -> Team -> Board
setUpRank board rank pieceType team = ((addRankOfPieces rank pieceType team) ++ (filterBoardByRank board rank))

addRankOfCustomPieces :: Rank -> [Piece] -> [File] -> Board
addRankOfCustomPieces rank [] [] = []
addRankOfCustomPieces rank (pc:pcs) (f:fs) = (Square f rank pc) : (addRankOfCustomPieces rank pcs fs)

setUpCustomRank :: Board -> Rank -> [Piece] -> [File] -> Board
setUpCustomRank board rank pieces files = ((addRankOfCustomPieces rank pieces files) ++ (filterBoardByRank board rank))

setupBoard :: Board
setupBoard = do {
  let board = genBoard
  ; let boardWithWhitePawns = (setUpRank board 2 Pawn White)
  ; let boardWithBlackPawns = (setUpRank boardWithWhitePawns 7 Pawn Black)
  ; let boardWithBlackPieces = (setUpCustomRank boardWithBlackPawns 8 [(Piece Rook Black), (Piece Knight Black), (Piece Bishop Black), (Piece Queen Black), (Piece King Black), (Piece Bishop Black), (Piece Knight Black), (Piece Rook Black)] ['a'..'h'])
  ; boardWithWhitePieces <- (setUpCustomRank boardWithBlackPieces 1 [(Piece Rook White), (Piece Knight White), (Piece Bishop White), (Piece Queen White), (Piece King White), (Piece Bishop White), (Piece Knight White), (Piece Rook White)] ['a'..'h'])
  ; return boardWithWhitePieces
} 

type Turn = Int
getValidMove :: Board -> Turn -> IO Move
-- not implemented: castling, check, en passent
getValidMove board turn = do
  print ("Enter move: ")
  candidate <- getLine
  if not $ isParsable candidate 
    then (getValidMove board turn)
  else do {
    let parsedCandidate = parseToMove candidate
    ; print (findHorizontalChange parsedCandidate)
    ; print (findVerticalChange parsedCandidate)
    ; let (ff, fr, lf, lr) = parseToMove candidate
    ; if ((not $ squareExists board ff fr) || (not $ squareExists board lf lr)) then getValidMove board turn else do {
      if (not $ isCorrectTeam board turn ff fr) then getValidMove board turn else
        if (not $ capturable board (getPieceTypeAtSquare board ff fr) parsedCandidate) then getValidMove board turn else
          return parsedCandidate
  }
}

playMove :: Board -> Turn -> IO ()
playMove board turn = do {
  printBoard board
  ; (ff,fr,lf,lr) <- getValidMove board turn
  ; playMove (updateBoardWithCoords board ff fr lf lr) (turn + 1)
}

main :: IO ()
main = do
  let board = setupBoard
  let turn = 1
  playMove board turn
  print("")