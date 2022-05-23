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

main :: IO ()
main = do
  let moveNumber = 0
  let piece = Piece Pawn Black
  moveNumber <- return (moveNumber+1)
  print(piece)
  print(typeOf piece)
  a <- getLine
  print(a)
  print(moveNumber)

