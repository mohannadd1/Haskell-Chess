import Data.List (find)
import Data.Char 
import Data.Maybe (isNothing)

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard =
  (White,
  [ R ('h',1), N ('g',1), B ('f',1), K ('e',1), Q ('d',1), B ('c',1), N ('b',1), R ('a',1),
    P ('h',2), P ('g',2), P ('f',2), P ('e',2), P ('d',2), P ('c',2), P ('b',2), P ('a',2)
  ],
  [ R ('h',8), N ('g',8), B ('f',8), K ('e',8), Q ('d',8), B ('c',8), N ('b',8), R ('a',8),
    P ('h',7), P ('g',7), P ('f',7), P ('e',7), P ('d',7), P ('c',7), P ('b',5	), P ('a',7)
  ])  

visualizeBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = boardStr ++ turnStr
  where
    boardStr = unlines (headerRow  : boardRows)
    headerRow = "     a    b    c    d    e    f    g    h"
    
    boardRows = reverse [rowStr rowNum | rowNum <- [1..8]]
    rowStr rowNum = show rowNum ++ " | " ++ concat [pieceStr (col, rowNum) | col <- ['a'..'h']] ++ " |"
    pieceStr loc = case findPiece loc of
      Just (P _) -> "P" ++ pieceColorSuffix loc ++ " | "
      Just (N _) -> "N" ++ pieceColorSuffix loc ++ " | "
      Just (K _) -> "K" ++ pieceColorSuffix loc ++ " | "
      Just (Q _) -> "Q" ++ pieceColorSuffix loc ++ " | "
      Just (R _) -> "R" ++ pieceColorSuffix loc ++ " | "
      Just (B _) -> "B" ++ pieceColorSuffix loc ++ " | "
      Nothing -> "   | "
    pieceColorSuffix loc
      | isPieceAt loc whitePieces = "W"
      | isPieceAt loc blackPieces = "B"
      | otherwise = ""
    isPieceAt loc pieces = any (\piece -> case piece of { P l -> l == loc; N l -> l == loc; K l -> l == loc; Q l -> l == loc; R l -> l == loc; B l -> l == loc }) pieces
    findPiece loc = find (\piece -> case piece of { P l -> l == loc; N l -> l == loc; K l -> l == loc; Q l -> l == loc; R l -> l == loc; B l -> l == loc }) (whitePieces ++ blackPieces)
    turnStr = "Turn: " ++ show player




isLegal :: Piece -> Board -> Location -> Bool
isLegal (P (col, row)) (player, whitePieces, blackPieces) (newCol, newRow)
  | player == White && row == 2 = ( (diff == 1 || diff == 2) && col == newCol && isNothing targetPiece && isNothing ( findPiece (col , row + 1) (whitePieces ++ blackPieces)) )
  | player == White = (diff == 1 && col == newCol && isNothing targetPiece)
  | player == Black && row == 7 = ( (diff == -1 || diff == -2) && col == newCol && isNothing targetPiece &&  isNothing (  findPiece (col, row -1) (whitePieces ++ blackPieces) ) )
  | player == Black = (diff == -1 && col == newCol && isNothing targetPiece)
  | otherwise = False
  where
    diff = newRow - row
    targetPiece = findPiece (newCol, newRow) (whitePieces ++ blackPieces)


isLegal (R (col, row)) (player, whitePieces, blackPieces) (newCol, newRow)
  | player == White || player == Black  =
    -- Rook can move vertically or horizontally
   ( col == newCol || row == newRow) && isNothing targetPiece 	
  where
    targetPiece = findPiece (newCol, newRow) (whitePieces ++ blackPieces)

isLegal (N (col, row)) (player, whitePieces, blackPieces) (newCol, newRow)
  -- Knight can move in an L-shape
  -- needs to be fixed
  | player == White = ( (abs (ord col - ord newCol) == 1 && abs (row - newRow) == 2) || (abs (ord col - ord newCol) == 2 && abs (row - newRow) == 1) ) && isNothing (  findPiece (newCol,newRow) (whitePieces++blackPieces) )
  | player == Black = ( (abs (ord col - ord newCol) == 1 && abs (row - newRow) == 2) || (abs (ord col - ord newCol) == 2 && abs (row - newRow) == 1) ) && isNothing (  findPiece (newCol,newRow) (whitePieces++blackPieces) )

isLegal (B (col, row)) (player, whitePieces, blackPieces) (newCol, newRow)
  | newCol < col && newRow<row = empDiagDownLeft (col,row) (player, whitePieces, blackPieces) (newCol, newRow)
  |  newCol > col && newRow<row = empDiagDownRight (col,row) (player, whitePieces, blackPieces) (newCol, newRow)
  |  newCol > col && newRow>row = empDiagUpRight (col,row) (player, whitePieces, blackPieces) (newCol, newRow)
  | newCol < col && newRow>row = empDiagUpLeft (col,row) (player, whitePieces, blackPieces) (newCol, newRow)
  

isLegal (Q (col, row)) (player, whitePieces, blackPieces) (newCol, newRow)
  | player == White || player == Black =
    -- Queen can move vertically, horizontally, or diagonally
    col == newCol || row == newRow || abs (ord col - ord newCol) == abs (row - newRow)
  where
    targetPiece = findPiece (newCol, newRow) (whitePieces ++ blackPieces)

isLegal (K (col, row)) (player, whitePieces, blackPieces) (newCol, newRow)
  | player == White || player == Black =
    -- King can move one step in any direction
    abs (ord col - ord newCol) <= 1 && abs (row - newRow) <= 1
  where
    targetPiece = findPiece (newCol, newRow) (whitePieces ++ blackPieces)


--Bishop legal moves helper
empDiagDownLeft :: Location -> Board -> Location  -> Bool
empDiagDownLeft (col,row) (player, whitePieces, blackPieces) (newCol, newRow) 
 | (col,row) == (newCol,newRow) && isNothing( findPiece (col,row) (whitePieces++blackPieces)) = True
 | col >= 'a' && row >= 1 =False 
 |(col,row) /= (newCol,newRow) && isNothing(findPiece (col,row) (whitePieces++blackPieces)) == False = False
 |player == White && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) whitePieces)  = False 
 |player == Black && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) blackPieces) = False 
 |otherwise = empDiagDownLeft (pred col,row-1) (player,whitePieces,blackPieces)(newCol,newRow)
 
 
empDiagDownRight :: Location -> Board -> Location  -> Bool
empDiagDownRight (col,row) (player, whitePieces, blackPieces) (newCol, newRow) 
  | (col,row) == (newCol,newRow) && isNothing( findPiece (col,row) (whitePieces++blackPieces)) = True
 | col <= 'h' && row >= 1 =False 
 |(col,row) /= (newCol,newRow) && isNothing(findPiece (col,row) (whitePieces++blackPieces)) == False = False
 |player == White && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) whitePieces)  = False 
 |player == Black && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) blackPieces) = False

 |otherwise = empDiagDownRight (succ col,row-1) (player,whitePieces,blackPieces)(newCol,newRow)
 
empDiagUpRight :: Location -> Board -> Location  -> Bool
empDiagUpRight (col,row) (player, whitePieces, blackPieces) (newCol, newRow) 
 | (col,row) == (newCol,newRow) && isNothing( findPiece (col,row) (whitePieces++blackPieces)) = True
 | col <= 'h' && row <= 8 =False 
 |(col,row) /= (newCol,newRow) && isNothing(findPiece (col,row) (whitePieces++blackPieces)) == False = False
 |player == White && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) whitePieces)  = False 
 |player == Black && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) blackPieces) = False 
 |otherwise = empDiagUpRight (succ col,row+1) (player,whitePieces,blackPieces)(newCol,newRow)
 
 
empDiagUpLeft :: Location -> Board -> Location  -> Bool
empDiagUpLeft (col,row) (player, whitePieces, blackPieces) (newCol, newRow) 
 | (col,row) == (newCol,newRow) && isNothing( findPiece (col,row) (whitePieces++blackPieces)) = True
 | col >= 'a' && row <= 8 =False 
 |(col,row) /= (newCol,newRow) && isNothing(findPiece (col,row) (whitePieces++blackPieces)) == False = False
 |player == White && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) whitePieces)  = False 
 |player == Black && (col,row) == (newCol,newRow) &&  isNothing(findPiece (col,row) blackPieces) = False 
 |otherwise = empDiagUpLeft (pred col,row+1) (player,whitePieces,blackPieces)(newCol,newRow)





getLocation :: Piece -> Location
getLocation (P location) = location
getLocation (N location) = location
getLocation (B location) = location
getLocation (R location) = location
getLocation (Q location) = location
getLocation (K location) = location


suggestMove :: Piece -> Board -> [Location]
suggestMove piece (player,whitePieces,blackPieces)  = suggestMoveHelper piece (player,whitePieces,blackPieces) ('a',8)

suggestMoveHelper :: Piece -> Board -> Location -> [Location]
suggestMoveHelper piece board ('i',1) = []
suggestMoveHelper piece board (col,row) | col > 'h' = suggestMoveHelper piece board ('a',row-1)
                                        | isLegal piece board (col,row) = (col,row) : suggestMoveHelper piece board (succ col,row)
                                        | otherwise = suggestMoveHelper piece board (succ col,row)

findPiece :: Location -> [Piece] -> Maybe Piece
findPiece loc pieces = find (\piece -> case piece of { P l -> l == loc; N l -> l == loc; K l -> l == loc; Q l -> l == loc; R l -> l == loc; B l -> l == loc }) pieces

isPiece :: Location -> [Piece] -> Bool
isPiece location = foldr (\p -> (||) (location == getLocation p)) False

move :: Piece -> Location -> Board -> Board
move piece@(P _) loc board@(player, _, _) | player == Black = error "This is White player’s turn, Black can’t move." 
move piece loc board | not (isLegal piece board loc) = error ("Illegal move for piece " ++ show piece)
move piece loc (player, whitePieces, blackPieces) = case player of
  White -> (Black, movePiece piece loc whitePieces, blackPieces)
  Black -> (White, whitePieces, movePiece piece loc blackPieces)
  where
    movePiece :: Piece -> Location -> [Piece] -> [Piece]
    movePiece piece loc = map (\p -> if p == piece then updatePieceLoc p loc else p)
    updatePieceLoc :: Piece -> Location -> Piece
    updatePieceLoc (P _) loc = (P loc)
    updatePieceLoc (N _) loc = (N loc)
    updatePieceLoc (K _) loc = (K loc)
    updatePieceLoc (Q _) loc = (Q loc)
    updatePieceLoc (R _) loc = (R loc)
    updatePieceLoc (B _) loc = (B loc)
	
	
	
	
	