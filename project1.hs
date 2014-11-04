{- CPSC 312, Project 1
   Allan Fong  40493058
   Tiffany Lam 99160079 -}

--"WWW-WW-------BB-BBB"
blah = ["---","-WW-","--W--","WWWW","BBB"]
jur = ["---","--B-","--W--","-WW-","---"]
jul = ["---","B---","-W---","-WW-","---"]
test3 = ["WWW","-WW-","-----","-BB-","BBB"]


--BOARD EVALUATION

getBoardScore board player
  | isWin board player               = 10
  | isWin board (getOpponent player) = -10
  | otherwise                        = 0  --placeholder. should call another fn to calc score

--determine if a player has won (opponent has lost n pieces OR opponent can't move)
isWin :: [String] -> Char -> Bool
isWin board player
  | (numOppPieces <= (numStartPieces n) - n) || (null oppMoves) = True
  | otherwise                                                   = False
  where n = length (head board);
        numOppPieces = length (getPlayerPos board (getOpponent player));
        oppMoves = generateAllMoves board (getOpponent player)


--MOVE GENERATION

generateAllMoves :: [String] -> Char -> [[String]]
generateAllMoves board player = filterOut board (generateAllMoves' board (getPlayerPos board player) player)

generateAllMoves' :: [String] -> [(Int, Int)] -> Char -> [[String]]
generateAllMoves' board positions player
  | null positions = []
  | otherwise      = generatePieceMoves board (head positions) player ++ generateAllMoves' board (tail positions) player

generatePieceMoves :: [String] -> (Int, Int) -> Char -> [[String]]
generatePieceMoves board pos player = concat [[slideHorizontalRight board x y player], [slideHorizontalLeft board x y player], 
                                              [slideUpRight board x y player], [slideUpLeft board x y player], 
                                              [slideDownRight board x y player], [slideDownLeft board x y player], 
                                              [jumpHorizontalRight board x y player], [jumpHorizontalLeft board x y player], 
                                              [jumpUpRight board x y player], [jumpUpLeft board x y player], 
                                              [jumpDownRight board x y player], [jumpDownLeft board x y player]]
                                      where x = (fst pos);
                                            y = (snd pos)

--possible moves
slideHorizontalRight :: [String] -> Int -> Int -> Char -> [String]
slideHorizontalRight board x y player
  | getElem board x (y + 1) == '-' = generateNewBoard (generateNewBoard board x y '-') x (y + 1) player
  | otherwise                      = board

slideHorizontalLeft :: [String] -> Int -> Int -> Char -> [String]
slideHorizontalLeft board x y player
  | getElem board x (y - 1) == '-' = generateNewBoard (generateNewBoard board x y '-') x (y - 1) player
  | otherwise                      = board

jumpHorizontalRight:: [String] -> Int -> Int -> Char -> [String]
jumpHorizontalRight board x y player
  | getElem board x (y + 1) == player && (getElem board x (y + 2) == '-' || getElem board x (y + 2) == getOpponent player) = generateNewBoard (generateNewBoard board x y '-') x (y + 2) player
  | otherwise = board

jumpHorizontalLeft :: [String] -> Int -> Int -> Char -> [String]
jumpHorizontalLeft board x y player
  | getElem board x (y - 1) == player && (getElem board x (y - 2) == '-' || getElem board x (y - 2) == getOpponent player) = generateNewBoard (generateNewBoard board x y '-') x (y - 2) player
  | otherwise = board

slideUpRight :: [String] -> Int -> Int -> Char -> [String]
slideUpRight board x y player
  | x <= (n - 1) && getElem board (x - 1) y == '-'      = generateNewBoard (generateNewBoard board x y '-') (x - 1) y player
  | x > (n - 1) && getElem board (x - 1) (y + 1) == '-' = generateNewBoard (generateNewBoard board x y '-') (x - 1) (y + 1) player
  | otherwise                                           = board
  where n = length (head board)

slideUpLeft :: [String] -> Int -> Int -> Char -> [String]
slideUpLeft board x y player
  | x <= (n - 1) && getElem board (x - 1) (y - 1) == '-' = generateNewBoard (generateNewBoard board x y '-') (x - 1) (y - 1) player
  | x > (n - 1) && getElem board (x - 1) y == '-'        = generateNewBoard (generateNewBoard board x y '-') (x - 1) y player
  | otherwise                                            = board
  where n = length (head board)

slideDownRight :: [String] -> Int -> Int -> Char -> [String]
slideDownRight board x y player
  | x < (n - 1) && getElem board (x + 1) (y + 1) == '-' = generateNewBoard (generateNewBoard board x y '-') (x + 1) (y + 1) player
  | x >= (n - 1) && getElem board (x + 1) y == '-'      = generateNewBoard (generateNewBoard board x y '-') (x + 1) y player
  | otherwise                                           = board
  where n = length (head board)

slideDownLeft :: [String] -> Int -> Int -> Char -> [String]
slideDownLeft board x y player
  | x < (n - 1) && getElem board (x + 1) y == '-'        = generateNewBoard (generateNewBoard board x y '-') (x + 1) y player
  | x >= (n - 1) && getElem board (x + 1) (y - 1) == '-' = generateNewBoard (generateNewBoard board x y '-') (x + 1) (y - 1) player
  | otherwise                                            = board
  where n = length (head board)

jumpUpRight :: [String] -> Int -> Int -> Char -> [String]
jumpUpRight board x y player
  | x <= (n - 1) && getElem board (x - 1) y == player && (getElem board (x - 2) y == '-' || getElem board (x - 2) y == getOpponent player)             = generateNewBoard (generateNewBoard board x y '-') (x - 2) y player
  | x == n && getElem board (x - 1) (y + 1) == player && (getElem board (x - 2) (y + 1) == '-' || getElem board (x - 2) (y + 1) == getOpponent player) = generateNewBoard (generateNewBoard board x y '-') (x - 2) (y + 1) player
  | x > n && getElem board (x - 1) (y + 1) == player && (getElem board (x - 2) (y + 2) == '-' || getElem board (x - 2) (y + 2) == getOpponent player)  = generateNewBoard (generateNewBoard board x y '-') (x - 2) (y + 2) player
  | otherwise = board
  where n = length (head board)

jumpUpLeft :: [String] -> Int -> Int -> Char -> [String]
jumpUpLeft board x y player
  | x <= (n - 1) && getElem board (x - 1) (y - 1) == player && (getElem board (x - 2) (y - 2) == '-' || getElem board (x - 2) (y - 2) == getOpponent player) = generateNewBoard (generateNewBoard board x y '-') (x - 2) (y - 2) player
  | x == n && getElem board (x - 1) y == player && (getElem board (x - 2) (y - 1) == '-' || getElem board (x - 2) (y - 1) == getOpponent player)             = generateNewBoard (generateNewBoard board x y '-') (x - 2) (y - 1) player
  | x > n && getElem board (x - 1) y == player && (getElem board (x - 2) y == '-' || getElem board (x - 2) y == getOpponent player)                          = generateNewBoard (generateNewBoard board x y '-') (x - 2) y player
  | otherwise = board
  where n = length (head board)

jumpDownRight :: [String] -> Int -> Int -> Char -> [String]
jumpDownRight board x y player
  | x < (n - 2) && getElem board (x + 1) (y + 1) == player && (getElem board (x + 2) (y + 2) == '-' || getElem board (x + 2) (y + 2) == getOpponent player)  = generateNewBoard (generateNewBoard board x y '-') (x + 2) (y + 2) player
  | x == (n - 2) && getElem board (x + 1) (y + 1) == player && (getElem board (x + 2) (y + 1) == '-' || getElem board (x + 2) (y + 1) == getOpponent player) = generateNewBoard (generateNewBoard board x y '-') (x + 2) (y + 1) player
  | x >= (n - 1) && getElem board (x + 1) y == player && (getElem board (x + 2) y == '-' || getElem board (x + 2) y == getOpponent player)                   = generateNewBoard (generateNewBoard board x y '-') (x + 2) y player
  | otherwise = board
  where n = length (head board)

jumpDownLeft :: [String] -> Int -> Int -> Char -> [String]
jumpDownLeft board x y player
  | x < (n - 2) && getElem board (x + 1) y == player && (getElem board (x + 2) y == '-' || getElem board (x + 2) y == getOpponent player)                    = generateNewBoard (generateNewBoard board x y '-') (x + 2) y player
  | x == (n - 2) && getElem board (x + 1) y == player && (getElem board (x + 2) (y - 1) == '-' || getElem board (x + 2) (y - 1) == getOpponent player)       = generateNewBoard (generateNewBoard board x y '-') (x + 2) (y - 1) player
  | x >= (n - 1) && getElem board (x + 1) (y - 1) == player && (getElem board (x + 2) (y - 2) == '-' || getElem board (x + 2) (y - 2) == getOpponent player) = generateNewBoard (generateNewBoard board x y '-') (x + 2) (y - 2) player
  | otherwise = board
  where n = length (head board)

--generate a board with the new move
generateNewBoard :: [String] -> Int -> Int -> Char -> [String]
generateNewBoard board x y elem
  | x == 0	   = (generateNewRow (head board) y elem):(tail board)
  | otherwise  = (head board) : (generateNewBoard (tail board) (x - 1) y elem)

generateNewRow :: String -> Int -> Char -> String
generateNewRow x y elem
  | y == 0	  = elem : (tail x)
  | otherwise = (head x) : generateNewRow (tail x) (y - 1) elem


--MINIMAX

--do stuff here


--HELPERS

--convert original board representation to a more readable representation
--e.g., "WWW-WW-------BB-BBB" -> ["---","-WW-","--W--","WWWW","BBB"]
convertToNewRep :: String -> Int -> Int -> [String]
convertToNewRep board n n'
  | n' >= (n * 2 - 1) = (fst split) : convertToNewRep' (snd split) (n' - 1)
  | otherwise         = (fst split) : convertToNewRep (snd split) n (n' + 1)
  where split = splitAt n' board

convertToNewRep' :: String -> Int -> [String]
convertToNewRep' board n
  | null board = []
  | otherwise  = (fst split) : convertToNewRep' (snd split) (n - 1)
  where split = splitAt n board

--convert our board representation to the original representation
--e.g., ["---","-WW-","--W--","WWWW","BBB"] -> "WWW-WW-------BB-BBB"
convertToOldRep :: [String] -> String
convertToOldRep board = concat board

--get the opponent's color
getOpponent :: Char -> Char
getOpponent player
  | player == 'W' = 'B'
  | otherwise     = 'W'

--calculate the number of starting pieces for a player
numStartPieces :: Int -> Int
numStartPieces n = (2 * n) - 1

--get all board positions (x,y) for a given player
getPlayerPos :: [String] -> Char -> [(Int, Int)]
getPlayerPos board player = getPlayerPos' board player 0

getPlayerPos' :: [String] -> Char -> Int -> [(Int, Int)]
getPlayerPos' board player rowNum
  | null board  = []
  | otherwise = getPlayerPosInRow (head board) player rowNum ++ getPlayerPos' (tail board) player (rowNum + 1)

getPlayerPosInRow :: String -> Char -> Int -> [(Int, Int)]
getPlayerPosInRow row player rowNum = getPlayerPosInRow' row player rowNum 0

getPlayerPosInRow' :: String -> Char -> Int -> Int -> [(Int, Int)]
getPlayerPosInRow' row player rowNum colNum
  | null row             = []
  | (head row) == player = (rowNum, colNum) : getPlayerPosInRow' (tail row) player rowNum (colNum + 1)
  | otherwise            = getPlayerPosInRow' (tail row) player rowNum (colNum + 1)

--get the elem at position (x,y) on the board; return '?' if (x,y) is out of bounds
getElem :: [String] -> Int -> Int -> Char
getElem board x y
  | x < 0                                                      = '?'
  | x >= numRows                                               = '?'
  | y < 0                                                      = '?'
  | x <= (n - 1) && y >= x + n                                 = '?'
  | x > (n - 1) && y > abs (x - middleRow - (numRows - n - 1)) = '?'
  | otherwise                                                  = ((board !! (x)) !! y)
  where numRows = length board;
        n = length (head board);
        middleRow = n * 2 - 1

filterOut elem list
  | null list           = []
  | (head list) == elem = filterOut elem (tail list)
  | otherwise           = (head list) : filterOut elem (tail list)
