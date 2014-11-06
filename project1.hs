{- CPSC 312, Project 1
   Allan Fong  40493058
   Tiffany Lam 99160079 -}

--"WWW-WW-------BB-BBB"
--crusher ex 'B' 1 3
ex = ["WW--W----B--B-B----"]
ex2 = ["BBB-W----W------W-B","BBB-W----W--W-----B"]
ex3 = ["WW-AAAAAAWBAAAAABB-"]
--getBoardTuples (generateAllNewMoves ex3b [ex3b] 'W') [ex3b] 'W' 4 []
ex3b = ["WW-","AAAA","AAWBA","AAAA","BB-"]
ex3c = ["WW-","AAAA","A-WBA","AAAA","BB-"]
ex4 = ["----WW---W--WWWWBBB","----WW---W--WWWWBBB"]
blah = ["---","-WW-","--W--","WWW-","BBB"]
test3 = ["WWW","-WW-","-----","-BB-","BBB"]
test5 = ["-----aaaaaa-------aaaaaaaa---------bbbbbbbb-------bbbbb-----","-----aaaaaa-------aaaaaaaa---------bbbbbbbb-------bbbbb-----"]


--TOP-LEVEL FUNCTION

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher boards player depth n = crusher' (head newRepBoards) newRepBoards player depth
                                where newRepBoards = convertBoardsToNewRep boards n

crusher' :: [String] -> [[String]] -> Char -> Int -> [String]
crusher' board history player depth
  | isWin board history (getOpponent player) = map convertToOldRep (board : history)
  | otherwise                                = map convertToOldRep (nextMove : history)
  where nextMove = pickNextMove (generateAllNewMoves board history player) history player depth


--BOARD EVALUATION

--get the "goodness" of a board for a given player
getBoardScore :: [String] -> [[String]] -> Char -> Int
getBoardScore board history player
  | isWin board history player               = 100
  | isWin board history (getOpponent player) = -100
  | otherwise                                = calcBoardScore board player

--determine if a player has won (opponent has lost n pieces OR opponent can't move)
isWin :: [String] -> [[String]] -> Char -> Bool
isWin board history player
  | (numOppPieces <= (numStartPieces n) - n) = True
  | (null oppMoves)                          = True
  | otherwise                                = False
  where n = length (head board);
        numOppPieces = length (getPlayerPos board (getOpponent player));
        oppMoves = generateAllNewMoves board history (getOpponent player)

--calculate the "goodness" of a board for a given player if there is no winner yet
calcBoardScore :: [String] -> Char -> Int
calcBoardScore board player = numPieces player - (numPieces (getOpponent player))
  where numPieces p = length (getPlayerPos board p)
	
		
--MOVE GENERATION

generateAllNewMoves :: [String] -> [[String]] -> Char -> [[String]]
generateAllNewMoves board history player = generateAllNewMoves' (generateAllMoves board player) history player

generateAllNewMoves' :: [[String]] -> [[String]] -> Char -> [[String]]
generateAllNewMoves' boards history player
  | null boards                           = []
  | isPreviousBoard (head boards) history = generateAllNewMoves' (tail boards) history player
  | otherwise                             = (head boards) : generateAllNewMoves' (tail boards) history player

generateAllMoves :: [String] -> Char -> [[String]]
generateAllMoves board player = filter (/=board) (generateAllMoves' board (getPlayerPos board player) player)

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

--get the board with the max propagated score
pickNextMove :: [[String]] -> [[String]] -> Char -> Int -> [String]
pickNextMove boards history player depth
  | null (tail boards) = (head boards)
  | otherwise          = getMax (getBoardTuples boards history player depth [])
	
--get (board,score) tuples generated by the top max level
getBoardTuples :: [[String]] -> [[String]] -> Char -> Int -> [([String], Int)] -> [([String], Int)]
getBoardTuples boards history player depth tuples
  | null boards = tuples
  | otherwise   = (getBoardTuples (tail boards) history player depth ((getBoardTuple (head boards) history player depth) : tuples))

getBoardTuple :: [String] -> [[String]] -> Char -> Int -> ([String], Int)
getBoardTuple board history player depth = (board, (minimax board (board : history) player (depth - 1) False))

--minimax algorithm; get the board score at terminal leaves (depth=0 or game is over)
minimax :: [String] -> [[String]] -> Char -> Int -> Bool -> Int
minimax board history player depth isMax
  | depth == 0                                        = getBoardScore board history player
  | isMax && isWin board history (getOpponent player) = getBoardScore board history player
  | not isMax && isWin board history player           = getBoardScore board history player
  | isMax     = maximum (minimax' (generateAllNewMoves board history player) history player (depth - 1) False)
  | otherwise = minimum (minimax' (generateAllNewMoves board history (getOpponent player)) history player (depth - 1) True)

minimax' :: [[String]] -> [[String]] -> Char -> Int -> Bool -> [Int]
minimax' boards history player depth isMax
  | null boards = []
--  | isPreviousBoard (head boards) history = minimax' (tail boards) history player depth isMax
  | otherwise   = (minimax (head boards) ((head boards) : history) player depth isMax) : (minimax' (tail boards) history player depth isMax)

--get the board that has the highest score from a list of (board,score) tuples
getMax :: [([String],Int)] -> [String]
getMax tuples
  | null (tail tuples)                           = fst (head tuples)
  | snd (head tuples) < snd (head (tail tuples)) = getMax (tail tuples)
  | otherwise                                    = getMax ((head tuples) : (tail (tail tuples)))


--HELPERS

convertBoardsToNewRep :: [String] -> Int -> [[String]]
convertBoardsToNewRep boards n
  | null boards = []
  | otherwise = (convertBoardToNewRep (head boards) n n) : (convertBoardsToNewRep (tail boards) n)

--convert original board representation to a more readable representation
--e.g., "WWW-WW-------BB-BBB" -> ["---","-WW-","--W--","WWWW","BBB"]
convertBoardToNewRep :: String -> Int -> Int -> [String]
convertBoardToNewRep board n n'
  | n' >= (n * 2 - 1) = (fst split) : convertBoardToNewRep' (snd split) (n' - 1)
  | otherwise         = (fst split) : convertBoardToNewRep (snd split) n (n' + 1)
  where split = splitAt n' board

convertBoardToNewRep' :: String -> Int -> [String]
convertBoardToNewRep' board n
  | null board = []
  | otherwise  = (fst split) : convertBoardToNewRep' (snd split) (n - 1)
  where split = splitAt n board

--convert our board representation to the original representation
--e.g., ["---","-WW-","--W--","WWWW","BBB"] -> "WWW-WW-------BB-BBB"
convertToOldRep :: [String] -> String
convertToOldRep board = concat board

--determine if the board has been seen before
isPreviousBoard :: [String] -> [[String]] -> Bool
isPreviousBoard board history
  | null history          = False
  | board == head history = True
  | otherwise             = isPreviousBoard board (tail history)
	
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
