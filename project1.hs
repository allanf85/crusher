{- CPSC 312, Project 1
   Allan Fong  40493058
   Tiffany Lam 99160079 -}


--TOP-LEVEL FUNCTION

--precondition: depth > 0
crusher_w5p8 :: [String] -> Char -> Int -> Int -> [String]
crusher_w5p8 boards player depth n = crusher_w5p8' (head newRepBoards) newRepBoards player depth
                                     where newRepBoards = convertBoardsToNewRep_w5p8 boards n

crusher_w5p8' :: [String] -> [[String]] -> Char -> Int -> [String]
crusher_w5p8' board history player depth
  | isWin_w5p8 board history (getOpponent_w5p8 player) = map convertToOldRep_w5p8 (board : history)
  | otherwise                                          = map convertToOldRep_w5p8 (nextMove : history)
  where nextMove = getNextMove_w5p8 (generateAllNewMoves_w5p8 board history player) history player depth


--BOARD EVALUATION

--get the "goodness" of a board for a given player (if we're at a max level)
getBoardScoreMax_w5p8 :: [String] -> [[String]] -> Char -> Int
getBoardScoreMax_w5p8 board history player
  | isWin_w5p8 board history (getOpponent_w5p8 player) = -100
  | otherwise                                          = calcBoardScore_w5p8 board player

--get the "goodness" of a board for a given player (if we're at a min level)
getBoardScoreMin_w5p8 :: [String] -> [[String]] -> Char -> Int
getBoardScoreMin_w5p8 board history player
  | isWin_w5p8 board history player = 100
  | otherwise                       = calcBoardScore_w5p8 board player

--determine if a player has won (opponent has lost n pieces OR opponent can't move)
isWin_w5p8 :: [String] -> [[String]] -> Char -> Bool
isWin_w5p8 board history player
  | (numOppPieces <= (numStartPieces_w5p8 n) - n) = True
  | (null oppMoves)                               = True
  | otherwise                                     = False
  where n = length (head board);
        numOppPieces = length (getPlayerPos_w5p8 board (getOpponent_w5p8 player));
        oppMoves = generateAllNewMoves_w5p8 board history (getOpponent_w5p8 player)

--calculate the "goodness" of a board for a given player if there is no winner yet
calcBoardScore_w5p8 :: [String] -> Char -> Int
calcBoardScore_w5p8 board player = numPieces player - (numPieces (getOpponent_w5p8 player))
  where numPieces p = length (getPlayerPos_w5p8 board p)
	
		
--MOVE GENERATION

--generate all boards that haven't been seen before
generateAllNewMoves_w5p8 :: [String] -> [[String]] -> Char -> [[String]]
generateAllNewMoves_w5p8 board history player = generateAllNewMoves_w5p8' (generateAllMoves_w5p8 board player) history player

generateAllNewMoves_w5p8' :: [[String]] -> [[String]] -> Char -> [[String]]
generateAllNewMoves_w5p8' boards history player
  | null boards                                = []
  | isPreviousBoard_w5p8 (head boards) history = generateAllNewMoves_w5p8' (tail boards) history player
  | otherwise                                  = (head boards) : generateAllNewMoves_w5p8' (tail boards) history player

generateAllMoves_w5p8 :: [String] -> Char -> [[String]]
generateAllMoves_w5p8 board player = filter (/=board) (generateAllMoves_w5p8' board (getPlayerPos_w5p8 board player) player)

generateAllMoves_w5p8' :: [String] -> [(Int, Int)] -> Char -> [[String]]
generateAllMoves_w5p8' board positions player
  | null positions = []
  | otherwise      = generatePieceMoves_w5p8 board (head positions) player ++ generateAllMoves_w5p8' board (tail positions) player

generatePieceMoves_w5p8 :: [String] -> (Int, Int) -> Char -> [[String]]
generatePieceMoves_w5p8 board pos player = concat [[slideHorizontalRight_w5p8 board x y player], [slideHorizontalLeft_w5p8 board x y player], 
                                              [slideUpRight_w5p8 board x y player], [slideUpLeft_w5p8 board x y player], 
                                              [slideDownRight_w5p8 board x y player], [slideDownLeft_w5p8 board x y player], 
                                              [jumpHorizontalRight_w5p8 board x y player], [jumpHorizontalLeft_w5p8 board x y player], 
                                              [jumpUpRight_w5p8 board x y player], [jumpUpLeft_w5p8 board x y player], 
                                              [jumpDownRight_w5p8 board x y player], [jumpDownLeft_w5p8 board x y player]]
                                           where x = (fst pos);
                                                 y = (snd pos)

--possible moves
slideHorizontalRight_w5p8 :: [String] -> Int -> Int -> Char -> [String]
slideHorizontalRight_w5p8 board x y player
  | getElem_w5p8 board x (y + 1) == '-' = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') x (y + 1) player
  | otherwise                           = board

slideHorizontalLeft_w5p8 :: [String] -> Int -> Int -> Char -> [String]
slideHorizontalLeft_w5p8 board x y player
  | getElem_w5p8 board x (y - 1) == '-' = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') x (y - 1) player
  | otherwise                           = board

jumpHorizontalRight_w5p8:: [String] -> Int -> Int -> Char -> [String]
jumpHorizontalRight_w5p8 board x y player
  | getElem_w5p8 board x (y + 1) == player && (getElem_w5p8 board x (y + 2) == '-' || getElem_w5p8 board x (y + 2) == getOpponent_w5p8 player) = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') x (y + 2) player
  | otherwise = board

jumpHorizontalLeft_w5p8 :: [String] -> Int -> Int -> Char -> [String]
jumpHorizontalLeft_w5p8 board x y player
  | getElem_w5p8 board x (y - 1) == player && (getElem_w5p8 board x (y - 2) == '-' || getElem_w5p8 board x (y - 2) == getOpponent_w5p8 player) = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') x (y - 2) player
  | otherwise = board

slideUpRight_w5p8 :: [String] -> Int -> Int -> Char -> [String]
slideUpRight_w5p8 board x y player
  | x <= (n - 1) && getElem_w5p8 board (x - 1) y == '-'      = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 1) y player
  | x > (n - 1) && getElem_w5p8 board (x - 1) (y + 1) == '-' = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 1) (y + 1) player
  | otherwise                                                = board
  where n = length (head board)

slideUpLeft_w5p8 :: [String] -> Int -> Int -> Char -> [String]
slideUpLeft_w5p8 board x y player
  | x <= (n - 1) && getElem_w5p8 board (x - 1) (y - 1) == '-' = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 1) (y - 1) player
  | x > (n - 1) && getElem_w5p8 board (x - 1) y == '-'        = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 1) y player
  | otherwise                                                 = board
  where n = length (head board)

slideDownRight_w5p8 :: [String] -> Int -> Int -> Char -> [String]
slideDownRight_w5p8 board x y player
  | x < (n - 1) && getElem_w5p8 board (x + 1) (y + 1) == '-' = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 1) (y + 1) player
  | x >= (n - 1) && getElem_w5p8 board (x + 1) y == '-'      = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 1) y player
  | otherwise                                                = board
  where n = length (head board)

slideDownLeft_w5p8 :: [String] -> Int -> Int -> Char -> [String]
slideDownLeft_w5p8 board x y player
  | x < (n - 1) && getElem_w5p8 board (x + 1) y == '-'        = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 1) y player
  | x >= (n - 1) && getElem_w5p8 board (x + 1) (y - 1) == '-' = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 1) (y - 1) player
  | otherwise                                                 = board
  where n = length (head board)

jumpUpRight_w5p8 :: [String] -> Int -> Int -> Char -> [String]
jumpUpRight_w5p8 board x y player
  | x <= (n - 1) && getElem_w5p8 board (x - 1) y == player && (getElem_w5p8 board (x - 2) y == '-' || getElem_w5p8 board (x - 2) y == getOpponent_w5p8 player)             = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 2) y player
  | x == n && getElem_w5p8 board (x - 1) (y + 1) == player && (getElem_w5p8 board (x - 2) (y + 1) == '-' || getElem_w5p8 board (x - 2) (y + 1) == getOpponent_w5p8 player) = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 2) (y + 1) player
  | x > n && getElem_w5p8 board (x - 1) (y + 1) == player && (getElem_w5p8 board (x - 2) (y + 2) == '-' || getElem_w5p8 board (x - 2) (y + 2) == getOpponent_w5p8 player)  = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 2) (y + 2) player
  | otherwise = board
  where n = length (head board)

jumpUpLeft_w5p8 :: [String] -> Int -> Int -> Char -> [String]
jumpUpLeft_w5p8 board x y player
  | x <= (n - 1) && getElem_w5p8 board (x - 1) (y - 1) == player && (getElem_w5p8 board (x - 2) (y - 2) == '-' || getElem_w5p8 board (x - 2) (y - 2) == getOpponent_w5p8 player) = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 2) (y - 2) player
  | x == n && getElem_w5p8 board (x - 1) y == player && (getElem_w5p8 board (x - 2) (y - 1) == '-' || getElem_w5p8 board (x - 2) (y - 1) == getOpponent_w5p8 player)             = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 2) (y - 1) player
  | x > n && getElem_w5p8 board (x - 1) y == player && (getElem_w5p8 board (x - 2) y == '-' || getElem_w5p8 board (x - 2) y == getOpponent_w5p8 player)                          = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x - 2) y player
  | otherwise = board
  where n = length (head board)

jumpDownRight_w5p8 :: [String] -> Int -> Int -> Char -> [String]
jumpDownRight_w5p8 board x y player
  | x < (n - 2) && getElem_w5p8 board (x + 1) (y + 1) == player && (getElem_w5p8 board (x + 2) (y + 2) == '-' || getElem_w5p8 board (x + 2) (y + 2) == getOpponent_w5p8 player)  = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 2) (y + 2) player
  | x == (n - 2) && getElem_w5p8 board (x + 1) (y + 1) == player && (getElem_w5p8 board (x + 2) (y + 1) == '-' || getElem_w5p8 board (x + 2) (y + 1) == getOpponent_w5p8 player) = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 2) (y + 1) player
  | x >= (n - 1) && getElem_w5p8 board (x + 1) y == player && (getElem_w5p8 board (x + 2) y == '-' || getElem_w5p8 board (x + 2) y == getOpponent_w5p8 player)                   = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 2) y player
  | otherwise = board
  where n = length (head board)

jumpDownLeft_w5p8 :: [String] -> Int -> Int -> Char -> [String]
jumpDownLeft_w5p8 board x y player
  | x < (n - 2) && getElem_w5p8 board (x + 1) y == player && (getElem_w5p8 board (x + 2) y == '-' || getElem_w5p8 board (x + 2) y == getOpponent_w5p8 player)                    = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 2) y player
  | x == (n - 2) && getElem_w5p8 board (x + 1) y == player && (getElem_w5p8 board (x + 2) (y - 1) == '-' || getElem_w5p8 board (x + 2) (y - 1) == getOpponent_w5p8 player)       = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 2) (y - 1) player
  | x >= (n - 1) && getElem_w5p8 board (x + 1) (y - 1) == player && (getElem_w5p8 board (x + 2) (y - 2) == '-' || getElem_w5p8 board (x + 2) (y - 2) == getOpponent_w5p8 player) = generateNewBoard_w5p8 (generateNewBoard_w5p8 board x y '-') (x + 2) (y - 2) player
  | otherwise = board
  where n = length (head board)

--generate a board with the new move
generateNewBoard_w5p8 :: [String] -> Int -> Int -> Char -> [String]
generateNewBoard_w5p8 board x y elem
  | x == 0	   = (generateNewRow_w5p8 (head board) y elem):(tail board)
  | otherwise  = (head board) : (generateNewBoard_w5p8 (tail board) (x - 1) y elem)

generateNewRow_w5p8 :: String -> Int -> Char -> String
generateNewRow_w5p8 x y elem
  | y == 0	  = elem : (tail x)
  | otherwise = (head x) : generateNewRow_w5p8 (tail x) (y - 1) elem


--MINIMAX

--get the board with the max propagated score
getNextMove_w5p8 :: [[String]] -> [[String]] -> Char -> Int -> [String]
getNextMove_w5p8 boards history player depth
  | null (tail boards) = (head boards)
  | otherwise          = getMax_w5p8 (getBoardTuples_w5p8 boards history player depth [])
	
--get (board,score) tuples generated below the top max level
getBoardTuples_w5p8 :: [[String]] -> [[String]] -> Char -> Int -> [([String], Int)] -> [([String], Int)]
getBoardTuples_w5p8 boards history player depth tuples
  | null boards = tuples
  | otherwise   = (getBoardTuples_w5p8 (tail boards) history player depth ((getBoardTuple_w5p8 (head boards) history player depth) : tuples))

getBoardTuple_w5p8 :: [String] -> [[String]] -> Char -> Int -> ([String], Int)
getBoardTuple_w5p8 board history player depth = (board, (minimax_w5p8 board (board : history) player (depth - 1) False))

--minimax algorithm; get the board score at terminal leaves (depth=0 or other player has won)
minimax_w5p8 :: [String] -> [[String]] -> Char -> Int -> Bool -> Int
minimax_w5p8 board history player depth isMax
  | depth == 0 && isMax                                         = getBoardScoreMax_w5p8 board history player
  | depth == 0 && not isMax                                     = getBoardScoreMin_w5p8 board history player
  | isMax && isWin_w5p8 board history (getOpponent_w5p8 player) = getBoardScoreMax_w5p8 board history player
  | not isMax && isWin_w5p8 board history player                = getBoardScoreMin_w5p8 board history player
  | isMax     = maximum (minimax_w5p8' (generateAllNewMoves_w5p8 board history player) history player (depth - 1) False)
  | otherwise = minimum (minimax_w5p8' (generateAllNewMoves_w5p8 board history (getOpponent_w5p8 player)) history player (depth - 1) True)

minimax_w5p8' :: [[String]] -> [[String]] -> Char -> Int -> Bool -> [Int]
minimax_w5p8' boards history player depth isMax
  | null boards = []
  | otherwise   = (minimax_w5p8 (head boards) ((head boards) : history) player depth isMax) : (minimax_w5p8' (tail boards) history player depth isMax)

--get the board that has the highest score from a list of (board,score) tuples
getMax_w5p8 :: [([String],Int)] -> [String]
getMax_w5p8 tuples
  | null (tail tuples)                           = fst (head tuples)
  | snd (head tuples) < snd (head (tail tuples)) = getMax_w5p8 (tail tuples)
  | otherwise                                    = getMax_w5p8 ((head tuples) : (tail (tail tuples)))


--HELPERS

convertBoardsToNewRep_w5p8 :: [String] -> Int -> [[String]]
convertBoardsToNewRep_w5p8 boards n
  | null boards = []
  | otherwise = (convertBoardToNewRep_w5p8 (head boards) n n) : (convertBoardsToNewRep_w5p8 (tail boards) n)

--convert original board representation to a more readable representation
--e.g., "WWW-WW-------BB-BBB" -> ["---","-WW-","--W--","WWWW","BBB"]
convertBoardToNewRep_w5p8 :: String -> Int -> Int -> [String]
convertBoardToNewRep_w5p8 board n n'
  | n' >= (n * 2 - 1) = (fst split) : convertBoardToNewRep_w5p8' (snd split) (n' - 1)
  | otherwise         = (fst split) : convertBoardToNewRep_w5p8 (snd split) n (n' + 1)
  where split = splitAt n' board

convertBoardToNewRep_w5p8' :: String -> Int -> [String]
convertBoardToNewRep_w5p8' board n
  | null board = []
  | otherwise  = (fst split) : convertBoardToNewRep_w5p8' (snd split) (n - 1)
  where split = splitAt n board

--convert our board representation to the original representation
--e.g., ["---","-WW-","--W--","WWWW","BBB"] -> "WWW-WW-------BB-BBB"
convertToOldRep_w5p8 :: [String] -> String
convertToOldRep_w5p8 board = concat board

--determine if the board has been seen before
isPreviousBoard_w5p8 :: [String] -> [[String]] -> Bool
isPreviousBoard_w5p8 board history
  | null history          = False
  | board == head history = True
  | otherwise             = isPreviousBoard_w5p8 board (tail history)
	
--get the opponent's color
getOpponent_w5p8 :: Char -> Char
getOpponent_w5p8 player
  | player == 'W' = 'B'
  | otherwise     = 'W'

--calculate the number of starting pieces for a player
numStartPieces_w5p8 :: Int -> Int
numStartPieces_w5p8 n = (2 * n) - 1

--get all board positions (x,y) for a given player
getPlayerPos_w5p8 :: [String] -> Char -> [(Int, Int)]
getPlayerPos_w5p8 board player = getPlayerPos_w5p8' board player 0

getPlayerPos_w5p8' :: [String] -> Char -> Int -> [(Int, Int)]
getPlayerPos_w5p8' board player rowNum
  | null board = []
  | otherwise  = getPlayerPosInRow_w5p8 (head board) player rowNum ++ getPlayerPos_w5p8' (tail board) player (rowNum + 1)

getPlayerPosInRow_w5p8 :: String -> Char -> Int -> [(Int, Int)]
getPlayerPosInRow_w5p8 row player rowNum = getPlayerPosInRow_w5p8' row player rowNum 0

getPlayerPosInRow_w5p8' :: String -> Char -> Int -> Int -> [(Int, Int)]
getPlayerPosInRow_w5p8' row player rowNum colNum
  | null row             = []
  | (head row) == player = (rowNum, colNum) : getPlayerPosInRow_w5p8' (tail row) player rowNum (colNum + 1)
  | otherwise            = getPlayerPosInRow_w5p8' (tail row) player rowNum (colNum + 1)

--get the elem at position (x,y) on the board; return '?' if (x,y) is out of bounds
getElem_w5p8 :: [String] -> Int -> Int -> Char
getElem_w5p8 board x y
  | x < 0                                                      = '?'
  | x >= numRows                                               = '?'
  | y < 0                                                      = '?'
  | x <= (n - 1) && y >= x + n                                 = '?'
  | x > (n - 1) && y > abs (x - middleRow - (numRows - n - 1)) = '?'
  | otherwise                                                  = ((board !! (x)) !! y)
  where numRows = length board;
        n = length (head board);
        middleRow = n * 2 - 1
