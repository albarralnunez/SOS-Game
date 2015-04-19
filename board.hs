module Board where

import Data.List

{------------------------------------------------------------------------------
    SOS Board type and logic
    
    The board looks like this:
    
    +---+---+---+   Some squares already played on
    | 1 | 2 | 3 |   the empty squares are numbered
    +---+---+---+
    | 4 | 5 |SSS|		The board is not displayed well
    +---+---+---+		with two-digit numbers (Need improvement)
    | 7 |OOO| 9 |
    +---+---+---+
------------------------------------------------------------------------------}
data Token = S | O deriving (Eq,Show)
type Square = Either Int Token 
type Board = [[Square]]
type Score = (Int,Int)
type Position = (Int,Int)
data GameState = Game { board :: Board, score :: Score, activePlayer :: Playing}
data Playing = Player1 | Player2 deriving (Eq)

createBoard :: Int  -> Int -> Board
createBoard row col = map (map Left) $ createBoard' [x | x <- [1..(row*col)]]
	where
		createBoard' :: [Int] -> [[Int]]
		createBoard' [] = []
		createBoard' l = col : createBoard' others  
			where (col, others) = splitAt row l

initialGameState :: Int -> Int -> Playing -> GameState
initialGameState row col player = Game board (0,0) player
	where board = createBoard row col

-- List the possible moves to play
possibleMoves :: Board -> [Int]
possibleMoves board = [k | Left k <- concat board]

intToPos :: Int -> Int-> Position
intToPos pos rows = ((pos-1) `mod` rows, (pos-1) `div` rows)

-- Play a token at a square
makeMove :: Int -> Token -> GameState -> Maybe GameState
makeMove k token gameState@(Game board score player)
	| not (k `elem` possibleMoves board) = Nothing   -- illegal move
	| otherwise = Just nextGameState
	where
		score' = scoreP gameState k token 
		nextGameState = if hasScored score score' then
											Game (map (map replace) board) score' player
										else Game (map (map replace) board) score' (switch player)
		replace (Left k') | k' == k = Right token
		replace x                   = x
		switch Player1 = Player2
		switch Player2 = Player1    

-- The game has ended?
ended :: GameState -> Bool
ended (Game board _ _) = all hasToken $ concat board

-- The square has a token?
hasToken :: Square -> Bool
hasToken (Right _) = True 
hasToken y = False 

-- Somebody scored this tourn
hasScored :: Score -> Score -> Bool
hasScored (x,y) (x',y') = x < x' || y < y'

-- Adds the points done in a turn
scoreP :: GameState -> Int -> Token ->  (Int,Int)
scoreP (Game board (player1,player2) Player1) move token = (player1+sc,player2)
	where sc = movePoints board move token
scoreP (Game board (player1,player2) Player2) move token = (player1,player2+sc)
	where sc = movePoints board move token

-- Number of points scored in a movement
movePoints :: Board -> Int -> Token -> Int
movePoints board pos token = points
	where
		points = sum $ map checkSos allCandidates
		allCandidates = 
			map (map (getToken' board token (x,y))) $
				filter (all (allowedPos m n)) positions
		positions = genCandidates token (x,y)
		n = length board
		m = length $ board !! 0
		(x,y) = intToPos pos m

-- Positions of interest near a position when token is placed
genCandidates :: Token -> Position -> [[Position]]
genCandidates S pos = genScandidates pos
genCandidates O pos = genOcandidates pos
	
genOcandidates :: Position -> [[Position]]
genOcandidates pos@(x, y) = comb board
	where
		comb [[a1,b1,c1],
					[a2,b2,c2],		--diagonal1 diagonal2  row        column
				  [a3,b3,c3]] = [[a1,b2,c3],[a3,b2,c1],[a2,b2,c2],[b1,b2,b3]]
		board = [[((x-1), (y-1)), (x, (y-1)), ((x+1), (y-1))], 
						 [((x-1), y    ), (x,  y   ), ((x+1), y    )], 
						 [((x-1), (y+1)), (x, (y+1)), ((x+1), (y+1))]]

genScandidates :: Position -> [[Position]]
genScandidates pos@(x, y) = [upR,up,upL,left,downL,down,downR,right]
	where
		upR   = [((x+2), (y-2)), ((x+1), (y-1)), pos]
		up    = [( x   , (y-2)), ( x   , (y-1)), pos]
		upL   = [((x-2), (y-2)), ((x-1), (y-1)), pos]
		left  = [((x-2),  y   ), ((x-1),  y   ), pos]
		downL = [((x-2), (y+2)), ((x-1), (y+1)), pos]
		down  = [( x   , (y+2)), ( x   , (y+1)), pos]
		downR = [((x+2), (y+2)), ((x+1), (y+1)), pos]
		right = [((x+2),  y   ), ((x+1),  y   ), pos]

-- Positions inside the board
allowedPos :: Int -> Int -> Position -> Bool
allowedPos n m (x, y)
	| 0 <= x && x < n && 0 <= y && y < m 	= True
	| otherwise														= False

-- Get token before the motion is doned . 
-- If the position at which to perform the movement is requested, 
-- the result is the record passed by parameter
getToken' :: Board -> Token -> Position -> Position -> Square
getToken' board token gpos apos@(x,y)
	| apos == gpos = (Right token)
	| otherwise   = board !! y !! x


checkSos :: [Square] -> Int
checkSos [(Right a),(Right b),(Right c)]
	| S == a && O == b &&  S == c = 1
	| otherwise    				   			= 0
checkSos x 											= 0	

-- Print and read board and tokens
showSquare :: Either Int Token -> String
showSquare = either (\n -> " " ++ show n ++ " ") 
										(concat . replicate 3 . show)

showBoard :: Board -> String
showBoard board = 
		unlines . surround (concat (replicate cols "+---"))
    . map (concat . surround "|". map showSquare) $ board
    where
    	digits n = length $ map (\x -> read[x]::Int) n
    	spliter y = (concat . replicate (2-(digits y)) $ " ") ++ "|"
    	cols = length $ board !! 0
    	surround x xs = [x] ++ intersperse x xs ++ [x]

showScore :: Score -> String
showScore (p1,p2) = "Player 1 -> " ++ show(p1) ++ 
										"\nPlayer 2 -> " ++ show(p2) ++ "\n"

readToken :: String -> Token
readToken "O" = O
readToken "o" = O
readToken "S" = S
readToken "s" = S

instance Show Playing where
    show (Player1) = "Player One Turn:"
    show (Player2) = "Player Two Turn:"

showMove :: (Int,Token) -> String
showMove (pos, token) = "* Place " ++ show token ++ " at " ++ show pos ++ "\n"

printActivePlayer :: Show a => a -> IO ()
printActivePlayer = putStrLn . show

printBoard = putStr . showBoard
printScore = putStr . showScore
printMove = putStr . showMove

-- Test Boards
board0 :: [[Square]]
board0 = [[(Left 1), (Left 2), (Left 3)],
					[(Left 4), (Left 5), (Left 6)],
					[(Left 7), (Left 8), (Left 9)]]
board1 :: [[Square]]
board1 = [[(Right S),	(Left 2),	(Right S)	],
					[(Left 4),	(Left 5),	(Left 6)	],
					[(Left 7),	(Left 8),	(Left 9)	]]
board2 :: [[Square]]
board2 = [[(Right S),	(Left 2),	(Right S)	],
					[(Left 4),	(Left 5),	(Left 6)	],
					[(Right S),	(Left 8),	(Right S)	]]