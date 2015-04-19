{-# LANGUAGE GADTs #-}
-- UndecidableInstances, MultiParamTypeClasses, FlexibleInstances
-- module SOS where

import Board
import Control.Monad
import Control.Monad.Operational
import Control.Monad.State

import Data.Either

import Data.List

import System.Random

{------------------------------FAST SIMULATIONS---------------------------------
	 	-Add * at the end of the name of IA to print the board every tourn
	 	  * Human
	 	  * Stupid (random behavior)
	 	  * Medium (5 years old kid behavior, if can't score play randomly
	 	  * Hard
-------------------------------------------------------------------------------}
hardVsmedium = do runGame (initialGameState 6 6 Player2) 
										 (chosePlayer "Challenger*") (chosePlayer "Medium")  

{------------------------------------------------------------------------------
    The Player monad for implementing players (human, AI, ...)
    provides two operations
    
        readBoard   -- read the current board state and score
        playMove    -- play a move
    
    Moreover, it's actually a monad transformer intended to be used over IO.
    This way, the players can perform IO computations.
------------------------------------------------------------------------------}
data PlayerI a where
    ReadBoard :: PlayerI (GameState)
    PlayMove  :: (Int, Token) -> PlayerI Bool
    
type Player m a = ProgramT PlayerI m a

readBoard = singleton ReadBoard
playMove  = singleton . PlayMove

    -- interpreter
runGame :: GameState -> Player IO () -> Player IO () -> IO ()
runGame init player1 player2 = eval' init player1 player2 
	where
		eval' game p1 p2 = viewT p1 >>= \p1view -> eval game p1view p2

		eval :: GameState -> ProgramViewT PlayerI IO () -> Player IO ()	-> IO ()
		eval game (Return _)            _  = return ()
		eval game (ReadBoard :>>= p1) p2 = 
			eval' game (p1 game) p2
		eval game (PlayMove (mv, token) :>>= p1) p2 =
			case makeMove mv token game of
				Nothing         -> eval' game (p1 False) p2
				Just game'
					| ended game'	-> let (pl1,pl2) = score game' in 
													 putStrLn $ "\n" ++
													 (if pl1 > pl2 then "Player 1 has won!" 
													 else if pl1 < pl2 then "Player 2 has won!"
													 else "Draw!") ++ "\n"
													 ++ "Player 1: " ++ (show pl1) 
													 ++ "\n" ++	"Player 2: " ++ (show pl2)
					| hasScored (score game) (score game') ->  eval' game' (p1 True) p2
					| otherwise	-> eval' game' p2 (p1 True)


main = do
  liftIO . putStrLn $ 
	 	 "\nPlayer One: Choose one player\n\
	 	\  * Human\n\
	 	\  * Stupid (random behavior)\n\
	 	\  * Medium (5 years old kid behavior, if can't score play randomly\n\
	 	\  * Hard\n\
	 	\  * Challenger (beta)\n\
	 	\-Add * at the end of the name of IA to print the board every tourn\n"
  player1 <- liftIO getLine
  liftIO . putStrLn $ 
	 	 "\nPlayer Two: Choose one player\n\
	 	\  * Human\n\
	 	\  * Stupid (random behavior)\n\
	 	\  * Medium (5 years old kid behavior, if can't score play randomly\n\
	 	\  * Hard\n\
	 	\  * Challenger (beta)\n\
	 	\-Add * at the end of the name of IA to print the board every tourn\n"
  player2 <- liftIO getLine
  liftIO . putStrLn $ 
  	"\nWho start? (Type p1 for Player One or p2 for Player Two)"
  n <- liftIO getLine
  liftIO . putStrLn $ "\nNumber of rows of the board?"
  y <- liftIO getLine
  liftIO . putStrLn $ "Number of columns of the board?"
  x <- liftIO getLine
  if n == "p1" then 
   	runGame (initialGameState (read x) (read y) Player1) 
   		(chosePlayer player1) (chosePlayer player2)
  else 
   	runGame (initialGameState (read x) (read y) Player2)
   		(chosePlayer player2) (chosePlayer player1)

chosePlayer :: String -> Player IO ()
chosePlayer "Human"  = do humanPlayer
chosePlayer "Stupid" = do lift getStdGen  >>= randomPlayer
chosePlayer "Medium" = do lift getStdGen  >>= mediumPlayer
chosePlayer "Hard" = do lift getStdGen >>= hardPlayer
chosePlayer "Challenger" = do lift getStdGen >>= challengerPlayer
chosePlayer "Stupid*" = do lift getStdGen >>= randomPlayer'
chosePlayer "Medium*" = do lift getStdGen >>= mediumPlayer'
chosePlayer "Hard*" = do lift getStdGen >>= hardPlayer'
chosePlayer "Challenger*" = do lift getStdGen >>= challengerPlayer'


--------------------------------------------------------------------------------
-- For the IA

-- Select one element at random
uniform :: Monad m => [a] -> StateT StdGen m a
uniform xs = do
	gen <- get
	let (n,gen') = randomR (1,length xs) gen
	put gen'
	return (xs !! (n-1))

-- Pick one of the best possible moves at rand
pickMoveMedium :: Monad m => Board -> StateT StdGen m (Int, Token)
pickMoveMedium board = do 
	p <- uniform . bestMoves $ board
	return $ pickMove' board p

-- Pick one of the best possible moves at rand, trying to harm the enemy
pickMoveHard :: Monad m => Board -> StateT StdGen m (Int, Token)
pickMoveHard board = do 
	p <-	if bestPossibleScore == 0 then
					uniform . trollMoves $ board
				else
					uniform . bestMoves $ board 
	return $ pickMove' board p
	where
		bestPossibleScore = maximum . concat . evalMovementsP $ board

-- Translate the result of the functions who choose 
-- best movements to a position and letter
pickMove' ::  Board -> Int -> (Int, Token)
pickMove' board n =  (allpos !! (n `div` 2) , if n `mod` 2 == 0 then S else O)
	where allpos = possibleMoves board

bestMoves :: Board -> [Int]
bestMoves board = 
	elemIndices  (maximum evaluatedMovementsP) evaluatedMovementsP
	where
		evaluatedMovementsP = concat . evalMovementsP $ board

-- Mat. of punt. per mov. out list pos., in S or O respect.
evalMovementsP :: Board -> [[Int]]
evalMovementsP board = 
	map (\x -> map (movePoints board x) [S,O]) (possibleMoves board)

trollMoves :: Board -> [Int]
trollMoves board = elemIndices (maximum evaluatedMovements) evaluatedMovements
	where
		evaluatedMovements = concat . evalMovements $ board

		evalMovements :: Board -> [[Int]]
		evalMovements board = 
			map (\x -> map (worth board x) [S,O]) (possibleMoves board)

		worth :: Board -> Int -> Token -> Int
		worth board pos S = 
			sum $ map (punctuateS board) p
			where 
				n = length $ board !! 0
				m = length board
				p = map (map (combMay n m)) (genCandidates S (intToPos pos n))
		worth board pos O = 
			sum $ map (punctuateO board) p
			where 
				n = length $ board !! 0
				m = length board
				p = map (map (combMay n m)) (genCandidates O (intToPos pos n))
		
		punctuateS :: Board -> [Maybe Position] -> Int
		punctuateS board ((Just (x,y)):(Just (i,j)):_) 
			| s == (Right S) && (not $ hasToken o) || 
				o == (Right O) && (not $ hasToken s) = 0 
			| otherwise = 1
			where 
				s = board !! y !! x
				o = board !! j !! i
		punctuateS board (Nothing:Nothing:_) = 1
		punctuateS board (Nothing:_:_) = 1
		punctuateS board l = 0
		
		punctuateO :: Board -> [Maybe Position] -> Int
		punctuateO board [(Just (x,y)),_,(Just (i,j))] 
			| a == (Right S) && c == (Right O) || 
				c == (Right S) && a == (Right O) = 2
			| otherwise = 0
			where 
				a = board !! y !! x
				c = board !! j !! i
		punctuateO board [_,_,Nothing] = 2
		punctuateO board [Nothing,_,_] = 2
		punctuateO board l = 0

combMay :: (Ord t1, Ord t, Num t1, Num t) => t -> t1 -> (t, t1) -> Maybe (t, t1)
combMay n m (x,y)
	 | 0 <= x && x < n && 0 <= y && y < m 	= Just (x,y)
	 | otherwise 														= Nothing 


--Challenger IA
pickMoveChallenger :: Monad m => Board -> Int -> StateT StdGen m (Int, Token)
pickMoveChallenger board p = do 
	p <-	if bestPossibleScore == 0 then
					uniform . trollMoves $ board
				else
					uniform $ challengerMove board p
	return $ pickMove' board p
	where
		bestPossibleScore = maximum . concat . evalMovementsP $ board

challengerMove :: Board -> Int -> [Int]
challengerMove board prof = elemIndices (maximum aux) aux  
	where aux = challengerMoveAux board prof

challengerMoveAux :: Board -> Int -> [Int]
challengerMoveAux board prof = challengerMove' 0 evaluatedMovementsP prof board
	where	evaluatedMovementsP = concat . evalMovementsP $ board

challengerMove' :: Int -> [Int] -> Int -> Board -> [Int]
challengerMove' _ _ 0 _ = []
challengerMove' _ [] _ _ = []
challengerMove' j (x:xs) prof board =
	(deep j x prof board) : (challengerMove' (j+1) xs prof board)

deep :: Int -> Int -> Int -> Board -> Int
deep p pun 0 boasrd = pun
deep _ 0 _ _ = 0
deep p pun prof board = pun + (maximumOr0 $ newMov board p) 

newMov :: Board -> Int -> [Int] 
newMov board' x = challengerMoveAux newBoard (x-1)
	where 
		(p,t) = pickMove' board' x
		newBoard = (simMove p t board')

maximumOr0 r 
	| r == [] = 0
	| otherwise = maximum r 

-- Play a token at a square
simMove :: Int -> Token -> Board -> Board
simMove k token board = nextGameState
	where
		nextGameState = map (map replace) board
		replace (Left k') | k' == k = Right token
		replace x                   = x
		switch Player1 = Player2
		switch Player2 = Player1

----------------------------------Players---------------------------------------

-- A human player on the command line
humanPlayer :: Player IO ()
humanPlayer = forever $ do
	--(board,score) <- readBoard 
	(Game board score activePlayer) <- readBoard
	liftIO . putStrLn $ "\n"
	liftIO . printActivePlayer $ activePlayer
	liftIO . printBoard $ board
	liftIO . printScore $ score
	doMove
	where
		-- ask the player where to move
		doMove :: Player IO ()
		doMove = do
			liftIO . putStrLn $ "\nAt which number would you like to play?"
			n <- liftIO getLine
			liftIO . putStrLn $ "What letter would you like to play? (S/O)"
			j <- liftIO getLine
			b <- playMove (read n, readToken j)
			unless (b) $ do
				liftIO . putStrLn $ "Position " ++ show n ++ " is already full."
				doMove

randomPlayer :: StdGen -> Player IO ()
randomPlayer = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board _ activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			n     <- uniform (possibleMoves board) -- select a random move
			m 		<- uniform [S,O] -- select a random token
			liftIO . printMove $ (n, m)
			lift $ playMove (n, m)

randomPlayer' :: StdGen -> Player IO ()
randomPlayer' = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board score activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			liftIO . printBoard $ board
			liftIO . printScore $ score
			n     <- uniform (possibleMoves board) -- select a random move
			m 		<- uniform [S,O] -- select a random token
			liftIO . printMove $ (n, m)
			lift $ playMove (n, m)

mediumPlayer :: StdGen -> Player IO ()
mediumPlayer = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board _ activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			(pos,token)   <- pickMoveMedium board
			liftIO . printMove $ (pos, token)
			lift $ playMove (pos, token)

mediumPlayer' :: StdGen -> Player IO ()
mediumPlayer' = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board score activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			liftIO . printBoard $ board
			liftIO . printScore $ score
			(pos,token)   <- pickMoveMedium board
			liftIO . printMove $ (pos, token)
			lift $ playMove (pos, token)

hardPlayer :: StdGen -> Player IO ()
hardPlayer = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board _ activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			(pos,token)   <- pickMoveHard board
			liftIO . printMove $ (pos, token)
			lift $ playMove (pos, token)

hardPlayer' :: StdGen -> Player IO ()
hardPlayer' = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board score activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			liftIO . printBoard $ board
			liftIO . printScore $ score
			(pos,token)   <- pickMoveHard board
			liftIO . printMove $ (pos, token)
			lift $ playMove (pos, token)

challengerPlayer :: StdGen -> Player IO ()
challengerPlayer = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board score activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			(pos,token)   <- pickMoveChallenger board 5
			liftIO . printMove $ (pos, token)
			lift $ playMove (pos, token)

challengerPlayer' :: StdGen -> Player IO ()
challengerPlayer' = evalStateT ai
	where
		ai :: StateT StdGen (ProgramT PlayerI IO) ()
		ai = forever $ do
			liftIO . putStrLn $ "\n"
			(Game board score activePlayer) <- lift $ readBoard
			liftIO . printActivePlayer $ activePlayer
			liftIO . printBoard $ board
			liftIO . printScore $ score
			(pos,token)   <- pickMoveChallenger board 5
			liftIO . printMove $ (pos, token)
			lift $ playMove (pos, token)
