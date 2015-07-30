import System.IO
import qualified Data.List as L
import Data.String
import Control.Applicative
import Control.Concurrent
import Network.Socket
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC


----------- DATA TYPES ------------

data Card = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
	deriving (Show, Eq, Ord, Read)

type Bill = [Card]

data Move = M Int Card | C
	deriving (Show, Eq, Ord)

data Game = G {bill :: Bill, opp :: String, moves :: [Move]}
	deriving (Show)


----------- FUNCTIONS ------------

count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (x:xs) y
	| x == y = 1 + count xs y
	| otherwise = count xs y

allMoves :: [Move]
allMoves = [M n c | n <- [1..8], c <- [C0, C1, C2, C3, C4, C5, C6, C7, C8, C9]]

format :: Move -> String
format C = "c"
format (M n c) = show n ++ (' ':(tail $ show c))

rightExtr :: Either a b -> b
rightExtr (Right b) = b

addMove :: Game -> Move -> Game
addMove (G b o ms) m = G b o $ m:ms

legalMoves :: Game -> [Move]
legalMoves g
	| moves g == [] = allMoves
	| otherwise = filter (\x -> x > last (moves g)) allMoves

pMovePre :: Bill -> Move -> (Double, Move) -- Computes proba of a hand existing given a bill
pMovePre b m@(M n c)
	| diff <= 0 = (1, m)
	| diff > 8 = (1, m)
	| otherwise = (0.1 ^ diff, m)
		where diff = n - count b c

pChaPre :: Bill -> Move -> Double
pChaPre b m@(M n c) -- Computes prob of winning a challenge
	| diff <= 0 = 0
	| diff > 8 = 1
	| otherwise = 1 - 0.1 ^ diff
	where diff = n - count b c

pMovePost :: Game -> Move -> (Double, Move)
pMovePost g m = pMovePre (bill g) m -- Temp, only considers last move

pChaPost :: Game -> (Double, Move)
pChaPost g
	| moves g == [] = (0, C)
	| otherwise = (pChaPre (bill g) $ last $ moves g, C) -- Temp, see above

combiner :: Game -> Move -> (Double, Move)
combiner g C = pChaPost g
combiner g m = pMovePost g m

sortedOptions :: Game -> [(Double, Move)] -- Gives each move with the odds of winning
sortedOptions g = L.sort $ map (combiner g) $ legalMoves g

bestMove :: Game -> Move
bestMove g = snd $ last $ sortedOptions g


----------- PARSERS ------------

strip :: String -> String
strip = L.filter (\x -> not (elem x "\n ()"))

parseCard :: P.Parsec String st Card
parseCard = liftA (\x -> read('C':[x])) PC.digit

parseInitial :: P.Parsec String st Game
parseInitial = liftA3 G
	(PC.string "Yourbill:" >> P.count 8 parseCard)
	(PC.string "Youropponent:" >> many PC.anyChar) $ pure []

parseMoves :: P.Parsec String st Move
parseMoves = liftA2 M
	(PC.string "Lastmove:" >> pure read <*> P.many PC.digit)
	(PC.char ',' >> parseCard) <|> pure C


----------- MAIN ------------

hostname = "localhost"
port = "5000"
username = "core"
password = "oftheapple"

openConnection :: HostName -> String -> IO Socket
openConnection hn p = do
		addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
		let serveraddr = head addrinfos
		sock <- socket (addrFamily serveraddr) Stream defaultProtocol
		setSocketOption sock KeepAlive 1
		connect sock (addrAddress serveraddr)
		recv sock 1024
		return sock

login :: Socket -> String -> String -> IO String
login s u p = do
	send s $ unwords [u, p] 
	recv s 1024

play :: Socket -> Game -> IO ()
play s g1 = do
	move <- liftA (rightExtr.(P.parse parseMoves "")) (liftA strip $ recv s 1024)
	case move of
		C -> return ()
		M q c -> do
			let g2 = addMove g1 move
			send s $ format $ bestMove g2
			play s g2

run :: HostName -> String -> String -> String -> IO ()
run h p u pw = do
	s <- openConnection h p
	login s u pw
	send s "m"
	game <- pure (rightExtr.(P.parse parseInitial "")) <*> (liftA strip $ recv s 1024)
	forkIO $ run h p u pw
	play s game
	close s