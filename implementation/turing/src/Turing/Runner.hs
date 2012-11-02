module Runner where
import Parser
import Data.Map (Map, (!), empty, insert)
import Data.List (find)
import Text.ParserCombinators.Parsec (ParseError)
import System.Environment

data TuringMachine = TuringMachine [TapeVal] [TapeVal] StateIdentifier (Map (StateIdentifier, TapeVal) (StateIdentifier, TapeVal, Movement)) [StateIdentifier]
	deriving (Eq, Show)

step (TuringMachine [] a b c d) = step $ TuringMachine [Blank] a b c d
step (TuringMachine a [] b c d) = step $ TuringMachine a [Blank] b c d
step tm@(TuringMachine left (readHead:right) state m endstates) 
		| done tm = tm
		| otherwise = TuringMachine newleft newright newstate m endstates where
			delta x y = m ! (x,y)
			(newstate, newtapechar, mov) = delta state readHead 
			(newleft, newright) = move mov left (newtapechar:right)
			move L (l:ls) rs    = (ls, l:rs)
			move L [] rs        = move L [Blank] rs
			move R ls (r:rs)    = (r:ls, rs)
			move R ls []        = move R ls [Blank]
			move N ls rs        = (ls, rs)

done (TuringMachine _ _ state _ endstates) = state `elem` endstates

act tm
		| done tm   = tm
		| otherwise = act $ step tm

actOn (TuringMachine left _ state m endstates) lst = act (TuringMachine left lst state m endstates)

createTm _ _ [] = error "Endstates must be given"
createTm startstate m endstates = TuringMachine [] [] startstate m endstates

toTuring s = createTm startstate m endstates where
		expressions = right $ parseTuring s
		right (Right x) = x
		right x = error $ show x
		just (Just x) = x
		just Nothing = error "Expected startstate"
		name (Startstate x) = x
		name (Endstate x ) = x
		name _ = error "WTF"
		startstate = name $ just $ find isStartstate expressions
		endstates = map name $ filter isEndstate expressions
		m = toMap (filter isTransition expressions)
		insertStateTransition (Transition oldState oldtape newState newtape movement) m = insert (oldState, oldtape) (newState, newtape, movement) m
		insertStateTransition _ _ = error "WTF"
		toMap ts = foldr insertStateTransition empty ts

asTape :: [Char] -> [TapeVal]
asTape = map (TapeVal . \x -> [x])

state (TuringMachine _ _ x _ _) = x
tape :: TuringMachine -> [TapeVal]
tape (TuringMachine l r _ _ _) = reverse l ++ r

parseMachineAndAct :: String -> String -> String
parseMachineAndAct s g = (show $ state ended) ++ "\n" ++ (show $ tape ended)
	where ended = ((toTuring s) `actOn` (asTape g))


main = do
	[f,g] <- getArgs
	s <- readFile f
	putStr $ (parseMachineAndAct s g) ++ "\n"
