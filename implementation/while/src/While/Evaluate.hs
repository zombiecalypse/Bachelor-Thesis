module While.Evaluate where
import While.Parser
import While.Statement
import While.Data
import While.Base
import While.FromData
import While.Helpers 
import Control.Monad
import Control.Monad.State
import Data.Monoid 
import qualified Data.Map as Map
import System.Environment (getArgs)

usingData :: DataExpression -> Evaluation ()
usingData dat = do {
	size <- sizeof dat;
	reportDataUsage size;
	tick (flatSize dat)
}

useData d = do
	usingData d;
	tick 1;
	evalData d

evalStatement :: WhileStatement -> Evaluation ()
evalStatement (Assign name dat) = do {
	ev <- useData dat;
  modify (\l -> l {dict = Map.insert name ev (dict l)})
}
evalStatement (IfElse d b1 b2) = do
	ev <- useData d;
	case ev of
		Nil -> evalBlock b2
		_   -> evalBlock b1
evalStatement w@(While d b) = do
	ev <- useData d;
	case ev of
		Nil -> return ()
		_   -> do
			evalBlock b;
			evalStatement w

evalBlock b = forM_ b evalStatement

interpretProgram (Program {
	programName = _, 
	input = read_var, 
	block = block, 
	output = write_var}) read = do
		inp <- evalData read;
		put (mempty { dict = Map.singleton read_var inp})
		ret <- evalBlock block;
		evalData (Var write_var)

data Report = Report {
	returnValue :: Tree,
	commandsExecuted :: Integer,
	spaceUsed :: Integer
} deriving (Show, Eq)
	
runProgram a i = interpret $ runEvaluation (mempty :: Context) (interpretProgram a i) where
	interpret (Left s) = error s
	interpret (Right ((r, env), _)) = compileReport r env
	compileReport r (RuntimeEnvironment {counter = Sum {getSum = ticks}, maxDataSize = Max {getMax = space}}) = Report {spaceUsed = space, commandsExecuted = ticks, returnValue = r}

main = do
	args <- getArgs
	parsed <- parseWhileFile (head args)
	let parsedInp = parseData (args!!1)
	case parsed of
		Left err -> error $ show err
		Right ast -> case parsedInp of
			Left err -> error $ show err
			Right inp -> print $ runProgram ast inp
