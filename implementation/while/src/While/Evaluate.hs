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

evalStatement :: WhileStatement -> Evaluation ()
evalStatement (Assign name dat) = do {
	usingData dat;
	ev <- evalData dat;
	modify (\l -> l {dict = Map.insert name ev (dict l)})
}
evalStatement (IfElse d b1 b2) = do
	usingData d;
	tick 1; -- Check for equality to nil
	ev <- evalData d;
	case ev of
		Nil -> (evalBlock b2)
		_   -> (evalBlock b1)
evalStatement w@(While d b) = do
	usingData d;
	tick 1;
	ev <- evalData d;
	case ev of
		Nil -> return ()
		_   -> do
			evalBlock b;
			evalStatement w

evalBlock b = forM_ b evalStatement

interpretProgram (Program {
	program_name = _, 
	input = read_var, 
	block = block, 
	output = write_var}) read = do
		inp <- evalData read;
		put (mempty { dict = Map.singleton read_var inp})
		ret <- evalBlock block;
		dat <- evalData (Var write_var);
		return dat

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
	parsed <- parseWhileFile (args!!0)
	let parsedInp = parseData (args!!1)
	case parsed of
		Left err -> error $ show err
		Right ast -> case parsedInp of
			Left err -> error $ show err
			Right inp -> do
				print $ runProgram ast inp
