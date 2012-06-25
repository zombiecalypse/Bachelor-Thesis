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
import System.Console.GetOpt 
import Text.Parsec.Error (ParseError)
import Data.Maybe (fromMaybe)

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

data OutputFormat = TreeFormat | ListFormat | IntegerFormat deriving (Show, Eq)

data Options = Options {
	optFormat :: OutputFormat,
	optInput  :: Either ParseError DataExpression
} deriving (Show)

defaultOptions = Options {
	optFormat = TreeFormat,
	optInput = Right NilExp
}

options = [ 
	Option ['I'] ["int"] (NoArg (\o -> return o { optFormat = IntegerFormat} )) "write output as integer",
	Option ['i'] ["input"] (OptArg _readInput "DataExpression") "use this input instead of nil" ]

_readInput (Just exp) o = return o { optInput = parseData exp }
_readInput Nothing o = return o { optInput = Right NilExp }

parseArgs = do
	args <- getArgs
	let ( actions, nonOpts, msgs ) = getOpt Permute options args
	opts <- foldl (>>=) (return defaultOptions) actions
	return (opts, nonOpts, msgs)

orParseError (Left err) = error $ show err
orParseError (Right x) = x

format :: OutputFormat -> Report -> String
format f (Report { returnValue = r, commandsExecuted = nc, spaceUsed = ns }) = 
	"Return value : " ++ (retFormat f r) ++ "\n" ++
	"Time         : " ++ (show nc) ++ "\n" ++
	"Space        : " ++ (show ns) 
	where 
		retFormat TreeFormat = show
		retFormat ListFormat = show . asList
		retFormat IntegerFormat = show . fromMaybe (error "NaN") . asNumber
	
main = do
	(opts, (while_file:rst), msgs) <- parseArgs
	forM_ msgs putStr
	guard (msgs == [])
	parsed <- parseWhileFile while_file
	let ast = orParseError parsed
	let dat = orParseError $ optInput opts
	putStrLn $ format (optFormat opts) $ runProgram ast dat
