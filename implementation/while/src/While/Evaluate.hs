module While.Evaluate where
import While.Base
import While.Data
import While.DataExpression
import While.Dialect as D
import While.FromData
import While.Helpers 
import While.Parser
import While.ProgramType
import While.Statement
import While.Tree (Tree(Nil))
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid 
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Console.GetOpt 
import Text.Parsec.Error (ParseError)
import Data.Maybe (fromMaybe)
import While.Tree

evalData :: DataExpression -> Evaluation Tree
evalData NilExp = return Nil
evalData (HdExp y) = do 
	ev <- evalData y
	context <- get
	case ev of 
		Cons x _ -> return x
		Nil -> fail ("Hd of nil -- " ++ show context)
evalData (TlExp y) = do
	ev <- evalData y
	context <- get
	case ev of
		Cons _ x -> return x
		Nil -> fail ("Tl of nil" ++ show context)
evalData (ConsExp x y) = do
	dialect <- getDialect
	case D.cons dialect of
		Allow -> do
					ev_left <- evalData x
					ev_right <- evalData y
					return $ Cons ev_left ev_right
		Disallow -> fail "Cons is not an allowed operation"
evalData (Var name) = do
	context <- get
	return $ lookup name context
		where
			lookup n (Context {dict = d, parentContext = Nothing}) 
				| n `M.member` d = (M.!) d n
				| otherwise = Nil
			lookup n (Context { dict = d, parentContext = (Just p) }) 
				| n `M.member` d = (M.!) d n
				| otherwise = lookup n p
evalData (FunctionCall name arg) = do
	dialect <- getDialect
	case D.calling dialect of
		NoCall -> fail "No calling is allowed"
		Calling -> do
			context <- get
			if isRecursive $ Just context
				then fail "No recursion allowed"
				else eval
		Recursion -> eval
	where
		isRecursive Nothing = False
		isRecursive (Just p) = functionName p == name || isRecursive (parentContext p)
		eval = do 
					procs <- getPrograms
					context <- get
					put (mempty { functionName = name, parentContext = Just context })
					r <- interpretProcedure ((M.!) procs name) arg
					put context
					return r

setVal name val = do {
	context@(Context {dict = d, parentContext = _}) <- get;
	put (context {dict = M.insert name val d})
}
 
sizeof :: DataExpression -> Evaluation Integer
sizeof dat = do
	ev <- evalData dat;
	let ds = dataSize ev in
		return ds 

usingData :: DataExpression -> Evaluation ()
usingData dat = do {
	size <- sizeof dat;
	reportDataUsage size;
	tick (flatSize dat)
}

useData d = do
	usingData d;
	evalData d

evalStatement :: WhileStatement -> Evaluation ()
evalStatement (Assign name dat) = do {
	ev <- useData dat;
	tick 1;
  modify (\l -> l {dict = M.insert name ev (dict l)})
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

interpretProgram l read = interpretProcedure (last l) read

interpretProcedure (Program {
	programName = name, 
	input = read_var, 
	block = block, 
	output = write_var}) read = do
		inp <- evalData read;
		put (mempty { functionName = name, dict = M.singleton read_var inp})
		ret <- evalBlock block;
		evalData (Var write_var)

data Report = Report {
	returnValue :: Tree,
	commandsExecuted :: Integer,
	spaceUsed :: Integer,
	reportedContext :: Context
} deriving (Show, Eq)
	
runProgram d a i = interpret $ runEvaluation d a mempty (interpretProgram a i) where
	interpret (Left s) = error s
	interpret (Right (r, env, context)) = compileReport r env context
	compileReport r (Sum {getSum = ticks}, Max {getMax = space}) context = Report {spaceUsed = space, commandsExecuted = ticks, returnValue = r, reportedContext = context}

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
	Option "I" ["int"] (NoArg (\o -> return o { optFormat = IntegerFormat} )) "write output as integer",
	Option "L" ["list"] (NoArg (\o -> return o { optFormat = ListFormat} )) "write output as integer",
	Option "i" ["input"] (OptArg _readInput "DataExpression") "use this input instead of nil" ]

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
format f (Report { returnValue = r, commandsExecuted = nc, spaceUsed = ns, reportedContext = c }) = 
	"Return value : " ++ retFormat f r ++ "\n" ++
	"Time         : " ++ show nc       ++ "\n" ++
	"Space        : " ++ show ns
	where 
		retFormat TreeFormat = show
		retFormat ListFormat = show . asList
		retFormat IntegerFormat = show . fromMaybe (error "NaN") . asNumber
	
main dialect = do
	(opts, while_files, msgs) <- parseArgs
	forM_ msgs putStr
	guard (msgs == [])
	parsed <- mapM parseWhileFile while_files
	let ast = concat $ map orParseError parsed
	let dat = orParseError $ optInput opts
	putStrLn $ format (optFormat opts) $ runProgram dialect ast dat
