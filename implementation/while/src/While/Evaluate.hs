module Interpreter.While where
import Text.Parsers.While
import Text.Parsers.While.Statement
import Text.Parsers.While.Data
import qualified Data.Map as M
import Control.Monad.State
import System (getArgs)

data Tree = Nil | Cons Tree Tree
	deriving (Show, Eq)

type ContextDict = M.Map String Tree

data Context = Context { dict :: ContextDict, parentContext :: Maybe Context }

evalData :: Context -> DataExpression -> Tree
evalData _ NilExp = Nil
evalData c (HdExp y) = case (evalData c y) of 
	Cons x _ -> x
	Nil -> error "Hd of nil"
evalData c (TlExp y) = case (evalData c y) of
	Cons _ x -> x
	Nil -> error "Tl of nil"
evalData c (ConsExp x y) = Cons (evalData c x) (evalData c y)
evalData (Context { dict = d, parentContext = Nothing }) (Var n) 
	| n `M.member` d = (M.!) d n
	| otherwise = Nil
evalData (Context { dict = d, parentContext = (Just p) }) v@(Var n) 
	| n `M.member` d = (M.!) d n
	| otherwise = evalData p v

evalStatement :: Context -> WhileStatement -> Context
evalStatement c@(Context { dict = d, parentContext = p }) (Assign name dat) = 
	c { dict = (M.insert name (evalData c dat) d) }
evalStatement c (IfElse d b1 b2) =
	case (evalData c d) of
		Nil -> (evalBlock c b2)
		_   -> (evalBlock c b1)
evalStatement c w@(While d b) =
	case (evalData c d) of
		Nil -> c
		_   -> evalStatement (evalBlock c b) w

evalBlock :: Context -> [WhileStatement] -> Context
evalBlock = foldl evalStatement

interpretProgram (Program {
	program_name = _, 
	input = read_var, 
	block = block, 
	output = write_var}) read = evalData (evalBlock 
				(Context {dict = (M.singleton read_var read), parentContext = Nothing}) 
				block) 
			(Var write_var)

main = do {
    args <- getArgs;
    parsed <- parseWhileFile (args!!0); 
	let {
		input = parseData (args!!1) ;
		ast = either (error . show) id  parsed;
		inputDataExpression = either (error . show) id input;
		inputData = evalData (Context { dict = M.empty, parentContext = Nothing }) inputDataExpression } in
	print (interpretProgram ast inputData)
}
