module While.Statement where
import While.Base
import While.Data
import While.Helpers
import While.ProgramType
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import System.Environment (getArgs)
import System.Console.GetOpt 
import Data.Maybe (fromMaybe)

progExpression = do {
	proc_name <- identifier;
	reserved "read";
	readVar <- identifier;
	blockVal <- blockExp;
	reserved "write";
	writeVar <- identifier;
	return Program { programName = proc_name, input = readVar, block = blockVal, output = writeVar }
}

fileExpression = do 
	optional whiteSpace
	progs <- progExpression `sepBy1` whiteSpace
	optional whiteSpace
	return progs

blockExp = (braces $ statement `sepBy1` (whiteSpace <|> optional semi) ) <?> "block"

statement = 
		try forExpression <|>
		try ifThenElseExpression <|>
		try whileExpression <|> 
		try assignmentExpression

assignmentExpression = do {
	name <- identifier;
	reservedOp ":=";
	dataexp <- dataExpression;
	return (Assign name dataexp)
	} <?> "assignment"

whileExpression = do {
	reserved "while";
	dataexp <- dataExpression;
	blockVal <- blockExp;
	return $ While dataexp blockVal
} <?> "while statement"

forExpression = do {
	reserved "for";
	name <- identifier;
	reservedOp "=";
	dataexp <- dataExpression;
	blockVal <- blockExp;
	return $ For name dataexp blockVal
} <?> "for statement"

ifThenElseExpression = do {
	reserved "if";
	dataexp <- dataExpression;
	ifBlockVal <- blockExp <?> "if block";
	elseBlockVal <- option [] (try elseBlock);
	return $ IfElse dataexp ifBlockVal elseBlockVal
} <?> "if statement"
	where elseBlock = do {
			reserved "else";
			blockExp
	} <?> "else block"

parseWhile = parse fileExpression "(unknown)"

parseWhileFile = parseFromFile fileExpression

main = do {
		inps <- getArgs;
		parsed <- parseWhileFile (head inps);
		case parsed of
				Left err -> error $ show err;
				Right ast -> print ast;
}
