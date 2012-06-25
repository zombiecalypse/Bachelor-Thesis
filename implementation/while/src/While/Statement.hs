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

{-
 -programAsData :: Program -> DataExpression
 -programAsData (Program {
 -  programName = _,
 -  input = read_var,
 -  block = block,
 -  output = write_var}) = var_as_data read_var `ConsExp` 
 -              block_as_data block `ConsExp` 
 -              var_as_data write_var
 -}


fileExpression = do {
	proc_name <- identifier;
	reserved "read";
	readVar <- identifier;
	blockVal <- blockExp;
	reserved "write";
	writeVar <- identifier;
	return Program { programName = proc_name, input = readVar, block = blockVal, output = writeVar }
}

blockExp = between (symbol "{") (symbol "}") $ endBy statement whiteSpace

statement = 
		try assignmentExpression <|> 
		try whileExpression <|> 
		try ifThenElseExpression

assignmentExpression = do {
	name <- identifier;
	reservedOp ":=";
	dataexp <- dataExpression;
	return (Assign name dataexp)
	}

whileExpression = do {
	reserved "while";
	dataexp <- dataExpression;
	blockVal <- blockExp;
	return (While dataexp blockVal)
}

ifThenElseExpression = do {
	reserved "if";
	dataexp <- dataExpression;
	ifBlockVal <- blockExp;
	reserved "else";
	elseBlockVal <- blockExp;
	return $ IfElse dataexp ifBlockVal elseBlockVal
}

parseWhile = parse fileExpression "(unknown)"

parseWhileFile = parseFromFile fileExpression


main = do {
		inps <- getArgs;
		parsed <- parseWhileFile (head inps);
		case parsed of
				Left err -> error $ show err;
				Right ast -> print ast;
}
