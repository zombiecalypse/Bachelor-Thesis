module While.Statement where
import While.Base
import While.Data
import While.Helpers
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import System.Environment (getArgs)
import System.Console.GetOpt 



type Block = [WhileStatement]

data WhileStatement =
	Assign Name DataExpression          | -- name := data
	While DataExpression Block          |
	IfElse DataExpression Block Block     -- if data then block1 else block2
	deriving (Show, Eq)

data Program = Program {
	programName :: Name,
	input :: Name,
	block :: Block,
	output :: Name
	}
	deriving (Show, Eq)

fileExpression = do {
	proc_name <- identifier;
	reserved "read";
	readVar <- identifier;
	blockVal <- blockExp;
	reserved "write";
	writeVar <- identifier;
	return Program { programName = proc_name, input = readVar, block = blockVal, output = writeVar }
}

blockExp = between (symbol "{") (symbol "}") $ sepBy statement whiteSpace

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
