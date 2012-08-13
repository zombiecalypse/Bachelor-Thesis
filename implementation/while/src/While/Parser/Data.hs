module While.Parser.Data where
import While.Base
import While.Parser.DataExpression
import While.Parser.ProgramType
import System.Environment (getArgs)
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (parseFromFile)

nilExp = do {
	reserved "nil";
	return NilExp
}
hdExp = do {
	reserved "hd";
	dat <- dataExpression;
	return $ HdExp dat
}
tlExp = do {
	reserved "tl";
	dat <- dataExpression;
	return $ TlExp dat
}

consExp = (consExplicit <|> parens consDotted) <?> "cons expression"
	where
		consExplicit = do {
    	reserved "cons";
    	dat1 <- dataExpression;
    	dat2 <- dataExpression;
    	return $ ConsExp dat1 dat2
		}
		consDotted = do {
			dat1 <- dataExpression;
			reservedOp ".";
			dat2 <- dataExpression;
			return $ ConsExp dat1 dat2
		}

varExp = do {
	dat <- identifier;
	return $ Var dat
} <?> "variable"

numExp = do {
	dat <- integer;
	return $ intAsData dat
} <?> "number"

evalExp = do {
	name <- brackets identifier;
	argument <- parens dataExpression;
	return $ FunctionCall name argument
} <?> "function call"
	
	
bareDataExpression = nilExp <|> numExp <|> hdExp <|> tlExp <|> consExp <|> varExp <|> evalExp
dataExpression = (try (parens bareDataExpression) <|> bareDataExpression) <?> "Data Expression"

parseData = parse dataExpression "(unknown)"

parseDataFile = parseFromFile dataExpression

main = do
    args <- getArgs
    let parsed = parseData (head args)
    case parsed of
        Left err -> error $ show err
        Right ast -> print ast
