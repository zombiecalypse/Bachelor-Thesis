module While.Data where
import While.Base
import While.DataExpression
import While.ProgramType
import System.Environment (getArgs)
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import Text.ParserCombinators.Parsec.Token (integer)

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

consExp = consExplicit <|> parens consDotted
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
}

numExp = do {
	dat <- many1 digit;
	return $ intAsData (read dat)
} 

evalExp = do {
	name <- brackets identifier;
	argument <- parens dataExpression;
	return $ FunctionCall name argument
}
	
	
bareDataExpression = nilExp <|> hdExp <|> tlExp <|> consExp <|> varExp <|> numExp <|> evalExp
dataExpression = (try (parens bareDataExpression) <|> bareDataExpression) <?> "Data Expression"

parseData = parse dataExpression "(unknown)"

parseDataFile = parseFromFile dataExpression

main = do
    args <- getArgs
    let parsed = parseData (head args)
    case parsed of
        Left err -> error $ show err
        Right ast -> print ast
