module While.Data where
import While.Base
import While.DataExpression
import While.ProgramType
import System.Environment (getArgs)
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import Text.ParserCombinators.Parsec.Token (integer)


flatSize (Var _) = 1
flatSize (HdExp x) = flatSize x - 1
flatSize (TlExp x) = flatSize x - 1
flatSize (ConsExp x y) = 1 + flatSize x + flatSize y
flatSize NilExp = 0

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

consExp = consExplicit <|> consDotted
	where
		consExplicit = do {
    	reserved "cons";
    	dat1 <- dataExpression;
    	dat2 <- dataExpression;
    	return $ ConsExp dat1 dat2
		}
		consDotted = do {
			symbol "(";
			whiteSpace;
			dat1 <- dataExpression;
			whiteSpace;
			symbol ".";
			whiteSpace;
			dat2 <- dataExpression;
			whiteSpace;
			symbol ")";
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
	
bareDataExpression = nilExp <|> hdExp <|> tlExp <|> consExp <|> varExp <|> numExp
dataExpression = try (parens bareDataExpression) <|> bareDataExpression

parseData = parse dataExpression "(unknown)"

parseDataFile = parseFromFile dataExpression

main = do
    args <- getArgs
    let parsed = parseData (head args)
    case parsed of
        Left err -> error $ show err
        Right ast -> print ast
