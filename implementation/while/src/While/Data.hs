module Text.Parsers.While.Data where
import Text.Parsers.While.Base
import System (getArgs)
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (parseFromFile)

type Name = String
data DataExpression =
	Nil                                 |
	Hd DataExpression                   |
	Tl DataExpression                   |
	Cons DataExpression DataExpression  |
	Var Name                            |
	Parens (Either DataExpression ()) DataExpression (Either DataExpression ()) -- Before, inside, after
	deriving (Show, Eq)

nilExp = do {
	reserved "nil";
	return Nil
}
hdExp = do {
	reserved "hd";
	dat <- dataExpression;
	return $ Hd dat
}
tlExp = do {
	reserved "tl";
	dat <- dataExpression;
	return $ Tl dat
}

consExp = do {
    reserved "cons";
    dat1 <- dataExpression;
    dat2 <- dataExpression;
    return $ Cons dat1 dat2
}

varExp = do {
	dat <- identifier;
	return $ Var dat
}

	
bareDataExpression = (nilExp <|> hdExp <|> tlExp <|> consExp <|> varExp)
dataExpression = parens bareDataExpression <|> bareDataExpression

parseData = parse dataExpression

parseFile = parseFromFile dataExpression

main = do
    args <- getArgs
    parsed <- parseFile (args!!0)
    case parsed of
        Left err -> error $ show err
        Right ast -> print ast



