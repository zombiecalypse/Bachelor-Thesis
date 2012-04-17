module While.Data where
import While.Base
import System.Environment (getArgs)
import Data.Foldable
import Data.Monoid (Monoid, mempty, mappend)
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (parseFromFile)

type Name = String
data DataExpression =
	NilExp                                 |
	HdExp DataExpression                   |
	TlExp DataExpression                   |
	ConsExp DataExpression DataExpression  |
	Var Name                               
	deriving (Show, Eq)

instance Monoid DataExpression where
	mempty = NilExp
	a `mappend` b = ConsExp a b

flatSize (Var _) = 1
flatSize (HdExp x) = (flatSize x) - 1
flatSize (TlExp x) = (flatSize x) - 1
flatSize (ConsExp x y) = 1 + (flatSize x) + (flatSize y)
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

consExp = do {
    reserved "cons";
    dat1 <- dataExpression;
    dat2 <- dataExpression;
    return $ ConsExp dat1 dat2
}

varExp = do {
	dat <- identifier;
	return $ Var dat
}

	
bareDataExpression = (nilExp <|> hdExp <|> tlExp <|> consExp <|> varExp)
dataExpression = parens bareDataExpression <|> bareDataExpression

parseData = parse dataExpression "(unknown)"

parseDataFile = parseFromFile dataExpression

main = do
    args <- getArgs
    let parsed = parseData (args!!0)
    case parsed of
        Left err -> error $ show err
        Right ast -> print ast
