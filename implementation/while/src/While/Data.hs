module While.Data where
import While.Base
import While.DataExpression
import While.ProgramType
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Char (char)
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import Text.Parsec.Expr

binaryOp name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix name fun = Prefix (do{ reserved name; return fun }) 

table = [ [ prefix "hd" HdExp, prefix "tl" TlExp ],
	[binaryOp "." (ConsExp) AssocRight],
	[binaryOp "=" EqualityExp AssocNone]]

nilExp = do {
	reserved "nil";
	return NilExp
}
operators = buildExpressionParser table term
 
varExp = do {
	dat <- identifier;
	return $ Var dat
} <?> "Variable"

symExp = do {
	char ':';
	name <- identifier;
	return $ Symbol name;
} <?> "Symbol"

numExp = do {
	dat <- natural;
	return $ intAsData dat
} <?> "number"

evalExp = do {
	name <- brackets identifier;
	argument <- parens dataExpression;
	return $ FunctionCall name argument
} <?> "function call"

consExp = do {
	reserved "cons";
	dat1 <- dataExpression;
	dat2 <- dataExpression;
	return $ ConsExp dat1 dat2
}

universalCallExp = do {
	dat1 <- between (symbol "[[") (symbol "]]") dataExpression;
	argument <- parens dataExpression;
	return $ UniversalCall dat1 argument
} <?> "Universal call"

sourceExp = do {
	name <- wakkas identifier;
	return $ Source name
} <?> "Source"

atomExp = do {
	reserved "atom?";
	d <- dataExpression;
	return $ AtomExp d
}

wakkas x =  between (symbol "<") (symbol ">") x

term = parens dataExpression <|>  sourceExp <|> nilExp <|> numExp <|> consExp <|> atomExp <|> varExp <|> symExp <|> try universalCallExp <|> evalExp 

dataExpression = operators <?> "Data Expression"

parseData = parse dataExpression "(unknown)"

parseDataFile = parseFromFile dataExpression

main = do
    args <- getArgs
    let parsed = parseData (head args)
    case parsed of
        Left err -> error $ show err
        Right ast -> print ast
