module While.Base where
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

whileGrammar = javaStyle
	{
	P.commentStart = "/*",
	P.commentEnd   = "*/",
	P.commentLine  = "//",
	P.nestedComments = True,
	P.identStart = letter <|> char '_',
	P.identLetter = alphaNum <|> char '_',
	P.reservedNames = ["read", "write", "while", "end", "hd", "tl", "nil"],
	P.reservedOpNames = ["=", ".", ":="],
	P.caseSensitive = True
}


lexer = P.makeTokenParser whileGrammar

symbol = P.symbol lexer
identifier = P.identifier lexer
operator = P.operator lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer
braces = P.braces lexer
whiteSpace = P.whiteSpace lexer

data Tree = Nil | Cons Tree Tree
	deriving (Eq)

instance Show Tree where	
	show Nil = "nil"
	show (Cons l r) = "(" ++ (show l) ++ "." ++ (show r) ++ ")"
