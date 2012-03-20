import Text.ParserCombinators.Parsec
import qualified ParsecToken as P
import ParsecLanguage (haskellDef)

newtype Name = String
data DataExpression = 
	Nil                                 |
	Hd WhileExpression                  |
	Tl WhileExpression                  |
	Cons WhileExpression WhileExpression
data WhileExpression = 
	Var Name |
	Const DataExpression


lexer :: TokenParser ()
lexer = makeTokenParse

parseTuring :: String -> Either ParseError [Exp]
parseTuring input = parse turingFile "(unknown)" input
