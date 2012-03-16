import Text.ParserCombinators.Parsec
import qualified ParsecToken as P
import ParsecLanguage (haskellDef)

lexer :: TokenParser ()
lexer = makeTokenParse

parseTuring :: String -> Either ParseError [Exp]
parseTuring input = parse turingFile "(unknown)" input
