{
module Parser (parser) where
import Data.Char
}
%name turing
%tokentype { Token }
%error { parseError }
%token
	ide	{ T_Ident $$  }
	';' { T_Separator }
	':' { T_Colon }
	'=>' { T_Arrow }
	'L' { T_L }
	'R' { T_R }
	'N' { T_N }
%%

File	: {- empty -}  { [] }
		| Expr ';' File  { $1 : $3 }

Expr : Endstate { $1 }
	 | Transition { $1 }

Transition : ide ide '=>' ide ide dir { T_Transition $1 $2 $4 $5 $6 }

dir : 'L' { T_L } 
	| 'R' { T_R }
	| 'N' { T_N }

Endstate : ':' ide { TokenIdentifier $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = T_Ident TokenIdentifier | T_Separator | T_Colon | T_EOF | T_L |
					T_N | T_R | T_Arrow | 
					T_Transition TokenIdentifier TokenIdentifier TokenIdentifier TokenIdentifier Token
	deriving Show

lexer :: String -> [Token]
lexer [] = []

parse :: [Token] -> ([EndState], [Transition])
parse [] = ([], [])
parse c = (parse_rec c [] []) where
	parse_rec [] a b = (a, b)
	parse_rec (T_Colon:(T_Ident a):cs) ends t = parse_rec cs ((EndState a):ends) t
	parse_rec (c:cs) a b = parse_rec cs a b

parser = parse . lexer

data EndState = EndState TokenIdentifier
	deriving Show

data Dir = L | N | R
	deriving Show

toDir "L" = L
toDir "R" = R
toDir "N" = N

data Transition = Transition ((TokenIdentifier, TokenIdentifier), (TokenIdentifier, TokenIdentifier, Dir))
	deriving Show

data TokenIdentifier = TokenIdentifier String
	deriving Show
}
