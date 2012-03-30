module Parser (parseTuring, Exp (Endstate, Transition, Startstate), TapeVal (TapeVal, Blank, AnyChar), StateIdentifier, Movement (L, N, R), isStartstate, isEndstate, isTransition) where
import Text.ParserCombinators.Parsec

data Exp = Startstate StateIdentifier | Endstate StateIdentifier | Transition StateIdentifier TapeVal StateIdentifier TapeVal Movement
	deriving (Show,Eq,Ord)
data TapeVal = TapeVal String | Blank | AnyChar
	deriving (Show,Eq,Ord)
data StateIdentifier = StateIdentifier String
	deriving (Show,Eq,Ord)
data Movement = L | N | R
	deriving (Show,Ord,Eq)

turingFile = endBy expression separator

separator = do{
	many $ string " ";
	optional (string ";");
	many $ string " ";
	eol;
	many $ string " ";
}
expression = transition <|> endstate <|> startstate


startstate = do{
				string ">";
				name <- identifier;
				return (Startstate $ StateIdentifier name)
				}
endstate   = do{ 
				string ":" ; 
				name <- identifier;
				return (Endstate $ StateIdentifier name)
				}
tapechar = do{
	name <- (string "#") <|> (string "_") <|> identifier ;
	return (case name of
			"#" -> Blank
			"_" -> AnyChar
			x   -> TapeVal x)
    }

transition = do {
				state <- identifier ;
				skipMany (string " ");
				readHead <- tapechar ;
				skipMany (string " ");
				string "=>";
				skipMany (string " ");
				newState <- identifier;
				skipMany (string " ");
				newReadHead <- tapechar;
				skipMany (string " ");
				mov <- oneOf "LNR" ;
				return (Transition 
							(StateIdentifier state) 
							readHead
							(StateIdentifier newState)	
							newReadHead
							(toDir mov))
				}

identifier = many1 alphaNum

eol = try (string "\n\r")
		<|> try (string "\r\n")
		<|> string "\n"
		<|> string "\r"

toDir 'L' = L
toDir 'N' = N
toDir 'R' = R
toDir x = error $ (show x) ++ " is not a valid direction"

isStartstate (Startstate _) = True
isStartstate _ = False

isEndstate (Endstate _) = True
isEndstate _ = False

isTransition (Transition _ _ _ _ _) = True
isTransition _ = False


parseTuring :: String -> Either ParseError [Exp]
parseTuring input = parse turingFile "(unknown)" input
