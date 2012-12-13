module While.Dialect where

data Calling = NoCall | Calling | Recursion deriving (Eq, Ord, Show)
data Symbols = OnlyNil | DefineAsYouGo deriving (Eq, Ord, Show)
data Allow = Disallow | Allow deriving (Eq, Ord, Show)

data WhileDialect = WhileDialect {
	name       :: String, 
	calling    :: Calling,
	symbols    :: Symbols,
	assignment :: Allow,
	while      :: Allow,
	for        :: Allow,
	cons       :: Allow,
	universal  :: Allow
} deriving (Show, Eq)

whileLanguage = WhileDialect {
	name = "WHILE",
	calling = Recursion,
	symbols = DefineAsYouGo,
	assignment = Allow,
	while = Allow,
	for = Allow,
	cons = Allow,
	universal = Allow
}

forLanguage = whileLanguage {
	name = "FOR",
	calling = Calling,
	while = Disallow
}
