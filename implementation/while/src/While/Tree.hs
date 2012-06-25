module While.Tree where

data Tree = Nil | Cons Tree Tree
	deriving (Eq)

instance Show Tree where	
	show Nil = "nil"
	show (Cons l r) = "(" ++ show l ++ "." ++ show r ++ ")"
