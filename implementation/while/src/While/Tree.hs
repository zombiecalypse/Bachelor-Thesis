module While.Tree where

data Tree = Nil | Cons Tree Tree
	deriving (Eq)

instance Show Tree where	
	show Nil = "nil"
	show (Cons l r) = "(" ++ show l ++ "." ++ show r ++ ")"

dataSize :: Tree -> Integer
dataSize Nil = 0
dataSize (Cons a b) = 1 + dataSize a + dataSize b
