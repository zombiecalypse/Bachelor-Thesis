{-# LANGUAGE FlexibleInstances #-}
module While.Tree where
import Data.Monoid

data Tree = Nil | Cons Tree Tree | Sym String
	deriving (Eq)

infixr 5 `Cons`

instance Show Tree where	
	show Nil = "nil"
	show (Cons l r) = "(" ++ show l ++ "." ++ show r ++ ")"
	show (Sym s) = ':' : s

instance Monoid Tree where
	mempty = Nil
	mappend = Cons

class TreeBijection a where
	toTree    :: a -> Tree
	fromTree  :: Tree -> a

instance TreeBijection Tree where
	toTree = id
	fromTree = id

instance TreeBijection [Tree] where
	toTree = mconcat
	fromTree (Cons a b) = a:(fromTree b)

instance TreeBijection Bool where
	toTree True = Cons Nil Nil
	toTree False = Nil
	fromTree Nil = False
	fromTree _ = True

dataSize :: Tree -> Integer
dataSize Nil = 0
dataSize (Sym _) = 1
dataSize (Cons a b) = 1 + dataSize a + dataSize b
