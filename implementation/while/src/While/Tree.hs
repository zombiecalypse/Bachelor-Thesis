{-# LANGUAGE FlexibleInstances #-}
module While.Tree where
import Data.Monoid

data Tree = Nil | Cons Tree Tree
	deriving (Eq)

instance Show Tree where	
	show Nil = "nil"
	show (Cons l r) = "(" ++ show l ++ "." ++ show r ++ ")"

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

dataSize :: Tree -> Integer
dataSize Nil = 0
dataSize (Cons a b) = 1 + dataSize a + dataSize b
