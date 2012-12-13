module While.DataExpression( Name, DataExpression(..), intAsData, intAsTree, treeAsInt, flatSize) where
import While.Tree
import Data.Functor
import Data.Monoid (Monoid, mempty, mappend)

flatSize (Var _) = 1
flatSize (Symbol _) = 1
flatSize (Source _) = 1
flatSize (EqualityExp a b) = 1
flatSize (HdExp x) = flatSize x - 1
flatSize (TlExp x) = flatSize x - 1
flatSize (ConsExp x y) = 1 + flatSize x + flatSize y
flatSize NilExp = 0
flatSize (AtomExp _) = 1
flatSize (FunctionCall _ _) = 0
flatSize (UniversalCall _ _) = 0

type Name = String
data DataExpression =
	NilExp                                     |
	HdExp       DataExpression                 |
	TlExp       DataExpression                 |
	ConsExp     DataExpression DataExpression  |
	EqualityExp DataExpression DataExpression  |
	Var Name                                   |
	Symbol Name                                |
	Source Name                                |
	FunctionCall Name DataExpression           |
	AtomExp DataExpression                     |
	UniversalCall DataExpression DataExpression
	deriving (Show, Eq)

instance Monoid DataExpression where
	mempty = NilExp
	a `mappend` b = ConsExp a b

data IntegerFormat = IntegerFormat {
	fromInt :: Integer -> Tree,
	toInt   :: Tree    -> Maybe Integer
}

unary = IntegerFormat {
	fromInt = from,
	toInt = to
}
	where 
		from 0 = Nil
		from n = Nil `Cons` from (n-1)
		to Nil = Just 0
		to ((Cons _ _) `Cons` _) = Nothing
		to (Nil `Cons` x) = to x >>= (Just . (1+))

binary = IntegerFormat {
	fromInt = from,
	toInt = to
} where 
				c0 = Nil `Cons` Nil
				c1 = Nil `Cons` Nil `Cons` Nil
				from n = fromList $ reverse $ binList n
					where
						binDigit 0 = c0
						binDigit n = c1
						binList 0 = [c0]
						binList 1 = [c1]
						binList n = binDigit (n`mod`2) : binList (n`div`2)
				fromList = foldr mappend Nil

				to x = do 
					digits <- mapM asDigit (asList x)
					return (foldl (\a b -> 2*a+b) 0 digits)
				asDigit Nil = Just 0
				asDigit (Cons Nil Nil) = Just 0
				asDigit (Nil `Cons` Nil `Cons` Nil) = Just 1
				asDigit x = Nothing
				asList Nil = []
				asList (Cons a b) = a: asList b

literal :: Tree -> DataExpression
literal Nil = NilExp
literal (a `Cons` b) = (literal a) `ConsExp` (literal b)

intAsData = literal . fromInt binary
intAsTree = fromInt binary
treeAsInt = toInt binary
