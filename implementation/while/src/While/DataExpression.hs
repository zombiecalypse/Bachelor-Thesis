module While.DataExpression( Name, DataExpression(..), intAsData, treeAsInt) where
import While.Tree
import Data.Functor
import Data.Monoid (Monoid, mempty, mappend)

type Name = String
data DataExpression =
	NilExp                                 |
	HdExp DataExpression                   |
	TlExp DataExpression                   |
	ConsExp DataExpression DataExpression  |
	Var Name                               
	deriving (Show, Eq)

instance Monoid DataExpression where
	mempty = NilExp
	a `mappend` b = ConsExp a b

data IntegerFormat = IntegerFormat {
	fromInt :: Integer -> DataExpression,
	toInt   :: Tree    -> Maybe Integer
}
unary = IntegerFormat {
	fromInt = from,
	toInt = to
}
	where 
		from 0 = NilExp
		from n = NilExp `ConsExp` from (n-1)
		to Nil = Just 0
		to (Cons (Cons _ _) _) = Nothing
		to (Cons Nil x) = to x >>= (Just . (1+))

binary = IntegerFormat {
	fromInt = from,
	toInt = to
} where 
				from 0 = NilExp
				from 1 = NilExp `ConsExp` NilExp
				from n = (from (n `mod` 2)) `ConsExp` (from (n `div` 2))

				to x = do 
					digits <- mapM asDigit (asList x)
					return (foldl (\a b -> 2*a+b) 0 digits)
				asDigit Nil = Just 0
				asDigit (Cons Nil Nil) = Just 1
				asDigit x = Nothing
				asList Nil = []
				asList (Cons a b) = a:(asList b)

intAsData_ :: IntegerFormat -> Integer -> DataExpression
intAsData_ f = fromInt f

treeAsInt_ f = toInt f

intAsData = intAsData_ binary
treeAsInt = treeAsInt_ binary
