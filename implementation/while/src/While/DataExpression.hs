module While.DataExpression where
import Data.Foldable
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

intAsData :: Integer -> DataExpression
intAsData 0 = NilExp
intAsData n = NilExp `ConsExp` intAsData (n-1)

