module While.Helpers where
import While.Base
import While.DataExpression
import While.ProgramType
import While.Tree
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error
import Data.Monoid (Sum, Monoid)
import Control.Monad.State
import qualified Data.Map as M

type Evaluation a = WriterT RuntimeEnvironment (StateT Context (ErrorT String Identity)) a
runEvaluation env ev = runIdentity (runErrorT (runStateT (runWriterT ev) env ))
-- LOGGING
data (Ord a) => Max a = Max {getMax :: a} | MaxNull
	deriving (Eq, Show, Ord)
instance (Ord a) => Monoid (Max a) where
	mempty = MaxNull
	mappend MaxNull x = x
	mappend x MaxNull = x
	mappend (Max {getMax = x}) (Max {getMax = y}) = Max {getMax = max x y}

data RuntimeEnvironment = RuntimeEnvironment {
	counter :: Sum Integer,
	maxDataSize :: Max Integer
} deriving (Show, Eq, Ord)

instance Monoid RuntimeEnvironment where
	mempty = RuntimeEnvironment { counter = mempty :: Sum Integer, maxDataSize = mempty :: Max Integer }
	mappend 
		(RuntimeEnvironment { counter = c1, maxDataSize = m1 })
		(RuntimeEnvironment { counter = c2, maxDataSize = m2 }) =
			RuntimeEnvironment {counter = c1 `mappend` c2, maxDataSize = m1 `mappend` m2}


tick :: Integer -> Evaluation ()
tick n = tell RuntimeEnvironment { counter = Sum n, maxDataSize = mempty }

reportDataUsage :: Integer -> Evaluation ()
reportDataUsage n = tell RuntimeEnvironment { counter = mempty, maxDataSize =Max n }
-- LOOKUP

type ContextDict = M.Map String Tree

data Context = Context { dict :: ContextDict, parentContext :: Maybe Context }
	deriving (Show, Eq)

instance Monoid Context where
	mempty = Context { dict = M.empty, parentContext = Nothing }
	a `mappend` b = b { parentContext = Just a }

evalData :: DataExpression -> Evaluation Tree
evalData NilExp = return Nil
evalData (HdExp y) = do 
	ev <- evalData y
	case ev of 
		Cons x _ -> return x
		Nil -> return $ error "Hd of nil"
evalData (TlExp y) = do
	ev <- evalData y
	case ev of
		Cons _ x -> return x
		Nil -> return $ error "Tl of nil"
evalData (ConsExp x y) = do
	ev_left <- evalData x
	ev_right <- evalData y
	return $ Cons ev_left ev_right
evalData (Var name) = do
	context <- get
	return $ lookup name context
		where
			lookup n (Context {dict = d, parentContext = Nothing}) 
				| n `M.member` d = (M.!) d n
				| otherwise = Nil
			lookup n (Context { dict = d, parentContext = (Just p) }) 
				| n `M.member` d = (M.!) d n
				| otherwise = lookup n p
setVal name val = do {
	context@(Context {dict = d, parentContext = _}) <- get;
	put (context {dict = M.insert name val d})
}
 
sizeof :: DataExpression -> Evaluation Integer
sizeof dat = do
	ev <- evalData dat;
	let ds = dataSize ev in
		return ds 
		where 
			dataSize :: Tree -> Integer
			dataSize Nil = 0
			dataSize (Cons a b) = 1 + dataSize a + dataSize b
