module While.Helpers where
import While.Data
import While.Base
import Control.Monad.State
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
	mappend (Max {getMax = x}) (Max {getMax = y}) = Max {getMax = (max x y)}

data RuntimeEnvironment = RuntimeEnvironment {
	counter :: Sum Integer,
	max_data_size :: Max Integer
} deriving (Show, Eq, Ord)

instance Monoid RuntimeEnvironment where
	mempty = RuntimeEnvironment { counter = mempty :: Sum Integer, max_data_size = mempty :: Max Integer }
	mappend 
		(RuntimeEnvironment { counter = c1, max_data_size = m1 })
		(RuntimeEnvironment { counter = c2, max_data_size = m2 }) =
			RuntimeEnvironment {counter = c1 `mappend` c2, max_data_size = m1 `mappend` m2}


tick :: Integer -> Evaluation ()
tick n = do
	tell RuntimeEnvironment { counter = Sum n, max_data_size = mempty }

reportDataUsage :: Integer -> Evaluation ()
reportDataUsage n = do
	tell RuntimeEnvironment { counter = mempty, max_data_size =Max n }
-- LOOKUP

type ContextDict = M.Map String Tree

data Context = Context { dict :: ContextDict, parentContext :: Maybe Context }
	deriving (Show, Eq)

instance Monoid Context where
	mempty = Context { dict = M.empty, parentContext = Nothing }
	a `mappend` b = b { parentContext = Just a }

evalData :: DataExpression -> Evaluation Tree
evalData NilExp = do
	return Nil
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
	put (context {dict = (M.insert name val d)})
}
 
sizeof :: DataExpression -> Evaluation Integer
sizeof dat = do
	ev <- evalData dat;
	let ds = dataSize ev in
		return ds 
		where 
			dataSize :: Tree -> Integer
			dataSize Nil = 0
			dataSize (Cons a b) = 1+(dataSize a)+(dataSize b)
