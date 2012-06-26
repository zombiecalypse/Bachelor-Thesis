module While.Helpers where
import While.Base
import While.DataExpression
import While.ProgramType
import While.Tree
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Data.Monoid (Sum, Monoid)
import Control.Monad.State
import qualified Data.Map as M

type Evaluation a = WriterT RuntimeEnvironment (StateT Context (ReaderT (M.Map Name Program) (ErrorT String Identity))) a
runEvaluation :: [Program] -> Context -> Evaluation a -> Either String (a, RuntimeEnvironment, Context)
runEvaluation procs context environ = report val
	where 
		val = runIdentity $ runErrorT $ runReaderT (runStateT (runWriterT environ) context) procmap
		report (Right ((return_val, runtime_log), dict)) = Right (return_val, runtime_log, dict)
		report (Left s) = Left s
		procmap = M.fromList [(programName p, p) | p <- procs]
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
reportDataUsage n = do 
	Context { dict = d, parentContext = _} <- get
	let maxDataSize = maximum $ map dataSize $ M.elems d
	tell RuntimeEnvironment { counter = mempty, maxDataSize =Max (max n maxDataSize) }

-- LOOKUP

type ContextDict = M.Map String Tree

data Context = Context { dict :: ContextDict, parentContext :: Maybe Context }
	deriving (Show, Eq)

instance Monoid Context where
	mempty = Context { dict = M.empty, parentContext = Nothing }
	a `mappend` b = b { parentContext = Just a }

