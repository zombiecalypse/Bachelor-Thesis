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
	MaxNull `mappend` x = x
	x `mappend` MaxNull = x
	(Max {getMax = x}) `mappend` (Max {getMax = y}) = Max {getMax = max x y}

type RuntimeEnvironment = (Sum Integer, Max Integer)

counter = fst
maxDataSize = snd


tick :: Integer -> Evaluation ()
tick n = tell (Sum n, mempty)

reportDataUsage :: Integer -> Evaluation ()
reportDataUsage n = do 
	Context { dict = d, parentContext = _} <- get
	let maxDataSize = maximum $ map dataSize $ M.elems d
	tell (mempty, Max (max n maxDataSize))

-- LOOKUP

type ContextDict = M.Map String Tree

data Context = Context { dict :: ContextDict, parentContext :: Maybe Context }
	deriving (Show, Eq)

instance Monoid Context where
	mempty = Context { dict = M.empty, parentContext = Nothing }
	a `mappend` b = b { parentContext = Just a }

