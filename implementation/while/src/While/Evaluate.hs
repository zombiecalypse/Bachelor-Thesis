module Interpreter.While where
import Text.Parsers.While
import Data.Map

data Tree = Nil | Cons Tree Tree

type ContextDict = Map String Tree

data Context = Context { dict :: ContextDict, parentContext :: Maybe Context }

evalStatement :: Context -> WhileStatement -> Context
evalStatement (Context { dict : d, parentContext :p }) (Assign name dat) = 
