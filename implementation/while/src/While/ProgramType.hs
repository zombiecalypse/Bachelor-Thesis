{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module While.ProgramType(Block, WhileStatement(..), Program(..)) where
import While.DataExpression
import While.Tree
import Data.Maybe (fromMaybe)

type Block = [WhileStatement]

data WhileStatement =
	Assign Name DataExpression          | -- name := data
	While DataExpression Block          |
	For Name DataExpression Block       |
	IfElse DataExpression Block Block     -- if data then block1 else block2
	deriving (Show, Eq)

data Program = Program {
	programName :: Name,
	input :: Name,
	block :: Block,
	output :: Name
	}
	deriving (Show, Eq)

instance TreeBijection DataExpression where
	toTree NilExp = Sym "nilexp"
	toTree (HdExp a) = Sym "hdexp" `Cons` toTree a `Cons` Nil
	toTree (TlExp a) = Sym "tlexp" `Cons` toTree a `Cons` Nil
	toTree (ConsExp a b) = Sym "consexp" `Cons` toTree a `Cons` toTree b `Cons` Nil
	toTree (Var a) = (Sym "var") `Cons` (Sym a) `Cons` Nil
	toTree (Symbol a) = Sym "symbol" `Cons` Sym a `Cons` Nil
	toTree (AtomExp a) = Sym "atom" `Cons` toTree a `Cons` Nil
	toTree (FunctionCall name input) = Sym "call" `Cons` Sym name `Cons` toTree input `Cons` Nil
	toTree (Source name) = Sym "Source" `Cons` Sym name `Cons` Nil
	toTree (UniversalCall d1 d2) = Sym "interpret" `Cons` toTree d1 `Cons` toTree d2 `Cons` Nil
	fromTree (Sym "nilexp") = NilExp
	fromTree (Sym "hdexp" `Cons` a `Cons` Nil) = HdExp $ fromTree a
	fromTree (Sym "tlexp" `Cons` a `Cons` Nil) = TlExp $ fromTree a
	fromTree (Sym "consexp" `Cons` a `Cons` b `Cons` Nil) = ConsExp (fromTree a) (fromTree b)
	fromTree (Sym "var" `Cons` Sym a `Cons` Nil) = Var a
	fromTree (Sym "symbol" `Cons` Sym a `Cons` Nil) = Symbol a
	fromTree (Sym "atom" `Cons` a `Cons` Nil) = AtomExp (fromTree a)
	fromTree (Sym "literal" `Cons` a `Cons` Nil) = literally a
						where 
								literally Nil = NilExp
								literally (Cons a b) = ConsExp (literally a) (literally b)
								literally (Sym x) = Symbol x
	fromTree (Sym "call" `Cons` Sym a `Cons` b `Cons` Nil) = FunctionCall a $ fromTree b
	fromTree (Sym "Source" `Cons` Sym name `Cons` Nil) = Source name
	fromTree (Sym "interpret" `Cons` d1 `Cons` d2 `Cons` Nil) = UniversalCall (fromTree d1) (fromTree d2)
	fromTree e = error (show e ++ " Does not signify a expression")

instance TreeBijection WhileStatement where
	toTree (Assign name exp) = Sym "Assign" `Cons` Sym name `Cons` toTree exp `Cons` Nil
	toTree (For name exp block) = Sym "for" `Cons` Sym name `Cons` toTree exp `Cons` toTree block
	toTree (While exp block) = Sym "for" `Cons` toTree exp `Cons` toTree block
	toTree (IfElse exp ifblock elseblock) = Sym "if" `Cons` toTree exp `Cons` toTree ifblock `Cons` toTree elseblock
	fromTree (Sym "Assign" `Cons` Sym name `Cons` a `Cons` Nil) = Assign name $ fromTree a
	fromTree (Sym "for" `Cons` Sym name `Cons` exp `Cons` block) = For name (fromTree exp) (fromTree block)
	fromTree (Sym "while" `Cons` exp `Cons` block) = While (fromTree exp) (fromTree block)
	fromTree (Sym "if" `Cons` exp `Cons` ifblock `Cons` elseblock) = IfElse (fromTree exp) (fromTree ifblock) (fromTree elseblock)
	fromTree e = error (show e ++ " does not signify a statement")

instance (TreeBijection Program) where
	toTree p = Sym "program" `Cons` Sym (programName p) `Cons` Sym (input p) `Cons` Sym (output p) `Cons` toTree (block p) `Cons` Nil
	fromTree (Sym "program" `Cons` Sym name `Cons` Sym i `Cons` Sym o `Cons` b `Cons` Nil) = Program { programName = name, input = i, output = o, block = fromTree b }
	fromTree e = error (show e ++ " does not signify a program")

instance (TreeBijection Block) where
  toTree [] = Nil
  toTree (x:y) = toTree x `Cons` toTree y
  fromTree Nil = []
  fromTree (Cons x y) = (fromTree x):(fromTree y)

instance TreeBijection Integer where
	toTree = intAsTree
	fromTree = fromMaybe 0 . treeAsInt
