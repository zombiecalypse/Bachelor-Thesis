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
	toTree NilExp = Sym "nil"
	toTree (HdExp a) = Sym "hd" `Cons` toTree a
	toTree (TlExp a) = Sym "tl" `Cons` toTree a
	toTree (ConsExp a b) = Sym "cons" `Cons` toTree a `Cons` toTree b
	toTree (Var a) = (Sym "var") `Cons` (Sym a)
	toTree (Symbol a) = Sym "symbol" `Cons` Sym a
	toTree (FunctionCall name input) = Sym "call" `Cons` Sym name `Cons` toTree input
	toTree (Source name) = Sym "Source" `Cons` Sym name
	toTree (UniversalCall d1 d2) = Sym "interpret" `Cons` toTree d1 `Cons` toTree d2
	fromTree (Sym "nil") = NilExp
	fromTree (Sym "hd" `Cons` a) = HdExp $ fromTree a
	fromTree (Sym "tl" `Cons` a) = TlExp $ fromTree a
	fromTree (Sym "cons" `Cons` a `Cons` b) = ConsExp (fromTree a) (fromTree b)
	fromTree (Sym "var" `Cons` Sym a) = Var a
	fromTree (Sym "symbol" `Cons` Sym a) = Symbol a
	fromTree (Sym "call" `Cons` Sym a `Cons` b) = FunctionCall a $ fromTree b
	fromTree (Sym "Source" `Cons` Sym name) = Source name
	fromTree (Sym "interpret" `Cons` d1 `Cons` d2) = UniversalCall (fromTree d1) (fromTree d2)

instance TreeBijection WhileStatement where
	toTree (Assign name exp) = Sym "Assign" `Cons` Sym name `Cons` toTree exp
	toTree (For name exp block) = Sym "for" `Cons` Sym name `Cons` toTree exp `Cons` toTree block
	toTree (While exp block) = Sym "for" `Cons` toTree exp `Cons` toTree block
	toTree (IfElse exp ifblock elseblock) = Sym "if" `Cons` toTree exp `Cons` toTree ifblock `Cons` toTree elseblock
	fromTree (Sym "Assign" `Cons` Sym name `Cons` a) = Assign name $ fromTree a
	fromTree (Sym "for" `Cons` Sym name `Cons` exp `Cons` block) = For name (fromTree exp) (fromTree block)
	fromTree (Sym "while" `Cons` exp `Cons` block) = While (fromTree exp) (fromTree block)
	fromTree (Sym "if" `Cons` exp `Cons` ifblock `Cons` elseblock) = IfElse (fromTree exp) (fromTree ifblock) (fromTree elseblock)

instance (TreeBijection Program) where
	toTree p = Sym "program" `Cons` Sym (programName p) `Cons` Sym (input p) `Cons` Sym (output p) `Cons` toTree (block p) `Cons` Nil
	fromTree (Sym "program" `Cons` Sym name `Cons` Sym i `Cons` Sym o `Cons` b `Cons` Nil) = Program { programName = name, input = i, output = o, block = fromTree b }

instance (TreeBijection Block) where
  toTree [] = Nil
  toTree (x:y) = toTree x `Cons` toTree y
  fromTree Nil = []
  fromTree (Cons x y) = (fromTree x):(fromTree y)

instance TreeBijection Integer where
	toTree = intAsTree
	fromTree = fromMaybe 0 . treeAsInt
