module While.ProgramType(Block, WhileStatement(..), Program(..)) where
import While.DataExpression

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
