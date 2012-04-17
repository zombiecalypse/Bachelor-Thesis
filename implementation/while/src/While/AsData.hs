module While.AsData where
import While.Parser
import While.Statement
import While.Data

intAsData :: Integer -> DataExpression
intAsData 0 = NilExp
intAsData n = NilExp `ConsExp` intAsData (n-1)

programAsData :: Program -> DataExpression
programAsData (Program {
	programName = _,
	input = read_var,
	block = block,
	output = write_var}) = var_as_data read_var `ConsExp` 
							block_as_data block `ConsExp` 
							var_as_data write_var
