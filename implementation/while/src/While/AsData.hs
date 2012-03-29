module While.AsData where
import While.Parser
import While.Statement
import While.Data

int_as_data :: Integer -> DataExpression
int_as_data 0 = NilExp
int_as_data n = NilExp `ConsExp` (int_as_data (n-1))

program_as_data :: Program -> DataExpression
program_as_data (Program {
	program_name = _,
	input = read_var,
	block = block,
	output = write_var}) = (var_as_data read_var) `ConsExp` 
							(block_as_data block) `ConsExp` 
							(var_as_data write_var) 
