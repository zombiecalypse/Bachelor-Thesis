module While.FromData where
import While.Parser
import While.Statement
import While.Data
import While.DataExpression
import While.Tree (Tree(Nil,Cons))

asNumber = treeAsInt

asList :: Tree -> [Tree]
asList Nil = []
asList (Cons x y) = x:asList y
