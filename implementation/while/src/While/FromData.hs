module While.FromData where
import While.Parser
import While.Statement
import While.Data
import While.Base (Tree(Nil,Cons))

as_number :: Tree -> Maybe Integer
as_number Nil = Just 0
as_number (Cons (Cons _ _) _) = Nothing
as_number (Cons Nil x) = (as_number x) >>= (Just . (1+))

as_list :: Tree -> [Tree]
as_list Nil = []
as_list (Cons x y) = x:(as_list y)
