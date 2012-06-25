module While.FromData where
import While.Parser
import While.Statement
import While.Data
import While.Tree (Tree(Nil,Cons))

asNumber :: Tree -> Maybe Integer
asNumber Nil = Just 0
asNumber (Cons (Cons _ _) _) = Nothing
asNumber (Cons Nil x) = asNumber x >>= (Just . (1+))

asList :: Tree -> [Tree]
asList Nil = []
asList (Cons x y) = x:asList y
