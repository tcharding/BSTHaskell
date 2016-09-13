import Data.List

{-
 Binary Search Tree 
-}

data Tree a = Node a (Tree a) (Tree a)
            | Nil
              deriving (Eq)

instance Show a => Show (Tree a)
  where show Nil = "Nil"
        show (Node x Nil Nil) = "[" ++ show x ++ "]"
        show (Node x l r) = "(Node " ++ show x ++ " " ++ show l ++ " " ++ show r ++ ")"

height :: Tree a -> Int
height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

size :: Tree a -> Int
size Nil = 0
size (Node _ l r) = 1 + (size l) + (size r)

minT :: Tree a -> a
minT = undefined

maxT :: Tree a -> a
maxT = undefined

preOrder :: Tree a -> [a]
preOrder = undefined

inOrder :: Tree a -> [a]
inOrder = undefined

postOrder :: Tree a -> [a]
postOrder = undefined

search :: Tree a -> Maybe a
search = undefined

predecessor :: a -> Maybe a
predecessor = undefined

successor :: a -> Maybe a
successor = undefined

insertT :: a -> Tree a -> Tree a
insertT = undefined

deleteT :: a -> Tree a -> Tree a
deleteT = undefined
