{-# OPTIONS_GHC -Wall #-}

{-
 Binary Search Tree 
-}

data Tree a = Node (Tree a) a (Tree a)
            | Nil
              deriving (Eq)

instance Show a => Show (Tree a)
  where show Nil = "Nil"
        show (Node Nil x Nil) = show x
        show (Node l x r) = "([" ++ show x ++ "] " ++ show l ++ " " ++ show r ++ ")"

-- ref: http://www.seas.upenn.edu/~cis194/spring13/lectures/07-folds-monoids.html
foldT :: b -> (b -> a -> b -> b) -> Tree a -> b
foldT e _ Nil = e
foldT e f (Node l x r) = f (foldT e f l) x (foldT e f r)

size :: Tree a -> Integer
size = foldT 0 (\l _ r -> 1 + l + r)

sumT :: Tree Integer -> Integer
sumT = foldT 0 (\l x r -> l + x + r)

height :: Tree a -> Integer
height = foldT 0 (\l _ r -> 1 + max l r)

flatten :: Tree a -> [a]
flatten = foldT [] (\l x r -> l ++ [x] ++ r)

maxT :: (Ord a, Bounded a) => Tree a -> a
maxT = foldT minBound (\l x r -> l `max` x `max` r)

minT :: (Ord a, Bounded a) => Tree a -> a
minT = foldT maxBound (\l x r -> l `min` x `min` r)

elemT :: Eq a => a -> Tree a -> Bool
elemT x (Node l y r) = x == y || (elemT x l) || (elemT x r)
elemT _ Nil = False

-- supports multiple nodes of same value
add :: Ord a => Tree a -> a -> Tree a
add Nil x = (Node Nil x Nil)
add (Node l v r) x
  | x < v = Node (add l x) v r
  | otherwise = Node l v (add r x)

-- ref: Introduction to Algorithms
--  Cormen, Leiserson, Rivest, Stein
deleteT :: Ord a => a -> Tree a -> Tree a

deleteT x (Node Nil v r)
  | x == v = r

deleteT x (Node l v Nil)
  | x == v = l

deleteT x (Node l v (Node Nil w r))
  | x == v = Node l w r

deleteT x (Node l v (Node (Node Nil b r) a s))
  | x == v = Node l b (Node r a s)

deleteT x (Node l v r)
  | x < v = Node (deleteT x l) v r
  | otherwise = Node l v (deleteT x r)

deleteT _ Nil = Nil

-- naive implementation
buildT :: Ord a => [a] -> Tree a
buildT = naive Nil
  where naive t (x:xs) = naive (add t x) xs
        naive t [] = t

