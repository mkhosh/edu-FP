module Monads where

data Tree a = Leaf a | Node (Tree a) a (Tree a) 

t1 = Node (Leaf "abcn") "ef" (Leaf "ghi")
t2 = Node (Leaf "jkl") "mo" (Leaf "d")
t3 = Node t1 "uzvzi" t2
t4 = Node t3 "z" (Leaf "ab")

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance (Show a) => Show (Tree a) where
  show (Leaf x) = show x
  show (Node l x r) = "(" ++ (show l) ++ ")<-" ++ (show x) ++ "->(" ++ (show r) ++ ")"

inc :: (Functor f,Num a) => f a -> f a
inc = fmap (+1)
