module Foldables where
import Data.Monoid
import Data.Foldable

data List a = Nil | Cons a (List a) 

instance (Show a) => Show (List a) where
   show Nil = "."
   show (Cons x xs) = show x ++ ", " ++ show xs 

l1 = Cons 12 Nil
l2 = Cons 1 l1

   
instance Monoid (List a) where
  -- mempty :: List a
  mempty = Nil

  -- mappend :: List a -> List a -> List a
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x (mappend xs ys) 


ex0 = getSum (foldMap Sum [1..10])
--55

ex1 = getProduct (foldMap Product [1..10])
--3628800”

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x) = x
  fold (Node l r) = fold l `mappend` fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 4)

ex2 = [foldr (+) 0 tree, getProduct (foldMap Product tree)]

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

ex3 = traverse dec [1,2,3]
--Just [0,1,2]

ex4 = traverse dec [2,1,0]
--Nothing
