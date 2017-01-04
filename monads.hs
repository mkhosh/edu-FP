module Monads where
import Data.Char

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

test a b c = a + b + c
res = pure test <*> Just 1 <*> Just 2 <*> Just 3
res2 = test <$> Just 1 <*> Just 2 <*> Just 3

-- getChars 0 = return []
-- getChars n = pure (:) <*> getChar <*> getChars (n-1)

getChars n = sequenceA (replicate n getChar)

data Expr = Val Int | Div Expr Expr

eval (Val n) = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m
-- eval (Div x y) =
--   eval x >>= \n ->
--   eval y >>= \m ->
--   safediv n m

safediv _ 0 = Nothing
safediv n m = Just (div n m)

pairs xs ys = [(x,y)| x <- xs, y <- ys]
pairs2 xs ys = do x <- xs
                  y <- ys
                  return (x,y)

type State = Int

newtype ST a = S(State -> (a,State))

app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -< b) -> ST a -> ST b
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S(\s -> (x,s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s -> let (f,s') = app stf s
                             (x,s'') = app stx s' in (f x, s''))

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S(\s -> let (x,s') = app st s in app (f x) s')

data Tr a = Le a | No (Tr a) (Tr a) deriving Show

tr = No (No (Le 'a') (Le 'b')) (Le 'c')

rl (Le _) n = (Le n, n+1)
rl (No l r) n = (No l' r', n'')
                    where
                       (l',n') = rl l n
                       (r',n'') = rl r n'

rlabel (Leaf _) n = (Leaf n, n+1)
rlabel (Node l x r) n = (Node l' n'' r', n''+1)
                      where
                        (l',n') = rlabel l n
                        (r',n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n,n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l x r) = Node <$> alabel l <*> fresh <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do n <- fresh
                     return (Leaf n)
mlabel (Node l x r) = do l' <- mlabel l
                         n <- fresh
                         r' <- mlabel r
                         return (Node l' n r')


conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing
