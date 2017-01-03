module Week9 where

type Pos = (Int,Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

type Pair a = (a,a)

multi :: Num a => Pair a -> a 
multi (x,y) = x * y

copy :: a -> Pair a
copy x = (x,x)

data Answer = Yes | No | Unknown deriving Show

filpAns Yes = No
flipAns No = Yes
flipAns Unknown = Unknown

data Shape = Circle Float | Rectangle Float Float

square n = Rectangle n n

area (Circle r) = pi * pi ^ 2
area (Rectangle x y) = x * y

data Nat = Zero | Succ Nat deriving Show

nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))
zero = Zero
one = Succ Zero
two = Succ one
three = Succ two
four = Succ three

add Zero n = n
add (Succ m) n = Succ (add m n)

six = add four two

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

data Tree = Leaf Int
          | Node Tree Int Tree

aTree = Node (Node (Leaf 1) 3 (Leaf 4))
             5
             (Node (Leaf 6) 7 (Leaf 9))

occurs m (Leaf n) = m == n
occurs m (Node l n r) | m==n = True
                        | m<n = occurs m l
                        | m>n = occurs m r

flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r
