module Week9hw where

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero | Succ Nat deriving Show

zero = Zero
one = Succ Zero
two = Succ one

nat2int :: Nat -> Integer
nat2int = \n -> genericLength [c | c <- show n, c == 'S']

int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add n Zero = n
add n (Succ m) = Succ (add n m)

mult n Zero = Zero
mult n (Succ m) = add n (mult n m)

data Tree = Leaf Integer | Node Tree Tree deriving Show
