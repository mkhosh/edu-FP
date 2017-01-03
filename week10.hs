main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 830)

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  
--apply :: Op -> Int -> Int -> Int
apply Add  = (+)
apply Sub  = (-)
apply Mul  = (*)
apply Div = div

valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && mod x y == 0

data Expr = Val Int | App Op Expr Expr 

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

v = App Add (Val 1)  (App Mul (Val 2) (Val 3))

values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval (Val n) = [n | n > 0]
eval (App o l r)  = [apply o x y | x <- eval l
                                 , y <- eval r
                                 , valid o x y]

subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs)) 

choices = concat . map perms . subs

solution e ns n = elem (values e) (choices ns) && eval e == [n]

split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
             l <- exprs ls,
             r <- exprs rs,
             e <- combine l r]

combine l r = [App op l r | op <- ops]
ops = [Add,Sub,Mul,Div]

solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
               lx <- results ls,
               ry <- results rs,
               res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r,apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]
