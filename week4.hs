import Data.Char

main = print 1

factors n = [x | x <- [1..n], mod n x == 0]

perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect x = sum (init (factors x)) == x

equiv = concat [[(x,y)| x <- [1,2,3],  y <- [4,5,6]]]

find k t = [v | (k',v) <- t, k' == k]

testFind = find 2 [(1,'a'),(2,'b'),(3,'c'),(2,'a')]

positions x xs = find x (zip xs [0..] )

testPositions = positions 'b' ['a','b','c','b']

dot xs ys = sum [x*y| (x, y) <- zip xs ys]

dot2 xs ys = sum (zipWith (*) xs ys)

testDot = dot2 [1,2,3] [4,5,6]

let2int c = ord c - ord 'a'

int2let n = chr(ord 'a' + n)

shiftLower n c = int2let ((let2int c + n) `mod` 26)

shift n c
  | isLower c = shiftLower n c
  | isUpper c = toUpper (shiftLower n (toLower c))
  | otherwise = c

encode n xs = [shift n x| x <- xs]

divides n1 n2 = mod n1 n2 == 0

divisors n = [x | x <- [1..n],divides n x]
