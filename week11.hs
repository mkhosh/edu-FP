loop = tail loop

ones = 1 : ones

primes [] = []
primes (x:xs) = x : primes [y | y <- xs, mod y x /= 0] --(filter (\y -> mod y x /= 0) xs) 


