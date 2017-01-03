main = print (f 12 13)

f x y = x * y

abs2 x = if x>=0 then x else -x

abs3 x | x>=0 = x
       | otherwise = -x

(&*) :: Bool -> Bool -> Bool
True &* b = b 
_ &* _ = False

sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

const2 x = \_ -> x

const3 x _ = x

recip2 xs = map (1/) xs

halve xs = splitAt (length xs `div` 2) xs

halve2 xs = (take n xs, drop n xs)
  where n =length xs `div` 2

safetail xs = if null xs then xs else tail xs

safetail2 (_:xs)
  | null xs = []
  | otherwise  =tail xs

safetail3 [] = []
safetail3 xs = tail xs
  
safetail4 xs = case xs of
                 [] -> []
                 (_ : xs) -> xs

b ||* c
  | b == c = b
  | otherwise = True
