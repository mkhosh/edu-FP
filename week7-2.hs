module Parsing2 where

p :: Parser (Char,Char)
p = do x <- item
       item
       y <- item
       return (x,y)
