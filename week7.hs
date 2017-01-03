module Parsing where

type Parser a = String -> [(a,String)]

item :: Parser Char
item = \inp -> case inp of
                  [] -> []
                  (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \inp -> []

return :: a -> Parser a
return v = \inp -> [(v,inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
                    [] -> parse q inp
                    [(v,inp)] -> [(v,inp)]

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

ex1 = parse (Parsing.return 1) "abc"
ex2 = parse (item +++ Parsing.return 'd') "abc"
ex3 = parse (failure +++ Parsing.return 'd') "abc"

--p :: Parser (Char,Char)
p = do x <- item
       item
       y <-  item
       Parsing.return (x,y)

-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do x <- item
--            if p x then
--              Parsing.return x
--            else
--              failure
