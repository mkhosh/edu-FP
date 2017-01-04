module Parsing where
import Data.Char
import Control.Monad

infixr 5 +++

-- | The monad of parsers
-- | ---------------------
type Parser a = String -> [(a,String)]


item :: Parser Char
item = \inp -> case inp of
                  [] -> []
                  (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \inp -> []

preturn :: a -> Parser a
preturn v = \inp -> [(v,inp)]


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
                    [] -> parse q inp
                    r -> r

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

ex1 = parse (preturn 1) "abc"
ex2 = parse (item +++ preturn 'd') "abc"
ex3 = parse (failure +++ preturn 'd') "abc"

--p :: Parser (Char,Char)
p = do x <- item
       item
       y <-  item
       preturn (x,y)

-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do x <- item
--            if p x then
--              Parsing.return x
--            else
--              failure
