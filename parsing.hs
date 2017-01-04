module Parsing where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x,xs)])

instance Functor Parser where
  -- fmap (a -> b) -> Parser a -> Parser b
  fmap g p = P(\imp -> case parse p imp of
                         [] -> []
                         [(v,out)] -> [(g v, out)])

ex1 = parse (fmap toUpper item) "abd"
ex2 = parse (fmap toUpper item) ""

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                           [] -> []
                           [(g,out)] -> parse (fmap g px) out)

ex3 = parse (pure 1) "abc"
ex4 = parse (pure digitToInt <*> item) "abc"

-- three :: Parser (Char,Char)
-- three = pure g <*> item <*> item <*> item
--         where g x y z = (x,z)

ex5 = parse three "abcde"
ex6 = parse three "ab"

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v,out)] -> parse (f v) out)

three :: Parser (Char,Char)
three = do x <- item
           item
           y <- item
           return (x,y)
