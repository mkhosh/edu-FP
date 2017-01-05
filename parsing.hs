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
  fmap g p = P (\imp -> case parse p imp of
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
ex40 = parse (pure digitToInt <*> item) "abc"
ex4 = parse (aFparser <*> item) "abc"

aFparser :: Parser (Char -> Int)
aFparser = P (\inp -> case inp of
                        [] -> []
                        x:xs -> [(\y -> digitToInt x + digitToInt y,xs)])

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

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])

ex7 = parse empty "abc"
ex8 = parse (item <|> return 'd') "abc"
ex9 = parse (empty <|> return 'd') "abc"

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

ex10 = parse (char 'a') "acdaba"

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ex11 = parse (string "abc") "abcdef"
ex12 = parse (string "abc") "ab1234"
ex13 = parse (many digit) "123abc"
ex14 = parse (many digit) "abc"
ex15 = parse (some digit) "abc"

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

ex16 = parse ident "abc def"
--[("abc"," def")]

ex17 =  parse nat "123 abc"
--[(123," abc")]

ex18 = parse space " abc"
-- [((),"abc")]

int :: Parser Int
int = (do char '-'
          space
          n <- nat
          return (-n))
      <|> nat

ex19 = parse int "-123 abc"
--[(-123," abc")]
ex20 = parse int "123 abc"
--[(123," abc")]

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

ex21 = parse nats " [1, 2, 3] "
--[([1,2,3],"")]

ex22 = parse nats "[1,2,]"
--[]

ex23 = parse nats "[1  ]"
--[]

expr :: Parser Int
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e))
            <|> return t

term :: Parser Int
term = do f <- factor
          (do symbol "*"
              t <- term
              return (f * t))
            <|> return f

factor :: Parser Int
factor = (do symbol "("
             e <- expr
             symbol ")"
             return e)
         <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n,[])]  -> n
            [(_,out)] -> error ("Unused input " ++ out)
            []        -> error "Invalid input"

ex24 = eval " 2 +3*4"
--14

ex25 = eval "2*(3+4)"
--14

ex26 = eval "2*3^4"
-- *** Exception: Unused input ^4

ex27 = eval "one plus two"
-- *** Exception: Invalid input”
