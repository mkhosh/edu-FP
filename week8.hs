import System.IO
import Data.Char

--main :: IO ()
-- main = do  
--     putStrLn "Hello, what's your name?"  
--     name <- getLine  
--     v <- act
--     print ("Hey " ++ name ++ ", you rock!" )  
--     print v
main = playNim initial 1

act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

strlen :: IO ()
strlen = do putStrLn "Type something: "
            xs <- getLine
            putStr "Your thing has "
            putStr (show (length xs))
            putStrLn " characters."

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
                else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)


getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                 putStrLn "You got it!"
                 else
                 do putStrLn (match word guess)
                    play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do putStrLn "ERROR: invalid digit"
                          getDigit prompt
newline :: IO ()
newline = putChar '\n'

playNim :: Board -> Int -> IO ()
playNim board player =
  do newline
     putBoard board
     if finished board then
       do newline
          putStr "Player "
          putStr (show (next player))
          putStrLn " wins!!"
       else
       do newline
          putStr "Player "
          putStrLn (show player)
          row <- getDigit "enter a row number: "
          num <- getDigit "Stars to remvoe : "
          if valid board row num then
            playNim (move board row num) (next player)
            else
            do newline
               putStrLn "ERROR: Invalid move"
               playNim board player

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putChar '\n'
                    putStr' xs
  --putChar x >> putStr' xs

