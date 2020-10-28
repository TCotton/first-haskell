-- module Main where

-- import Lib (someFunc)

-- zip examples
-- zip[1,2,3][4,5,6]
-- [(1,4),(2,5),(3,6)]
-- zip [1,2,4]"abc"
-- [(1,'a'),(2,'b'),(4,'c')]
-- zip3 "glasgow" "beijing" "nairobi"
-- [('g','b','n'),('l','e','a'),('a','i','i'),('s','j','r'),('g','i','o'),('o','n','b'),('w','g','i')]
-- zip [1..10] ['a'..'z']
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]
-- zipWith max [1,2,3][0,2,4]
-- [1,2,4]
-- zipWith (+) [1,2,3][0,2,4]
-- [1,4,7]
-- zipWith(\x->(\y->(x,y))) [1,2,3] "a,b,c"
-- [(1,'a'),(2,','),(3,'b')]
-- putStrLn "hello world"
-- use do to specify a sequence of actions
-- use <- inside a do to associate input values with names
-- do { putStrLn "what is your name?"; x <- getLine; putStrLn ("hello " ++ x) }
-- whatis your name?
-- john
-- let greet() = do
-- planet <- getLine
-- home <- getLine
-- putStrLn ("greetings " ++ planet ++ "ling.")
-- putStrLn ("I am from " ++ home ++ ".")
-- putStrLn "Take me to your leader."
-- let a= reverse "winston"; b = reverse "churchill"  in "sir " ++a ++" "++ b
-- "sir notsniw llihcruhc"
-- What is the difference between -> and <- in Haskell syntax?
-- <- is for associating names with values in do blocks whereas -> is used for defining functions.
whatever :: IO ()
whatever = do
  putStrLn "what is your name?"
  x <- getLine
  putStrLn ("hello " ++ x)

nameDo :: IO ()
nameDo = do
  putStr "What is your first name? "
  first <- getLine
  putStr "And your last name? "
  last <- getLine
  let full = first ++ " " ++ last
  putStrLn ("Pleased to meet you, " ++ full ++ "!")

here :: IO ()
here = do
  putStrLn "what is your name?"
  x <- getLine
  putStrLn ("hello " ++ x)

fact2 :: Int -> Int
fact2 n = if n == 0 then 1 else n * fact2 (n -1)

check :: String -> String -> Char -> (Bool, String)
check word display c =
  ( c `elem` word,
    [ if x == c
        then c
        else y
      | (x, y) <- zip word display
    ]
  )

turn :: String -> String -> Int -> IO ()
turn word display n =
  do
    if n == 0
      then putStrLn "You lose"
      else
        if word == display
          then putStrLn "You win!"
          else mkguess word display n

mkguess :: String -> [Char] -> Int -> IO ()
mkguess word display n =
  do
    putStrLn (display ++ "  " ++ take n (repeat '*'))
    putStr "  Enter your guess: "
    q <- getLine
    let (correct, display') = check word display (q !! 0)
    let n' = if correct then n else n -1
    turn word display' n'

starman :: String -> Int -> IO ()
starman word = turn word ['-' | x <- word]

-- :l main.js d
