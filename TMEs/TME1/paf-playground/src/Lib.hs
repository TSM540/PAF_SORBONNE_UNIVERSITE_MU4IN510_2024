module Lib
    ( someFunc,
    maxInt,
    fib
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxInt:: Int->Int->Int
maxInt x y 
    | x < y = y
    | otherwise = x


fib :: Int -> Int 
fib 0 = 1 
fib 1 = 1 
fib n =  (fib (n -1))+ (fib (n-2)) 
