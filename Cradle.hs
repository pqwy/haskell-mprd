module Main where

import Connection
import Commands



main :: IO ()
main = do
    c <- quickConnect
    Right xn <- quickCmd c (listallinfo Nothing)
    print (length xn)


