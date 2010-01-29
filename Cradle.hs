module Main where

import Core
import Connection
import Commands

import System.Environment



main1 :: IO ()
main1 = do
    Right xn <- quickRead "torture1.txt" (listall Nothing)
    print (length xn)

main2 = do
    Right xn <- quickRead "torture2.txt" (listallinfo Nothing)
    print (length xn)

main = main2
