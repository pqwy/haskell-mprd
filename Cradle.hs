module Main where

import Network.MPDMPD.Connection
import Network.MPDMPD.Commands

import System.Environment


main1, main2 :: IO ()

main1 = do
    Right mpd <- attachFile "torture1.txt"
    res <- cmd mpd (listall Nothing)
    print (length `fmap` res)

main2 = do
    Right mpd <- attachFile "torture3.txt"
    res <- cmd mpd (listallinfo Nothing)
    print (length `fmap` res)

-- main1 :: IO ()
-- main1 = do
--     Right xn <- quickRead "torture1.txt" (listall Nothing)
--     print (length xn)
-- 
-- main2 = do
--     Right xn <- quickRead "torture2.txt" (listallinfo Nothing)
--     print (length xn)

main = main2
