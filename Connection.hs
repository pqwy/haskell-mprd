{-# LANGUAGE OverloadedStrings  #-}

module Connection where


import Core
import Types
import Commands


import Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T ( pack )

import System.IO
import Network

import Control.Concurrent.MVar


newtype MPDConn = MPDConn (MVar (Either (IO ()) MPDConnState))



-- XXX handle some exceptions on this level?
withConnection :: MPDConn -> (MPDConnState -> IO a) -> IO (Result a)
withConnection (MPDConn c) act = do
    v <- takeMVar c
    r <- either (\_ -> return $ Left ErrIdling) (fmap Right . act) v
    putMVar c v
    return r


-- XXX or on this one?
getLines :: (ByteString -> Bool) -> MPDConn -> IO (Result [ByteString])
getLines p c = withConnection c $ collect . mpdConn
    where
        collect h = do
            t <- B.hGetLine h
            if p t then (t:) `fmap` collect h else return [t] 


getResponseBlock :: MPDConn -> IO (Result [ByteString])
getResponseBlock c =
    getLines (not . liftM2 (||) isOK isAck) c >>= \ls ->
        case ls of
             Right ls | isAck (last ls) -> return (Left (OtherError "ack."))
                      | otherwise       -> return (return (init ls))
             Left  e                    -> return (Left e)

    where
        isOK = (== "OK")
        isAck = B.isPrefixOf "ACK "






quickConnect :: IO MPDConn
quickConnect = do
    h <- connectTo "localhost" (PortNumber 6600)
    c <- MPDConn `fmap` newMVar (Right $ zeroState { mpdConn = h } )
    getLines (const False) c
    return c


quickRead :: FilePath -> Command a -> IO (Result a)
quickRead f (Command _ fn) = do
    h <- openFile f ReadMode
    c <- MPDConn `fmap` newMVar (Right $ zeroState { mpdConn = h })
    (>>= fn zeroState) `fmap` getResponseBlock c


quickSend :: ByteString -> MPDConn -> IO (Result ())
quickSend b c = withConnection c $ \s ->
    B.hPutStrLn (mpdConn s) b >> hFlush (mpdConn s)


quickCmd c@(MPDConn v) (Command t f) = do
    quickSend t c
    Right s <- readMVar v
    fmap (join . fmap (f s)) (getResponseBlock c)
    

