module Connection where


import Core
import Types
import Commands


import Control.Monad

import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import System.IO
import Network

import Control.Concurrent.MVar


newtype MPDConn = MPDConn (MVar (Either (IO ()) MPDConnState))


-- either2 :: (a -> c) -> (b -> d) -> Either a b -> Either c d
-- either2 f g = either (Left . f) (Right . g)


-- XXX handle some exceptions on this level?
withConnection :: MPDConn -> (MPDConnState -> IO a) -> IO (Result a)
withConnection (MPDConn c) act = do
    v <- takeMVar c
    r <- either (\_ -> return $ Left ErrIdling) (fmap Right . act) v
    putMVar c v
    return r


-- XXX or on this one?
getLines :: (T.Text -> Bool) -> MPDConn -> IO (Result [T.Text])
getLines p c = withConnection c $ collect . mpdConn
    where
        collect h = do
            t <- TE.decodeUtf8 `fmap` B.hGetLine h
            if p t then (t:) `fmap` collect h else return [t] 


getResponseBlock :: MPDConn -> IO (Result [T.Text])
getResponseBlock c =
    -- getLines ( \t -> not (ok `T.isPrefixOf` t || ack `T.isPrefixOf` t)) c >>= \ls ->
    getLines ( \t -> not (t == ok || ack `T.isPrefixOf` t)) c >>= \ls ->
        case ls of
             Right ls | ack `T.isPrefixOf` last ls -> return (Left (OtherError "ack."))
                      | otherwise                  -> return (return (init ls))
             Left  e                               -> return (Left e)

    where
        ok  = T.pack "OK"
        ack = T.pack "ACK "






quickConnect :: IO MPDConn
quickConnect = do
    h <- connectTo "localhost" (PortNumber 6600)
    c <- MPDConn `fmap` newMVar (Right $ zeroState { mpdConn = h } )
    getLines (const False) c
    return c

quickSend :: T.Text -> MPDConn -> IO (Result ())
quickSend t c = withConnection c $ \s -> B.hPutStrLn (mpdConn s) (TE.encodeUtf8 t) >> hFlush (mpdConn s)


quickCmd c@(MPDConn v) (Command t f) = do
    quickSend t c
    Right s <- readMVar v
    fmap (join . fmap (f s)) (getResponseBlock c)
    

