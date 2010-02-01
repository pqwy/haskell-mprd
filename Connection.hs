{-# LANGUAGE OverloadedStrings  #-}

module Connection
    ( MPDConn
    , connect, connectEx, cmd, cmds, cmds_
    , kill, close
    ) where


import Core
import Types
import Commands
import Codec ( Decoder, isOK, isListOK, isAck, readAck )


import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T ( pack )

import System.IO
import System.IO.Error hiding ( try, catch )
import Network

import Control.Concurrent.MVar

import Control.Exception
import GHC.IOBase ( IOErrorType(..), IOException(..) )



type MPDVar = MVar (Either (IO ()) Handle)

newtype MPDConn = MPDConn MPDVar

instance Show MPDConn where
    show _ = "<MPD connection>"




connect :: IO (Result MPDConn)
connect = connectEx "localhost" 6600 Nothing


connectEx :: HostName -> Int -> Maybe String -> IO (Result MPDConn)
connectEx host port pass = do
    r <- try (connectTo host (PortNumber (fromIntegral port)))
    case r of
         Right h -> prepareHandle h >>= \mpd ->
                        case mpd of
                             e@(Left _) -> hClose h >> return e
                             _          -> return mpd

         Left e | isDoesNotExistError e -> return (Left DaemonNotResponding)
                | otherwise             -> throwIO e

    where prepareHandle h =
              -- hSetBuffering h LineBuffering
              validateProto <$> B.hGetLine h >>= \proto' ->
                  case proto' of
                       Nothing    -> return (Left UnknownProtoResponse)
                       Just proto -> MPDConn <$> newMVar (Right h) >>= \mpd ->
                            case pass of
                                 Nothing -> return (Right mpd)
                                 Just p  -> (mpd <$) <$> cmd mpd (password p)


validateProto :: ByteString -> Maybe String
validateProto s | "OK MPD " `B.isPrefixOf` s = Just (B.unpack $ B.drop 7 s)
                | otherwise                  = Nothing





takeConnectionEx :: (MPDVar -> Handle -> IO (Result a)) -> MPDConn -> IO (Result a)
takeConnectionEx act c@(MPDConn var) = do
    x <- takeMVar var
    ( case x of
           Left _  -> return (Left ConnLocked)
           -- Right h -> join <$> tryJust mapExceptions (act var h)
           Right h -> catchJust mapExceptions
                        (act var h)
                        (\e -> hClose h >> return (Left e))
        ) `finally` putMVar var x


mapExceptions :: IOException -> Maybe MPDError
mapExceptions e =
    -- In search of a System.IO.Error.isResourceVanished...
    case ioe_type e of
         EOF              -> Just DaemonGone
         ResourceVanished -> Just DaemonGone
         -- This is raised on closed handle. Do we simply map it to
         -- DaemonGone and simplify exceptions, or differentiate between
         -- first occurences of errors and closed handles..?
         IllegalOperation -> Just DaemonGone
         _                -> Nothing


usingHandle :: (Handle -> IO (Result a)) -> MPDConn -> IO (Result a)
usingHandle = takeConnectionEx . const




-- XXX might be a choking point...
getResponseBlock :: Handle -> IO (Result ([ByteString], Maybe Ack))
getResponseBlock h =
    collect $ \a b ->
                case b of
                     Nothing -> return (return (a, Nothing))
                     Just e  -> return ( readAck e >>= \e' ->
                                            return (a, Just e') )
    where
        collect k = B.hGetLine h >>= \l ->
                        case () of
                             _ | isOK l    -> k [] Nothing
                               | isAck l   -> k [] (Just l)
                               | otherwise -> collect $ \a b -> k (l:a) b


getResponseBlock' :: Handle -> IO (Result ([ByteString], Maybe Ack))
getResponseBlock' h = B.hGetLine h >>= \l ->
        case () of
             _ | isOK l    -> return (return ([], Nothing))
               | isAck l   -> return (readAck l >>= \a -> return ([], Just a))
               | otherwise -> (first (l:) <$>) <$> getResponseBlock' h



readResponse :: Handle -> Decoder a -> IO (Result a)
readResponse h dec = getResponseBlock h >>= \res ->
        return $ res >>= \lines ->
                    case lines of
                         (bs, Nothing) -> dec bs
                         (_,  Just a)  -> Left (AckError a)


cmd :: MPDConn -> Command a -> IO (Result a)
cmd c (Command txt dec) = flip usingHandle c $ \h ->
            -- Fancy that. The place where output is checked for newlines and flushed
            -- if the handle is LineBuffered is only the standard hPutChar. hPutStrLn
            -- does hPutStr s >> hPutChar '\n', thus triggering the flush. Nothing
            -- of that sort happens in bytestring's backend's code path, hPutBuf,
            -- of course.
            B.hPutStrLn h txt >> hFlush h >> readResponse h dec


readResponses :: Handle -> [Decoder a] -> IO (Result ([a], Maybe Ack))
readResponses h decs = getResponseBlock h >>= \res ->
        return $ res >>= \(lines, mack) ->
            (\l -> (l, mack)) <$>
                sequence (zipWith ($) decs (chunkBy isListOK lines))


cmds :: MPDConn -> [Command a] -> IO (Result ([a], Maybe Ack))
cmds c cs = flip usingHandle c $ \h -> do

        B.hPutStrLn h "command_list_ok_begin"
        mapM_ (B.hPutStrLn h) cmdTexts
        B.hPutStrLn h "command_list_end"
        hFlush h

        readResponses h cmdDecs

    where cmdTexts = map (\(Command t _) -> t) cs
          cmdDecs  = map (\(Command _ d) -> d) cs

cmds_ :: MPDConn -> [Command a] -> IO (Result ())
cmds_ c cs = (() <$) <$> cmds c cs



chunkBy f [] = []
chunkby f xs = go xs (\a b -> a : chunkBy f b)
    where
        go [] k                 = k [] []
        go (x:xs) k | f x       = k [] xs
                    | otherwise = go xs (k . (x:))

chunkBy' f [] = []
chunkBy' f xs =
    case f `break` xs of
         (a, [])  -> [a]
         (a, _:b) -> chunkBy' f b

-- idle :: MPDConn -> IO (Result [SubsysChanged])
-- idle = usingHandle $ \h ->
--         B.hPutStrLn h x >> (readResponse h dec
--                             `finally` B.hPutStrLn h "noidle" )
-- 
-- idle :: MPDConn -> IO (Result [SubsysChanged])
-- idle = takeConnectionEx $ \var h -> do
--         B.hPutStrLn h "idle"
--         putMVar var (return ())
--         readResponse h d
--             `finally` takeMVar var
-- 
--     where Command x d = unsafeIdle


killingCmd :: Command () -> MPDConn -> IO (Result ())
killingCmd c cn = cmd cn c >>= \r ->
    case r of
         Left DaemonGone -> return (return ())
         x               -> return x


close, kill :: MPDConn -> IO (Result ())

close = killingCmd unsafeClose
kill = killingCmd unsafeKill




first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

