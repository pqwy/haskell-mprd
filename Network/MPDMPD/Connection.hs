{-# LANGUAGE OverloadedStrings  #-}

module Network.MPDMPD.Connection
    ( MPDConn
    , protocolVersion
    , connect, connectEx, cmd, cmds
    , kill, close
    , attachFile
    ) where


import Network.MPDMPD.Types
import Network.MPDMPD.Commands
import Network.MPDMPD.Codec ( Decoder, isOK, isListOK, isAck, readAck )


import Control.Monad
import Control.Applicative
import Data.Traversable

import qualified Data.ByteString.Char8 as B

import System.IO
import System.IO.Error hiding ( try, catch )
import Network

import Control.Concurrent.MVar

import Control.Exception
-- Gah. Lack of IOException predicates, that's all.
import GHC.IOBase ( IOErrorType(..), IOException(..) )



type MPDVar = MVar (Either (IO ()) (Handle, Handle))

data MPDConn = MPDConn String MPDVar

instance Show MPDConn where
    show (MPDConn v _) = "<MPD connection, \"" ++ v ++ "\">"


protocolVersion :: MPDConn -> String
protocolVersion (MPDConn v _) = v


attachFile :: FilePath -> IO (Result MPDConn)
attachFile f = do
    hs <- (,) <$> openFile f ReadMode
              <*> openFile "/dev/null" WriteMode
    Right . (MPDConn "") <$> newMVar (Right hs)



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

    where prepareHandle h = do
              hSetBuffering h (BlockBuffering Nothing)
              proto' <- validateProto <$> B.hGetLine h
              case proto' of
                   Nothing    -> return (Left UnknownProtoResponse)
                   Just proto -> MPDConn proto <$> newMVar (Right (h, h)) >>= \mpd ->
                       case pass of
                            Nothing -> return (Right mpd)
                            Just p  -> (mpd <$) <$> cmd mpd (password p)


validateProto :: ByteString -> Maybe String
validateProto s | "OK MPD " `B.isPrefixOf` s = Just (B.unpack $ B.drop 7 s)
                | otherwise                  = Nothing


takeConnectionEx :: (MPDVar -> (Handle, Handle) -> IO (Result a)) -> MPDConn -> IO (Result a)
takeConnectionEx act (MPDConn _ var) =
    takeMVar var >>= \x ->
        case x of
             Left _            -> return (Left ConnLocked)
             Right hs@(h1, h2) ->
                 catchJust mapExceptions (act var hs)
                    (\e -> hClose h1 >> hClose h2 >> return (Left e))
        `finally` putMVar var x

mapExceptions :: IOException -> Maybe MPDError
mapExceptions e =
    -- In search of a System.IO.Error.isResourceVanished...
    case ioe_type e of
         EOF              -> Just DaemonGone
         ResourceVanished -> Just DaemonGone
         -- This is raised on closed handle. Do we simply map it to
         -- DaemonGone and simplify exceptions, or differentiate between
         -- first occurences of errors and already closed handles..?
         IllegalOperation -> Just DaemonGone
         _                -> Nothing



usingHandle :: ((Handle, Handle) -> IO (Result a)) -> MPDConn -> IO (Result a)
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


-- getResponseBlock' :: Handle -> IO (Result ([ByteString], Maybe Ack))
-- getResponseBlock' h = B.hGetLine h >>= \l ->
--         case () of
--              _ | isOK l    -> return (return ([], Nothing))
--                | isAck l   -> return (readAck l >>= \a -> return ([], Just a))
--                | otherwise -> (first (l:) <$>) <$> getResponseBlock' h


cmd :: MPDConn -> Command a -> IO (Result a)
cmd mpd c = flip usingHandle mpd $ \h ->
            case c of
                 Command txt dec   -> commSingleCommand h txt dec
                 Commands txtn dec -> commMultiCommands h txtn dec
             

commSingleCommand :: (Handle, Handle) -> ByteString -> Decoder a -> IO (Result a)
commSingleCommand h@(hR, hW) txt dec =
    B.hPutStrLn hW txt >> hFlush hW >> readResponseWith (:[]) hR dec


commMultiCommands :: (Handle, Handle) -> [ByteString] -> Decoder a -> IO (Result a)
commMultiCommands (hR, hW) txt dec = do
    B.hPutStrLn hW "command_list_ok_begin"
    mapM_ (B.hPutStrLn hW) txt
    B.hPutStrLn hW "command_list_end"
    hFlush hW
    readResponseWith (chunkBy isListOK) hR dec


readResponseWith :: ([ByteString] -> [[ByteString]])
                 -> Handle -> Decoder a -> IO (Result a)
readResponseWith f h dec = getResponseBlock h >>= \res ->
        return $ res >>= \(lns, mack) ->
                    case mack of
                         Nothing -> fst <$> dec (f lns)
                         Just a  -> Left (AckError a)


cmds :: MPDConn -> [Command a] -> IO (Result [a])
cmds mpd = cmd mpd . sequenceA


chunkBy :: (Show a) => (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy f l = go l (\a b -> a : chunkBy f b)
    where
        go [] k                 = k [] []
        go (x:xs) k | f x       = k [] xs
                    | otherwise = go xs (k . (x:))


-- chunkBy' f [] = []
-- chunkBy' f xs =
--     case f `break` xs of
--          (a, [])  -> [a]
--          (a, _:b) -> chunkBy' f b

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

