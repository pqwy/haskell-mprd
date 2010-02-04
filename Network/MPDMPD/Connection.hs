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



type MPDVar = MVar (Maybe (Handle, Handle))

data MPDConn = MPDConn String MPDVar

instance Show MPDConn where
    show (MPDConn v _) = "<MPD connection, \"" ++ v ++ "\">"


protocolVersion :: MPDConn -> String
protocolVersion (MPDConn v _) = v


attachFile :: FilePath -> IO (Result MPDConn)
attachFile f = do
    hs <- (,) <$> openFile f ReadMode
              <*> openFile "/dev/null" WriteMode
    Right . (MPDConn "") <$> newMVar (Just hs)



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
                   Just proto ->
                       MPDConn proto <$> newMVar (Just (h, h)) >>= \mpd ->
                       case pass of
                            Nothing -> return (Right mpd)
                            Just p  -> (mpd <$) <$> cmd mpd (password p)


validateProto :: ByteString -> Maybe String
validateProto s | "OK MPD " `B.isPrefixOf` s = Just (B.unpack $ B.drop 7 s)
                | otherwise                  = Nothing



takeConnection :: ((Handle, Handle) -> IO (Result a)) -> MPDConn -> IO (Result a)
takeConnection act (MPDConn _ var) =
    takeMVar var >>= \x ->
        case x of
             Nothing          -> return (Left ConnLocked)
             Just hs@(h1, h2) ->
                 catchJust mapExceptions
                           (act hs)
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



cmds :: MPDConn -> [Command a] -> IO (Result [a])
cmds mpd = cmd mpd . sequenceA

cmd :: MPDConn -> Command a -> IO (Result a)
cmd mpd c = flip takeConnection mpd $ \h ->
            case c of
                 Command txt dec   -> commSingleCommand h txt dec
                 Commands txtf dec -> commMultiCommands h txtf dec
             

commSingleCommand :: (Handle, Handle) -> ByteString -> Decoder a -> IO (Result a)
commSingleCommand (hR, hW) txt dec =
    B.hPutStrLn hW txt >> hFlush hW >> readResponseWith (:[]) hR dec


commMultiCommands :: (Handle, Handle)
                  -> ([ByteString] -> [ByteString])
                  -> Decoder a -> IO (Result a)
commMultiCommands (hR, hW) txtf dec = do
    B.hPutStrLn hW "command_list_ok_begin"
    mapM_ (B.hPutStrLn hW) (txtf [])
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


-- CPS'd version is kinda fastest but the worst memory hog too.
getResponseBlock :: Handle -> IO (Result ([ByteString], Maybe Ack))
getResponseBlock h = collect $ \a b ->
                case b of
                     Nothing -> return (return (a, Nothing))
                     Just e  -> return ( readAck e >>= \e' ->
                                            return (a, Just e') )
    where
        collect k = B.hGetLine h >>= \l ->
                        case () of
                             _ | isOK l    -> k [] Nothing
                               | isAck l   -> k [] (Just l)
                               | otherwise -> collect (k . (l:))

-- getResponseBlock h = B.hGetLine h >>= \l ->
--         case () of
--              _ | isOK l    -> return (return ([], Nothing))
--                | isAck l   -> return (readAck l >>= \a -> return ([], Just a))
--                | otherwise -> (first (l:) <$>) <$> getResponseBlock h


-- Yup. CPS'd fastest.
chunkBy :: (Show a) => (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy f l = go l (\a b -> a : chunkBy f b)
    where
        go [] k                 = k [] []
        go (x:xs) k | f x       = k [] xs
                    | otherwise = go xs (k . (x:))



killingCmd :: Command () -> MPDConn -> IO (Result ())
killingCmd c cn = cmd cn c >>= \r ->
    case r of
         Left DaemonGone -> return (return ())
         x               -> return x

close, kill :: MPDConn -> IO (Result ())

close = killingCmd unsafeClose
kill  = killingCmd unsafeKill


