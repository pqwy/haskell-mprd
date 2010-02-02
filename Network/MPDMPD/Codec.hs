{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
{-# LANGUAGE BangPatterns  #-}

module Network.MPDMPD.Codec
    ( Decoder, Parameter(..)
    , joinParams, (<+>), bsToString, stringToBs

    , decodeTrack, decodeTracks, decodePLTrack, decodePLTracks, decodeOutputs
    , decodePosIDs, decodeSongsPltime, decodeSingleTags, decodeURIs
    , decodeDirsFiles, decodeDirsTracks, decodeTagTypes, decodeURLHandlers
    , decodeCommands, decodePlaylists, decodeSticker, decodeStickers, decodeStickersFiles
    , decodeStats, decodeStatus, decodeSubsysChanged, decodeTID, decodeJID

    , isOK, isAck, isListOK, readAck
    ) where

import Network.MPDMPD.Types

import Data.Maybe
import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

-- co {{{

class Parameter a where encode :: a -> ByteString

instance (Parameter a) => Parameter (Maybe a) where
    encode Nothing  = ""
    encode (Just x) = encode x

instance Parameter String where encode = stringToBs

instance Parameter Bool where
    encode True  = "1"
    encode False = "0"
    
instance Parameter Int where encode = simpleEncode

instance Parameter Text where encode = E.encodeUtf8 . quotes

instance Parameter Range where
    encode (a :/: b) = B.intercalate ":" [encode a, encode b]

instance Parameter [QueryPred] where
    encode = joinParams . map (\(MF t, v) -> t <+> encode v)

instance Parameter [SubsysChanged] where
    encode = joinParams . map encodeSubsysChanged

instance Parameter Playlist where encode (Playlist p) = encode p

instance Parameter URI where encode (URI u) = encode u


joinParams :: [ByteString] -> ByteString
joinParams = B.intercalate " "

-- XXX SPEED
(<+>) :: ByteString -> ByteString -> ByteString
a <+> b = a `B.append` (' ' `B.cons` b)

quotes :: Text -> Text
quotes = T.cons '"' . (`T.snoc` '"')

simpleEncode :: (Show a) => a -> ByteString
simpleEncode = B.pack . show

-- XXX fugly....
stringToBs :: String -> ByteString
stringToBs = E.encodeUtf8 . T.pack

encodeSubsysChanged :: SubsysChanged -> ByteString
encodeSubsysChanged = fromMaybe (error "Codec: inexhaustive subSysMap.")
                        . (`lookup` map (\(a, b) -> (b, a)) subSysMap)
-- }}}

-- dec {{{

type Decoder a = [ByteString] -> Result a


newtype Parser a = P { nP :: Int -> [(ByteString, ByteString)]
                         -> Either (Int, String) (a, Int, [(ByteString, ByteString)]) }

export :: Parser a -> Decoder a
export (P p) bs = mapM splitKey bs >>= \bss ->
                    case p 0 bss of
                         Right (x, _, _) -> return x
                         Left  (l, e)    -> Left (DecodeError l e)
    where
        splitKey b =
            case B.breakSubstring ": " b of
                 (k, v) | B.length v >= 2 -> return (k, B.drop 2 v)
                        | otherwise -> Left (DecodeError 0 "malformed key-value pair")


instance Monad Parser where
    return x  = P (\ !l s -> Right (x, l, s))
    P p >>= f = P (\ !l s -> either Left (\(a, !l', s') -> nP (f a) l' s') (p l s))

instance Functor Parser where fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    empty = parseError ""
    P a <|> P b = P (\ !l s -> either (\_ -> b l s) Right (a l s))


parseError :: String -> Parser a
parseError s = P (\ !l _ -> Left (l, s))

line :: Parser (ByteString, ByteString)
line = P $ \ !l s -> case s of
                        []     -> Left  (l, "eof")
                        (x:xs) -> Right (x, l+1, xs)

satKey :: (ByteString -> Bool) -> Parser (ByteString, ByteString)
satKey p = line >>= \x@(k, _) ->
            if p k then pure x
                   else parseError ("unexpected key " ++ B.unpack k)

rawKey :: ByteString -> Parser ByteString
rawKey k = snd <$> satKey (== k)

key :: (Field a) => ByteString -> Parser a
key = rawKey >=> field



class Field a where field :: ByteString -> Parser a

instance Field Int where
    field b = case B.readInt b of
                   Just (x, y) | B.null y -> pure x
                   _                      -> parseError (B.unpack b)

instance Field Bool   where field = fmap (/= (0 :: Int)) . field

instance Field Text   where field = pure . E.decodeUtf8

instance Field URI    where field = fmap URI . field

instance Field String where field = pure . bsToString

-- XXX fugly, but avoids utf8-string...
bsToString :: ByteString -> String
bsToString = T.unpack . E.decodeUtf8



parseTrack :: Parser Track
parseTrack = mkTrack <$> key "file"
                     <*> ((Just <$> key "Time") <|> pure Nothing)
                     <*> parseTags
    where
        mkTrack f t ts = zeroTrack { trackFile = f, trackTime = t, trackTags = ts } 

parseTags :: Parser Tags
parseTags = mkTags <$> many ( (\(k, v) -> (MF k, E.decodeUtf8 v)) <$> satKey tagKey )
    where
        tagKey = not . (`elem` ["file", "Pos", "directory"])

decodeTrack :: Decoder Track
decodeTrack = export parseTrack

decodeTracks :: Decoder [Track]
decodeTracks = export $ many parseTrack


parsePLTrack :: Parser PlaylistTrack
parsePLTrack = mkPLT <$> parseTrack <*> key "Pos" <*> (TID <$> key "Id")
    where
        mkPLT trk pos tid =
            PlaylistTrack { plTrack = trk, plTrackPos = pos, plTrackId = tid }

decodePLTrack :: Decoder PlaylistTrack
decodePLTrack = export parsePLTrack

decodePLTracks :: Decoder [PlaylistTrack]
decodePLTracks = export $ many parsePLTrack


decodeOutputs :: Decoder [Output]
decodeOutputs = export $ many $ mkOut <$> (OutputID <$> key "outputid")
                                      <*> key "outputname"
                                      <*> key "outputenabled"
    where mkOut tid nm en =
              Output { outputID = tid, outputName = nm, outputEnabled = en }


decodePosIDs :: Decoder [(PlaylistPos, TrackID)]
decodePosIDs = export $
    many ( (,) <$> key "cpos" <*> (TID <$> key "Id") )

decodeSongsPltime :: Decoder (Int, Seconds)
decodeSongsPltime = export $ (,) <$> key "songs" <*> key "playtime"

decodeSingleTags :: ByteString -> Decoder [MetaContent]
decodeSingleTags = export . many . key

decodeURIs :: Decoder [URI]
decodeURIs = export $ many (key "file")

decodeDirsFiles :: Decoder [Either URI URI]
decodeDirsFiles = export $
    many ( Left <$> key "directory" <|> Right <$> key "file" )

decodeDirsTracks :: Decoder [Either URI Track]
decodeDirsTracks = export $
    many ( Left <$> key "directory" <|> Right <$> parseTrack )

decodeTagTypes :: Decoder [MetaField]
decodeTagTypes = export $ many (MF <$> rawKey "tagtype")

decodeURLHandlers :: Decoder [String]
decodeURLHandlers = export $ many (key "handler")

decodeCommands :: Decoder [String]
decodeCommands = export $ many (key "command")

-- XXX last-modified. proper time?
decodePlaylists :: Decoder [(Playlist, Text)]
decodePlaylists = export $
    many ((,) <$> (Playlist <$> key "playlist") <*> key "Last-Modified")


parseSticker :: Parser (Text, Text)
parseSticker = split <$> rawKey "sticker"
    where
        split x = let (a, b) = (== '=') `B.break` x
                  in (E.decodeUtf8 a, E.decodeUtf8 (B.drop 1 b))

decodeSticker :: Decoder Text
decodeSticker = export $ snd <$> parseSticker

decodeStickers :: Decoder [(Text, Text)]
decodeStickers = export $ many parseSticker

decodeStickersFiles :: Decoder [(URI, Text)]
decodeStickersFiles = export $
    many ((\u (_, v) -> (u, v)) <$> key "file" <*> parseSticker)

decodeTID :: Decoder TrackID
decodeTID = export (TID <$> key "Id")

decodeJID :: Decoder JobID
decodeJID = export (JID <$> (key "updating_db"))


concatFs :: [a -> a] -> a -> a
concatFs = foldr (.) id

decodeStats :: Decoder Stats
decodeStats = export $
    ( ($ zeroStats) . concatFs ) <$>
    ( many $ line >>= \(k, v) ->
        case k of
             "artists"     -> (\x s -> s { stsArtists    = x}) <$> field v
             "albums"      -> (\x s -> s { stsAlbums     = x}) <$> field v
             "songs"       -> (\x s -> s { stsSongs      = x}) <$> field v
             "uptime"      -> (\x s -> s { stsUptime     = x}) <$> field v
             "playtime"    -> (\x s -> s { stsPlaytime   = x}) <$> field v
             "db_playtime" -> (\x s -> s { stsDbPlaytime = x}) <$> field v
             "db_update"   -> (\x s -> s { stsDbUpdate   = x}) <$> field v
             _             -> pure id  )


decodeStatus :: Decoder Status
decodeStatus = export $
    ( ($ zeroStatus) . concatFs ) <$>
    ( many $ line >>= \(k, v) ->
        case k of
             "volume"        -> (\x s -> s { stVolume     = plVol x      }) <$> field v
             "repeat"        -> (\x s -> s { stRepeat     = x            }) <$> field v
             "random"        -> (\x s -> s { stRandom     = x            }) <$> field v
             "single"        -> (\x s -> s { stSingle     = x            }) <$> field v
             "consume"       -> (\x s -> s { stConsume    = x            }) <$> field v
             "playlist"      -> (\x s -> s { stPLVer      = PLV x        }) <$> field v
             "playlistlength"-> (\x s -> s { stPLLen      = x            }) <$> field v
             "xfade"         -> (\x s -> s { stXfade      = x            }) <$> field v
             "state"         -> (\x s -> s { stState      = x            }) <$> state v
             "song"          -> (\x s -> s { stSong       = x            }) <$> field v
             "songid"        -> (\x s -> s { stSongId     = TID x        }) <$> field v
             "bitrate"       -> (\x s -> s { stBitrate    = x            }) <$> field v
             "nextsong"      -> (\x s -> s { stNextsong   = x            }) <$> field v
             "nextsongid"    -> (\x s -> s { stNextsongId = TID x        }) <$> field v
             "updating_db"   -> (\x s -> s { stUpdatingDB = Just (JID x) }) <$> field v
             "time"      -> (\[a,b] s -> s { stTime       = (a, b)       }) <$> nTuple 2 v
             "audio"   -> (\[a,b,c] s -> s { stAudio      = (a, b, c)    }) <$> nTuple 3 v
             _               -> pure id  )
    where
        plVol x | x < 0     = Nothing
                | otherwise = Just x

        state "play"  = pure StatePlay
        state "stop"  = pure StateStop
        state "pause" = pure StatePause
        state x       = parseError (B.unpack x ++ "?")

        nTuple n x | length xn == n = mapM field xn
                   | otherwise      = parseError (show n ++ "-place list")
            where xn = B.split ':' x


decodeSubsysChanged :: Decoder [SubsysChanged]
decodeSubsysChanged = export $
    ( ($ []) . concatFs ) <$>
        many (rawKey "changed" >>=
                pure . maybe id (:) . (`lookup` subSysMap))


subSysMap :: [(ByteString, SubsysChanged)]
subSysMap = [ ("database"        , ChangedDatabase)
            , ("mixer"           , ChangedMixer)
            , ("options"         , ChangedOptions)
            , ("output"          , ChangedOutput)
            , ("player"          , ChangedPlayer)
            , ("playlist"        , ChangedPlaylist)
            , ("stored_playlist" , ChangedStoredPlaylist)
            , ("update"          , ChangedUpdate) ]
-- }}}

-- that _other_ dec {{{

isOK, isAck, isListOK :: ByteString -> Bool
isOK     = ( == "OK" )
isAck    = ( "ACK " `B.isPrefixOf` )
isListOK = ( == "list_OK" )


readAck :: ByteString -> Result Ack
readAck = maybe (Left $ OtherError "can't parse ack") Right . parseAck

-- Yes. It's ugly.
parseAck :: ByteString -> Maybe Ack
parseAck s = do
    guard ("ACK [" `B.isPrefixOf` s)
    (err, s1) <- first int2AckErr <$> B.readInt (B.drop 5 s)
    (pos, s2) <- (snd <$> B.uncons s1) >>= B.readInt
    (cmd, s3) <- (B.break (== '}') . snd)
                    <$> B.uncons (snd $ B.break (== '{') s2)
    let errCmd | B.null cmd = Nothing
               | otherwise  = Just (B.unpack cmd)
    rest      <- (dropWhile (== ' ') . B.unpack . snd)
                    <$> B.uncons s3

    return Ack { ackError = err, ackPosition = pos
               , ackCommand = errCmd, ackDescription = rest }


first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
-- }}}

-- vim:set fdm=marker:
