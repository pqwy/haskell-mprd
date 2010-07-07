{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PackageImports  #-}

module Network.MPrD.Codec
    ( Decoder, Parameter(..)
    , joinParams, (<+>), bsToString, stringToBs

    , nullDecoder
    , decodeTrack, decodeTracks, decodePLTrack, decodePLTracks, decodeOutputs
    , decodePosIDs, decodeSongsPltime, decodeSingleTags, decodeURIs
    , decodeDirsFiles, decodeDirsTracks, decodeTagTypes, decodeURLHandlers
    , decodeCommands, decodePlaylists, decodeSticker, decodeStickers, decodeStickersFiles
    , decodeStats, decodeStatus, decodeSubsysChanged, decodeTID, decodeJID

    , isOK, isAck, isListOK, readAck
    ) where

import Network.MPrD.Tags
import Network.MPrD.Types
import Network.MPrD.Misc

import Data.Maybe
import Control.Monad
import Control.Applicative
import "mtl" Control.Monad.State

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

instance Parameter MetaField where encode = metafieldToBytestring

instance Parameter Range where
    encode (a :/: b) = B.intercalate ":" [encode a, encode b]

instance Parameter [QueryPred] where
    encode = joinParams . map (\(t, v) -> encode t <+> encode v)

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

-- The story is this: a Decoder needs its list of lines to parse, but since Commands
-- compose as they do, from outside, it's impossible to map a particular input block
-- to its command's corresponding decoder (from outside, simple commands look like
-- composite ones). So decoders have to feed each other input blocks in sequence.
-- 
-- Decoders essentially form YET ANOTHER state monad, this one being the base for the
-- Applicative instance of Commands.
type Decoder a = [[ByteString]] -> Result (a, [[ByteString]])

nullDecoder :: a -> Decoder a
nullDecoder = export . pure


-- ...while the parsers are a good ole state/exception mon'. CPSing doesnt improve speed
-- noticably. Using Parsec pulls in a needless dependency AND is around 35% slower.
newtype Parser a = P { nP :: Int -> [(ByteString, ByteString)]
                         -> Either (Int, String) (a, Int, [(ByteString, ByteString)]) }


export :: Parser a -> Decoder a
export _     []       = Left (DecodeError 0 "decoder expecting an input chunk")
export (P p) (bs:bss) = splitKeys bs >>= \bs' ->
                    case p 0 bs' of
                         Right (x, _, _) -> Right (x, bss)
                         Left  (l, e)    -> Left (DecodeError l e)

splitKeys :: [ByteString] -> Result [(ByteString, ByteString)]
splitKeys = mapM $ \b ->
    case B.breakSubstring ": " b of
         (k, v) | B.length v >= 2 -> return (k, B.drop 2 v)
                | otherwise       -> Left (DecodeError 0 "malformed key-value pair")


-- And yes, compositional parsers are *much* easier to work with than ad-hoc parsing
-- operations.
instance Monad Parser where
    return x  = P (\ !l s -> Right (x, l, s))
    P p >>= f = P (\ !l s -> either Left (\(a, !l', s') -> nP (f a) l' s') (p l s))
    fail s = P (\ !l _ -> Left (l, s))

instance Functor Parser where fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    empty = fail ""
    P a <|> P b = P (\ !l s -> either (\_ -> b l s) Right (a l s))


line :: Parser (ByteString, ByteString)
line = P $ \ !l s -> case s of
                        []     -> Left  (l, "eof")
                        (x:xs) -> Right (x, l+1, xs)

satKey :: (ByteString -> Bool) -> Parser (ByteString, ByteString)
satKey p = line >>= \x@(k, _) ->
            if p k then pure x
                   else fail ("unexpected key " ++ B.unpack k)

rawKey :: ByteString -> Parser ByteString
rawKey k = snd <$> satKey (== k)

key :: (Field a) => ByteString -> Parser a
key = rawKey >=> field


-- For overloading the "key". Convenient.
class Field a where field :: ByteString -> Parser a

instance Field Int where
    field b = case B.readInt b of
                   Just (x, y) | B.null y -> pure x
                   _                      -> fail (B.unpack b)

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
parseTags = mkTags <$> many ( tuple <$> satKey tagKey )
    where
        tagKey       = not . (`elem` ["file", "Pos", "directory"])
        tuple (k, v) = (bytestringToMetafield k, E.decodeUtf8 v)

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
decodeTagTypes = export $ many (bytestringToMetafield <$> rawKey "tagtype")

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
        state x       = fail (B.unpack x ++ "?")

        nTuple n x | length xn == n = mapM field xn
                   | otherwise      = fail (show n ++ "-place list")
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
-- If we can't manage an ack all bets are probably off, but let's leave
-- that to the client.
-- readAck = maybe (Left $ OtherError "can't parse ack") Right . parseAck
readAck = maybe (Left $ OtherError "can't parse ack") Right . (parseAck' `evalStateT`)

-- Yes. It's ugly. But lightweight.
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


edit :: (ByteString -> Maybe (a, ByteString)) -> StateT ByteString Maybe a
edit f = get >>= lift . f >>= \(a, b) -> put b >> return a

parseAck' :: StateT ByteString Maybe Ack
parseAck' = do
    get >>= guard . ("ACK [" `B.isPrefixOf`)
    err <- int2AckErr <$> edit (B.readInt . B.drop 5)
    pos <- edit B.uncons >> edit B.readInt
    cmd <- modify (snd . B.break (== '{')) >> edit B.uncons >>
            edit (Just . B.break (== '}')) >>= \c -> return $
                if B.null c then Nothing else Just (B.unpack c)
    rest <- edit B.uncons >> gets (dropWhile (== ' ') . B.unpack) 

    return Ack { ackError = err, ackPosition = pos
               , ackCommand = cmd, ackDescription = rest }

-- }}}

-- vim:set fdm=marker:
