{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternGuards, FlexibleInstances, TypeSynonymInstances, OverlappingInstances  #-}

module Network.MPDMPD.Codec
    ( Decoder, Parameter(..), Response(..)
    , joinParams, (<+>)
    , bsToString, stringToBs

    , decodePosIDs, decodeSongsPltime, decodeSingleTags, decodeDirsFiles, decodeDirsTracks
    , decodeTagTypes, decodeURLHandlers, decodeCommands, decodeURIs, decodePlaylists
    , decodeSticker, decodeStickers, decodeStickersFiles

    , isAck, isOK, isListOK, readAck
    ) where


import Network.MPDMPD.Types


import Data.List ( sortBy )
import Data.Function ( on )
import qualified Data.Map as M

import Control.Monad
import Control.Applicative hiding ( Alternative(..), many )

import qualified Data.Text as T ( cons, snoc, pack, unpack )
import qualified Data.Text.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.ByteString.Char8 as B

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Error ( showErrorMessages, errorMessages )

import Text.Parsec.ByteString ()


-- co {{{

class Parameter a where encode :: a -> ByteString


instance (Parameter a) => Parameter (Maybe a) where
    encode Nothing  = ""
    encode (Just x) = encode x


instance Parameter String where encode = stringToBs

instance (Parameter a) => Parameter [a] where
    encode = joinParams . map encode

instance Parameter Bool where
    encode True  = "1"
    encode False = "0"
    
instance Parameter Int where encode = simpleEncode

instance Parameter TrackID where encode (TID x) = encode x

instance Parameter PlaylistVersion where encode (PLV x) = encode x

instance Parameter Text where encode = E.encodeUtf8 . quotes

instance Parameter Range where
    encode (a :/: b) = B.intercalate ":" [encode a, encode b]

instance Parameter URI where encode (URI u) = encode u

instance Parameter QueryPred where
    encode (MF t, v) = joinParams [t, encode v]

instance Parameter OutputID where encode (OutputID o) = encode o

instance Parameter SubsysChanged where encode = encodeChangedSubsys

instance Parameter Playlist where encode (Playlist p) = encode p


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

-- }}}

-- dec {{{

infixl 4 <$$>
(<$$>) :: (Functor f) => (a -> b) -> (c -> f a) -> c -> f b
(<$$>) = (.) . fmap


type Decoder a = [ByteString] -> Result a

type Parser = Parsec [ByteString] ()


-- class interface {{{

-- "Responses" are types whose parsers are unique. This class therefore exports
-- all the parsers that can be selected by the type they return alone.

class Response a where decode :: Decoder a

instance Response ()              where decode _ = return ()

instance Response Stats           where decode = asDecoder parseStats

instance Response Status          where decode = asDecoder parseStatus

instance Response [SubsysChanged] where decode = asDecoder parseChangedSubsys

instance Response Track           where decode = asDecoder parseTrack

instance Response [Track]         where decode = asDecoder parseTracks

instance Response PlaylistTrack   where decode = asDecoder parsePLTrack

instance Response [PlaylistTrack] where decode = asDecoder parsePLTracks

instance Response TrackID         where decode = asDecoder (key "Id")

instance Response JobID           where decode = asDecoder (key "updating_db")

instance Response [Output]        where decode = asDecoder parseOutputs

-- }}}

-- payloads {{{

-- A "payload" is a type that can be extracted from the rest of a line that
-- begins with a "key: ". Handy for the 'key "foo"' construct.

class Payload a where payload :: ByteString -> Parser a

instance Payload ByteString    where payload = return

instance Payload MetaField     where payload = return . MF

instance Payload Text          where payload = return . E.decodeUtf8

instance Payload String        where payload = return . bsToString

instance Payload TrackID       where payload = TID <$$> payload

instance Payload JobID         where payload = JID <$$> payload

instance Payload OutputID      where payload = OutputID <$$> payload

instance Payload Playlist      where payload = Playlist <$$> payload

instance Payload URI           where payload = URI <$$> payload

instance Payload SubsysChanged where payload = parseChangedSubsys1

instance Payload Int where
    payload b =
        case B.readInt b of
             Just (x, y) | B.null y -> return x
             _                      -> unexpected (B.unpack b)

instance Payload Bool where
    payload = fmap (/= 0) . (payload :: ByteString -> Parser Int)

instance Payload PlayState where
    payload x | x == "play"  = return StatePlay
              | x == "stop"  = return StateStop
              | x == "pause" = return StatePause
              | otherwise    = unexpected (B.unpack x)


-- }}}

-- prims {{{

asDecoder :: Parser a -> Decoder a
asDecoder p tx = either (Left . mapError) Right
                        (parse p "" tx)
    where
        mapError e = DecodeError
                        (sourceLine (errorPos e))
                        (showErrorMessages
                                "or" "unknown" "expecting"
                                "unexpected" "eof"
                                (errorMessages e))

posNextLine ::  SourcePos -> a -> b -> SourcePos
posNextLine s _ _ = incSourceLine s 1

splitKeyValue :: ByteString -> Maybe (ByteString, ByteString)
splitKeyValue t | (k, v) <- split t, B.length v >= 2 = Just (k, 2 `B.drop` v)
                | otherwise                          = Nothing
    where
        split = B.breakSubstring ": " -- <- B.break?

lineKV :: Parser (ByteString, ByteString)
lineKV = tokenPrim show posNextLine splitKeyValue
                <?> "'key: val' line"

satisfyKey :: (ByteString -> Bool) -> Parser (ByteString, ByteString)
satisfyKey p = try ( lineKV >>= \x@(k, _) ->
                        if p k then return x else mzero )
                <?> "some key..."

key :: (Payload a) => ByteString -> Parser a
key x = satisfyKey (== x) >>= payload . snd
                <?> ( "key '" ++ B.unpack x ++ "'" )

-- }}}

-- parsers (internal) {{{


-- XXX ugly, but avoids utf8-string...
bsToString :: ByteString -> String
bsToString = T.unpack . E.decodeUtf8

parseNTuple :: (Payload a) => Int -> ByteString -> Parser [a]
parseNTuple n =
    payload >=> mapM payload . B.split ':' >=> \l ->
        if length l == n then return l else fail (show n ++ "-place list")

parseTrack :: Parser Track
parseTrack =
    mkTrack <$> key "file"
            <*> ((Just <$> key "Time") <|> pure Nothing)
            <*> parseTags

    where
        mkTrack f tm tg =
            zeroTrack { trackFile = f, trackTime = tm, trackTags = tg } 
    
parseTags :: Parser Tags
parseTags = loop []
    where
        loop acc = ( satisfyKey tagKey >>= \(k, v) ->
                        loop ((MF k, E.decodeUtf8 v) : acc) )
               <|> pure (mkTags acc)

        -- tagKey = hasTag ts

        -- The nice thing about this is that it's completely retarded.
        -- It relies on assumptions about possible keys following a tag
        -- block, including everything where anything containing one
        -- appears. Then again, it is not only much simpler than looking
        -- up each key in a known-tags structure obtained from the MPD,
        -- it is faster.
        tagKey = not . ( `elem` ["file", "directory", "Pos"] )

parsePLTrack :: Parser PlaylistTrack
parsePLTrack = mkPLT <$> parseTrack
                     <*> key "Pos"
                     <*> key "Id"

    where
        mkPLT trk pos tid =
            PlaylistTrack { plTrack = trk, plTrackPos = pos, plTrackId = tid }

parseTracks :: Parser [Track]
parseTracks = many parseTrack

parsePLTracks :: Parser [PlaylistTrack]
parsePLTracks = many parsePLTrack

parseOutputs :: Parser [Output]
parseOutputs = many
        ( mkOut <$> key "outputid"
                <*> key "outputname"
                <*> key "outputenabled" )
    where
        mkOut k n e = Output { outputID = k, outputName = n, outputEnabled = e }

parseSticker :: Parser (Text, Text)
parseSticker = split <$> key "sticker"
    where
        split x = let (a, b) = (== '=') `B.break` x
                  in (E.decodeUtf8 a, E.decodeUtf8 (B.drop 1 b))


-- }}}

-- decoders (exported) {{{

decodePosIDs :: Decoder [(PlaylistPos, TrackID)]
decodePosIDs = asDecoder $ many ( (,) <$> key "cpos" <*> key "Id" )

decodeSongsPltime :: Decoder (Int, Seconds)
decodeSongsPltime = asDecoder ( (,) <$> key "songs" <*> key "playtime" )

decodeSingleTags :: ByteString -> Decoder [MetaContent]
decodeSingleTags = asDecoder . many . key

decodeURIs :: Decoder [URI]
decodeURIs = asDecoder ( many (key "file") )

decodeDirsFiles :: Decoder [Either URI URI]
decodeDirsFiles = asDecoder $
    many (   Left  <$> key "directory"
         <|> Right <$> key "file" )

decodeDirsTracks :: Decoder [Either URI Track]
decodeDirsTracks = asDecoder $
        many (   Left  <$> key "directory"
             <|> Right <$> parseTrack )

decodeTagTypes :: Decoder [MetaField]
decodeTagTypes = asDecoder ( many ( key "tagtype" ) )

decodeURLHandlers :: Decoder [String]
decodeURLHandlers = asDecoder ( many (key "handler") )

decodeCommands :: Decoder [String]
decodeCommands = asDecoder ( many (key "command") )

-- XXX last-modified. proper time?
decodePlaylists :: Decoder [(Playlist, Text)]
decodePlaylists = asDecoder $
        many ( (,) <$> key "playlist"
                   <*> key "Last-Modified" )

decodeSticker :: Decoder Text
decodeSticker = asDecoder (snd <$> parseSticker)

decodeStickers :: Decoder [(Text, Text)]
decodeStickers = asDecoder (many parseSticker)

decodeStickersFiles :: Decoder [(URI, Text)]
decodeStickersFiles = asDecoder $
        many ( wrap <$> key "file"
                    <*> parseSticker )
    where
        wrap u (_, v) = (u, v)

-- }}}

-- big tables {{{

buildByMapLax :: a -> M.Map ByteString (ByteString -> Parser (a -> a)) -> Parser a
buildByMapLax zero decoders = loop zero
    where
        loop a = ( lineKV >>= \(k, v) ->
                        maybe (pure a) (\p -> p v <*> pure a)
                                (k `M.lookup` decoders) >>= loop )
                 <|> pure a


assocTypeLax :: a -> [(ByteString, ByteString -> Parser (a -> a))] -> Parser a
assocTypeLax zero =
            buildByMapLax zero
            . M.fromDistinctAscList
            . sortBy (compare `on` fst)



parseStats :: Parser Stats
parseStats = assocTypeLax zeroStats 

    [ ("artists"    , (\x s -> s { stsArtists    = x}) <$$> payload)
    , ("albums"     , (\x s -> s { stsAlbums     = x}) <$$> payload)
    , ("songs"      , (\x s -> s { stsSongs      = x}) <$$> payload)
    , ("uptime"     , (\x s -> s { stsUptime     = x}) <$$> payload)
    , ("playtime"   , (\x s -> s { stsPlaytime   = x}) <$$> payload)
    , ("db_playtime", (\x s -> s { stsDbPlaytime = x}) <$$> payload)
    , ("db_update"  , (\x s -> s { stsDbUpdate   = x}) <$$> payload)
    ]



parseStatus :: Parser Status
parseStatus = assocTypeLax zeroStatus

    [ ("volume"        , (\x s -> s { stVolume     = x            }) <$$> plVol    )
    , ("repeat"        , (\x s -> s { stRepeat     = x            }) <$$> payload  )
    , ("random"        , (\x s -> s { stRandom     = x            }) <$$> payload  )
    , ("single"        , (\x s -> s { stSingle     = x            }) <$$> payload  )
    , ("consume"       , (\x s -> s { stConsume    = x            }) <$$> payload  )
    , ("playlist"      , (\x s -> s { stPLVer      = PLV x        }) <$$> payload  )
    , ("playlistlength", (\x s -> s { stPLLen      = x            }) <$$> payload  )
    , ("xfade"         , (\x s -> s { stXfade      = x            }) <$$> payload  )
    , ("state"         , (\x s -> s { stState      = x            }) <$$> payload  )
    , ("song"          , (\x s -> s { stSong       = x            }) <$$> payload  )
    , ("songid"        , (\x s -> s { stSongId     = TID x        }) <$$> payload  )
    , ("bitrate"       , (\x s -> s { stBitrate    = x            }) <$$> payload  )
    , ("nextsong"      , (\x s -> s { stNextsong   = x            }) <$$> payload  )
    , ("nextsongid"    , (\x s -> s { stNextsongId = TID x        }) <$$> payload  )
    , ("updating_db"   , (\x s -> s { stUpdatingDB = Just (JID x) }) <$$> payload  )
    , ("time"     , (\[a, b] s -> s { stTime       = (a, b)       }) <$$> parseNTuple 2 )
    , ("audio" , (\[a, b, c] s -> s { stAudio      = (a, b, c)    }) <$$> parseNTuple 3 )

--      elapsed: [3] Total time elapsed within the current song, but with higher resolution.
--      error: if there is an error, returns message here 

     ]

    where
        plVol = (\x -> if x < 0 then Nothing else Just x) <$$> payload



coDecMap :: (Ord a)
         => [(ByteString, a)]
         -> (ByteString -> Parser a, a -> ByteString)

coDecMap m = ( \b -> case b `M.lookup` M.fromAscList (sort m) of
                          Nothing -> unexpected (B.unpack b)
                          Just x  -> return x

             , \a -> case a `M.lookup` (M.fromAscList . sort . swap) m of
                          Nothing -> error "coDecMap: inexhaustive map."
                          Just x  -> x

             )
    where

        sort :: (Ord a) => [(a, b)] -> [(a, b)]
        sort = sortBy (compare `on` fst)

        swap = map (\(a, b) -> (b, a))


parseChangedSubsys1 :: ByteString -> Parser SubsysChanged
encodeChangedSubsys :: SubsysChanged -> ByteString

( parseChangedSubsys1, encodeChangedSubsys ) = coDecMap

        [ ("database"       , ChangedDatabase      ) 
        , ("mixer"          , ChangedMixer         ) 
        , ("options"        , ChangedOptions       ) 
        , ("output"         , ChangedOutput        ) 
        , ("player"         , ChangedPlayer        ) 
        , ("playlist"       , ChangedPlaylist      ) 
        , ("stored_playlist", ChangedStoredPlaylist) 
        , ("update"         , ChangedUpdate        ) 
        ]

parseChangedSubsys :: Parser [SubsysChanged]
parseChangedSubsys = many ( key "changed" )

-- }}}


-- }}}

-- that _other_ dec {{{

isOK, isAck, isListOK :: ByteString -> Bool
isOK     = ( == "OK" )
isAck    = ( "ACK " `B.isPrefixOf` )
isListOK = ( == "list_OK" )


readAck :: ByteString -> Result Ack
readAck s = either
                -- If we can't manage an ack all bets are probably off,
                -- but let's push the responsibility for handling that
                -- gracefully to the client.
                (\_ -> Left (OtherError "can't parse ack"))
                Right (parse ackParser "" s)


int :: Parsec ByteString u Int
int = read <$> many1 digit

ackerr :: Parsec ByteString u AckError
ackerr = int2AckErr <$> int

between1 :: Char -> Char -> Parsec ByteString u a -> Parsec ByteString u a
between1 a b = between (char a) (char b)

ackParser :: Parsec ByteString () Ack
ackParser = do
    string "ACK "
    (e, p) <- between1 '[' ']'
                ((,) <$> ackerr <* char '@' <*> int)
              <* spaces
    cmd <- between1 '{' '}' ( many ( noneOf "}" ) )
           <* spaces
    let cmd' | null cmd  = Nothing
             | otherwise = Just cmd
    rest <- bsToString <$> getInput

    return  Ack { ackError = e
                , ackPosition = p
                , ackCommand = cmd'
                , ackDescription = rest }


-- }}}

-- vim:set fdm=marker:
