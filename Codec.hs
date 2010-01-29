{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternGuards, FlexibleInstances, TypeSynonymInstances  #-}

module Codec
    ( Decoder, Parameter(..), Response(..)
    , joinParams
    , readPosIDs, readSongsPltime, readSingleTags, readDirsFiles, readDirsTracks
    , readTagTypes, readURLHandlers, readCommands
    ) where


import Core
import Types


import Prelude hiding ( readList )


import Data.List ( unfoldr, sortBy )
import Data.Function ( on )
import Data.Maybe

import Control.Monad
import Control.Applicative hiding ( Alternative(..), many )

import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B

import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos



-- co {{{

class Parameter a where encode :: a -> ByteString


instance (Parameter a) => Parameter [a] where
    encode = joinParams . map encode

instance Parameter Bool where
    encode True  = "1"
    encode False = "0"
    
instance Parameter Int where encode = simpleEncode

instance Parameter TrackID where encode (TID x) = encode x

instance Parameter PlaylistVersion where encode (PLV x) = encode x

instance Parameter Text where
    encode = E.encodeUtf8 . quotes

instance Parameter Range where
    encode (a :/: b) = B.intercalate ":" [encode a, encode b]

instance Parameter URI where
    encode (URI u) = encode u

instance Parameter QueryPred where
    encode (t, v) = joinParams [t, encode v]

instance Parameter OutputID where
    encode (OutputID o) = encode o

instance Parameter SubsysChanged where
    encode = encodeChangedSubsys


joinParams :: [ByteString] -> ByteString
joinParams = B.intercalate " "

quotes :: Text -> Text
quotes = T.cons '"' . (`T.snoc` '"')

simpleEncode :: (Show a) => a -> ByteString
simpleEncode = B.pack . show

-- }}}

-- dec {{{

type Decoder a = MPDConnState -> [ByteString] -> Result a


class Response a where decode :: Decoder a


instance Response () where decode _ _ = return ()

instance Response Stats where
    decode = asDecoder' readStats

instance Response Status where
    decode = asDecoder' readStatus

instance Response [SubsysChanged] where
    decode = asDecoder' readChangedSubsys

instance Response Track where
    decode = asDecoder (readTrack . mpdTags)

instance Response [Track] where
    decode = asDecoder (readTracks . mpdTags)

instance Response PlaylistTrack where
    decode = asDecoder (readPLTrack . mpdTags)

instance Response [PlaylistTrack] where
    decode = asDecoder (readPLTracks . mpdTags)

instance Response TrackID where
    decode = asDecoder' readTID

instance Response JobID where
    decode = asDecoder' readJID

instance Response [Output] where
    decode = asDecoder' readOutputs



type Parser = Parsec [ByteString] (Maybe ByteString)



asDecoder :: (MPDConnState -> Parser a) -> Decoder a
asDecoder p cn tx =
    either (Left . DecodeError2 . sourceLine . errorPos) Right
           (runParser (p cn) Nothing "<input>" tx)


asDecoder' :: Parser a -> Decoder a
asDecoder' = asDecoder . const




posNextLine s _ _ = incSourceLine s 1


satisfyLn :: (ByteString -> Bool) -> Parser ByteString
satisfyLn p = tokenPrim show posNextLine $
                \l -> if p l then Just l else Nothing

line :: Parser ByteString
line = satisfyLn (const True) <?> "a line of input"



lineKV :: Parser (ByteString, ByteString)
lineKV = tokenPrim show posNextLine splitKeyValue
                <?> "'key: val' line"


satisfyKey :: (ByteString -> Bool) -> Parser (ByteString, ByteString)
satisfyKey p = try ( lineKV >>= \x@(k, v) ->
                        if p k then return x else mzero )
                <?> "some key..."


key :: ByteString -> Parser ByteString
key x = snd <$> satisfyKey (== x)
            <?> ( "key '" ++ B.unpack x ++ "'" )


splitKeyValue :: ByteString -> Maybe (ByteString, ByteString)
splitKeyValue t | (k, v) <- split t, B.length v > 2 = Just (k, 2 `B.drop` v)
                | otherwise                         = Nothing
    where
        split = B.breakSubstring ": " -- <- B.break?



readInt :: ByteString -> Parser Int
readInt t = case B.readInt t of
                 Just (a, b) | B.null b -> return a
                 _                      -> unexpected (B.unpack t)


readBool :: ByteString -> Parser Bool
readBool = fmap (/= 0) . readInt



readColonList :: ByteString -> Parser [ByteString]
readColonList = return . B.split ':'


readNTuple :: Int -> ByteString -> Parser [ByteString]
readNTuple n = readColonList >=> \l ->
    if length l == n then return l
                     else fail (show n ++ "-place list")


readInts x = readNTuple x >=> mapM readInt



fileField :: Parser URI
fileField = (URI . E.decodeUtf8) <$> key "file"


dirField :: Parser URI
dirField = (URI . E.decodeUtf8) <$> key "directory"



assocTypeLax :: a -> (M.Map ByteString (ByteString -> Parser (a -> a))) -> Parser a
assocTypeLax zero decoders = loop zero
    where
        loop a = ( lineKV >>= \(k, v) ->
                        -- maybe (pure a) (<*> pure a)
                        maybe (pure a) (\p -> p v <*> pure a)
                                (k `M.lookup` decoders) >>= loop )
                 <|> pure a


assocTypeLax' :: a -> [(ByteString, ByteString -> Parser (a -> a))] -> Parser a
assocTypeLax' zero =
            assocTypeLax zero
            . M.fromDistinctAscList
            . sortBy (compare `on` fst)


readTrack :: Tagset -> Parser Track
readTrack tags =
    mkTrack <$> fileField
            <*> ((Just <$> (key "Time" >>= readInt)) <|> pure Nothing)
            <*> readTags tags

    where
        mkTrack f tm tg =
            zeroTrack { trackFile = f, trackTime = tm, trackTags = tg } 
    

readTags :: Tagset -> Parser Tags
readTags ts = loop []
    where
        loop acc = ( satisfyKey tagKey >>= \(k, v) ->
                        loop ((k, E.decodeUtf8 v) : acc) )
               <|> pure (mkTags acc)

        tagKey = hasTag ts

        -- tagKey = not . ( `elem` ["file", "directory", "Pos", "Id"] )


infixl 4 <$$>
(<$$>) :: (Functor f) => (a -> b) -> (c -> f a) -> c -> f b
(<$$>) = (.) . fmap



readTID :: Parser TrackID
readTID = key "Id" >>= TID <$$> readInt


readJID :: Parser JobID
readJID = key "updating_db" >>= JID <$$> readInt


readPLTrack :: Tagset -> Parser PlaylistTrack
readPLTrack ts =
    mkPLT <$> readTrack ts
          <*> (key "Pos" >>= readInt)
          <*> readTID

    where
        mkPLT trk pos id =
            PlaylistTrack { plTrack = trk, plTrackPos = pos, plTrackId = id }


readTracks :: Tagset -> Parser [Track]
readTracks ts = many (readTrack ts)

readPLTracks :: Tagset -> Parser [PlaylistTrack]
readPLTracks ts = many (readPLTrack ts)



readStats :: Parser Stats
readStats = assocTypeLax' zeroStats

    [ ("artists"    , (\x s -> s { stsArtists    = x}) <$$> readInt)
    , ("albums"     , (\x s -> s { stsAlbums     = x}) <$$> readInt)
    , ("songs"      , (\x s -> s { stsSongs      = x}) <$$> readInt)
    , ("uptime"     , (\x s -> s { stsUptime     = x}) <$$> readInt)
    , ("playtime"   , (\x s -> s { stsPlaytime   = x}) <$$> readInt)
    , ("db_playtime", (\x s -> s { stsDbPlaytime = x}) <$$> readInt)
    , ("db_update"  , (\x s -> s { stsDbUpdate   = x}) <$$> readInt)
    ]



readStatus :: Parser Status
readStatus = assocTypeLax' zeroStatus

    [ ("volume"        , (\x s -> s { stVolume         = x            }) <$$> readVol        )
    , ("repeat"        , (\x s -> s { stRepeat         = x            }) <$$> readBool       )
    , ("random"        , (\x s -> s { stRandom         = x            }) <$$> readBool       )
    , ("single"        , (\x s -> s { stSingle         = x            }) <$$> readBool       )
    , ("consume"       , (\x s -> s { stConsume        = x            }) <$$> readBool       )
    , ("playlist"      , (\x s -> s { stPlaylist       = PLV x        }) <$$> readInt        )
    , ("playlistlength", (\x s -> s { stPlaylistLength = x            }) <$$> readInt        )
    , ("xfade"         , (\x s -> s { stXfade          = x            }) <$$> readInt        )
    , ("state"         , (\x s -> s { stState          = x            }) <$$> readPlayState  )
    , ("song"          , (\x s -> s { stSong           = x            }) <$$> readInt        )
    , ("songid"        , (\x s -> s { stSongId         = TID x        }) <$$> readInt        )
    , ("bitrate"       , (\x s -> s { stBitrate        = x            }) <$$> readInt        )
    , ("nextsong"      , (\x s -> s { stNextsong       = x            }) <$$> readInt        )
    , ("nextsongid"    , (\x s -> s { stNextsongId     = TID x        }) <$$> readInt        )
    , ("updating_db"   , (\x s -> s { stUpdatingDB     = Just (JID x) }) <$$> readInt        )
    , ("time"     , (\[a, b] s -> s { stTime           = (a, b)       }) <$$> readInts 2     )
    , ("audio" , (\[a, b, c] s -> s { stAudio          = (a, b, c)    }) <$$> readInts 3     )

--      elapsed: [3] Total time elapsed within the current song, but with higher resolution.
--      error: if there is an error, returns message here 

     ]


readVol = (\x -> if x < 0 then Nothing else Just x) <$$> readInt

readPlayState x | x == "play"  = return StatePlay
                | x == "stop"  = return StateStop
                | x == "pause" = return StatePause
                | otherwise    = unexpected (B.unpack x)


readPosIDs :: Decoder [(PlaylistPos, TrackID)]
readPosIDs = asDecoder' $
        many ( (,) <$> (key "cpos" >>= readInt) <*> readTID )


readSongsPltime :: Decoder (Int, Seconds)
readSongsPltime = asDecoder' $
        (,) <$> (key "songs" >>= readInt)
            <*> (key "playtime" >>= readInt)


readSingleTags :: ByteString -> Decoder [Text]
readSingleTags = asDecoder' . many . (fmap E.decodeUtf8) . key



readDirsFiles :: Decoder [Either URI URI]
readDirsFiles = asDecoder' $
    many ( Left  <$> dirField <|> Right <$> fileField )


readDirsTracks :: Decoder [Either URI Track]
readDirsTracks = asDecoder $ \ts ->
        many ( Left <$> dirField
          <|> (Right <$> readTrack (mpdTags ts)))


readOutputs :: Parser [Output]
readOutputs = many
        ( mkOut <$> (OutputID <$> (key "outputid" >>= readInt))
                <*> (E.decodeUtf8 <$> key "outputname")
                <*> (key "outputenabled" >>= readBool) )
    where
        mkOut k n e = Output { outputID = k, outputName = n, outputEnabled = e }


readTagTypes :: Decoder [MetaField]
readTagTypes = asDecoder' $ many ( key "tagtype" )

readURLHandlers :: Decoder [Text]
readURLHandlers = asDecoder' $
        many (E.decodeUtf8 <$> key "handler")

readCommands :: Decoder [Text]
readCommands = asDecoder' $
        many (E.decodeUtf8 <$> key "command")



coDecMap :: (Ord a)
         => [(ByteString, a)]
         -> (ByteString -> Parser a, a -> ByteString)

coDecMap m = ( \b -> case b `M.lookup` M.fromAscList (sort m) of
                          Nothing -> unexpected (B.unpack b)
                          Just x  -> return x

             , \a -> fromMaybe
                        (error "coDecMap: inexhaustive map.")
                        (a `M.lookup` (M.fromAscList . sort . swap) m )

             )
    where

        sort :: (Ord a) => [(a, b)] -> [(a, b)]
        sort = sortBy (compare `on` fst)

        swap = map (\(a, b) -> (b, a))



( readChangedSubsys1, encodeChangedSubsys ) = coDecMap

        [ ("database"       , ChangedDatabase      ) 
        , ("mixer"          , ChangedMixer         ) 
        , ("options"        , ChangedOptions       ) 
        , ("output"         , ChangedOutput        ) 
        , ("player"         , ChangedPlayer        ) 
        , ("playlist"       , ChangedPlaylist      ) 
        , ("stored_playlist", ChangedStoredPlaylist) 
        , ("update"         , ChangedUpdate        ) 
        ]

readChangedSubsys = many ( key "changed" >>= readChangedSubsys1 )


-- }}}


-- vim:set fdm=marker:
