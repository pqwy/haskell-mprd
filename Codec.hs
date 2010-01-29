{-# LANGUAGE PatternGuards, FlexibleInstances, TypeSynonymInstances  #-}

module Codec
    ( Decoder, Parameter(..), Response(..)
    , joinParams
    , readPosIDs, readSongsPltime, readSingleTags, readDirsFiles, readDirsTracks
    ) where


import Core
import Types


import Prelude hiding ( readList )


import Data.List ( unfoldr, sortBy )
import Data.Function ( on )

import Control.Monad
import Control.Applicative hiding ( Alternative(..), many )

import Data.Text ( Text, pack, unpack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Pos



-- co {{{

class Parameter a where encode :: a -> Text


instance Parameter Bool where
    encode True  = pack "1"
    encode False = pack "0"
    
instance Parameter Int where encode = simpleEncode

instance Parameter TrackID where encode (TID x) = encode x

instance Parameter PlaylistVersion where encode (PLV x) = encode x

instance Parameter Range where
    encode (a :/: b) = T.intercalate (pack ":") [encode a, encode b]

instance Parameter URI where
    encode (URI u) = quotes u

instance Parameter QueryPred where
    encode (t, v) = joinParams [t, quotes v]

instance Parameter [QueryPred] where
    encode tvs = joinParams [ p | (t, v) <- tvs, p <- [t, quotes v] ]

instance Parameter Text where
    encode = quotes


joinParams :: [Text] -> Text
joinParams = T.intercalate (pack " ")

quotes :: Text -> Text
quotes = T.cons '"' . (`T.snoc` '"')

simpleEncode :: (Show a) => a -> Text
simpleEncode = pack . show

-- }}}

-- dec {{{

type Decoder a = MPDConnState -> [Text] -> Result a


class Response a where decode :: Decoder a


instance Response () where decode _ _ = return ()

instance Response Stats where
    decode = asDecoder' readStats

instance Response Status where
    decode = asDecoder' readStatus

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



type Parser = Parsec [Text] (Maybe Text)



asDecoder :: (MPDConnState -> Parser a) -> Decoder a
asDecoder p cn tx =
    either (Left . DecodeError2 . sourceLine . errorPos) Right
           (runParser (p cn) Nothing "<input>" tx)


asDecoder' :: Parser a -> Decoder a
asDecoder' = asDecoder . const




posNextLine s _ _ = incSourceLine s 1


satisfyLn :: (Text -> Bool) -> Parser Text
satisfyLn p = tokenPrim show posNextLine $
                \l -> if p l then Just l else Nothing

line :: Parser Text
line = satisfyLn (const True) <?> "a line of input"



lineKV :: Parser (Text, Text)
lineKV = tokenPrim show posNextLine splitKeyValue
                <?> "'key: val' line"

satisfyKey :: (Text -> Bool) -> Parser (Text, Text)
satisfyKey p = try ( lineKV >>= \x@(k, v) ->
                        if p k then return x else mzero )
                <?> "some key..."

textKey :: Text -> Parser Text
textKey x = (fmap snd . satisfyKey . (==)) x <?> ( "key '" ++ unpack x ++ "'" )

key :: String -> Parser Text
key s = ( textKey . pack ) s <?> ( "key '" ++ s ++ "'" )


splitKeyValue :: Text -> Maybe (Text, Text)
splitKeyValue t | (k, v) <- split t, T.length v > 2 = Just (k, 2 `T.drop` v)
                | otherwise                         = Nothing
    where
        split = T.break (pack ": ")




readInt :: Text -> Parser Int
readInt t = case reads (unfoldr T.uncons t) of
                 [(x, [])] -> return x
                 _         -> unexpected (show t)


readBool :: Text -> Parser Bool
readBool = fmap (/= 0) . readInt



readColonList :: Text -> Parser [Text]
readColonList = return . T.split (pack ":")


readNTuple :: Int -> Text -> Parser [Text]
readNTuple n = readColonList >=> \l ->
    if length l == n then return l
                     else fail (show n ++ "-place list")


readInts x = readNTuple x >=> mapM readInt



assocTypeLax :: a -> (M.Map Text (Text -> Parser (a -> a))) -> Parser a
assocTypeLax zero decoders = loop zero
    where
        loop a = ( lineKV >>= \(k, v) ->
                        -- maybe (pure a) (<*> pure a)
                        maybe (pure a) (\p -> p v <*> pure a)
                                (k `M.lookup` decoders) >>= loop )
                 <|> pure a


assocTypeLax' :: a -> [(String, Text -> Parser (a -> a))] -> Parser a
assocTypeLax' zero =
            assocTypeLax zero
            . M.fromDistinctAscList
            . map (\(a, b) -> (pack a, b))
            . sortBy (compare `on` fst)


readTrack :: Tagset -> Parser Track
readTrack tags =
    mkTrack <$> key "file"
            <*> ((Just <$> (key "Time" >>= readInt)) <|> pure Nothing)
            <*> readTags tags

    where
        mkTrack f tm tg =
            zeroTrack { trackFile = f, trackTime = tm, trackTags = tg } 
    

readTags :: Tagset -> Parser Tags
readTags ts = loop []
    where
        loop acc = (satisfyKey tagKey >>= loop . (: acc))
               <|> pure (mkTags acc)

        tagKey = hasTag ts

        -- tagKey = not . ( `elem` map T.pack
        --         ["file", "directory", "Pos", "Id"] )


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

readPlayState x | x == pack "play"  = return StatePlay
                | x == pack "stop"  = return StateStop
                | x == pack "pause" = return StatePause
                | otherwise         = unexpected (unpack x)


readPosIDs :: Decoder [(PlaylistPos, TrackID)]
readPosIDs = asDecoder' $
        many ( (,) <$> (key "cpos" >>= readInt) <*> readTID )


readSongsPltime :: Decoder (Int, Seconds)
readSongsPltime = asDecoder' $
        (,) <$> (key "songs" >>= readInt)
            <*> (key "playtime" >>= readInt)


readSingleTags :: Text -> Decoder [Text]
readSingleTags = asDecoder' . many . textKey



readDirsFiles :: Decoder [Either Text Text]
readDirsFiles = asDecoder' $
    many ( Left  <$> key "directory" <|> Right <$> key "file" )


readDirsTracks :: Decoder [Either Text Track]
readDirsTracks = asDecoder $ \ts ->
        many ( Left <$> key "directory" <|> (Right <$> readTrack (mpdTags ts)))

-- }}}


-- vim:set fdm=marker:
