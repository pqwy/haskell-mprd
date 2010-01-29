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



lineKV :: Parser Text
lineKV = ( tokenPrim show posNextLine splitKeyValue >>=
                \(k, v) -> setState (Just v) >> return k )
            <?> "'key: val' line"

key :: String -> Parser ()
key s = try ( lineKV >>= \k ->
                if k == pack s then return () else mzero )
            <?> ("'" ++ s ++ "': val")

splitKeyValue :: Text -> Maybe (Text, Text)
splitKeyValue t | (k, v) <- split t, T.length v > 2 = Just (k, 2 `T.drop` v)
                | otherwise                         = Nothing
    where
        split = T.break (pack ": ")


popReg :: Parser Text
popReg = getState >>=
            maybe (fail "reg empty") (\s -> setState Nothing >> return s)





readInt :: Text -> Parser Int
readInt t = case reads (unfoldr T.uncons t) of
                 [(x, [])] -> return x
                 _         -> unexpected (show t)

regReadInt :: Parser Int
regReadInt = popReg >>= readInt


regReadBool :: Parser Bool
regReadBool = fmap (/= 0) regReadInt



regReadColonList :: Parser [Text]
regReadColonList = popReg >>= return . T.split (pack ":")


regReadNTuple :: Int -> Parser [Text]
regReadNTuple n = regReadColonList >>= \l ->
    if length l == n then return l
                     else fail (show n ++ "-place list")


regReadInts = regReadNTuple >=> mapM readInt



assocTypeLax :: a -> (M.Map Text (Parser (a -> a))) -> Parser a
assocTypeLax zero decoders = loop zero
    where
        loop a = ( lineKV >>= \k ->
                        maybe (pure a) (<*> pure a)
                                (k `M.lookup` decoders) >>= loop )
                 <|> pure a


assocTypeLax' :: a -> [(String, Parser (a -> a))] -> Parser a
assocTypeLax' zero =
            assocTypeLax zero
            . M.fromDistinctAscList
            . map (\(a, b) -> (pack a, b))
            . sortBy (compare `on` fst)


readTrack :: Tagset -> Parser Track
readTrack tags =
    mkTrack <$> (key "file" >> popReg)
            <*> ((Just <$> (key "Time" >> regReadInt)) <|> pure Nothing)
            <*> readTags tags

    where
        mkTrack f tm tg =
            zeroTrack { trackFile = f, trackTime = tm, trackTags = tg } 
    

-- readTags :: Tagset -> Parser Tags
-- readTags ts = loop []
--     where
--         loop acc = do
--                 k <- try (lineKV >>=
--                     \k -> if hasTag ts k then return k else mzero)
--                 popReg >>= \v -> loop ((k, v) : acc)
--             <|> pure (mkTags acc)


readTags _ = loop []
    where
        loop acc = do
                k <- try (lineKV >>=
                        \k -> if stopKey k then mzero else return k )
                popReg >>= \v -> loop ((k, v) : acc)
            <|> pure (mkTags acc)

        stopKey k = k `elem` map T.pack
                    ["file", "directory", "Pos", "Id"]


readTID :: Parser TrackID
readTID = key "Id" >> TID <$> regReadInt

readJID :: Parser JobID
readJID = key "updating_db" >> JID <$> regReadInt


readPLTrack :: Tagset -> Parser PlaylistTrack
readPLTrack ts =
    mkPLT <$> readTrack ts
          <*> (key "Pos" >> regReadInt)
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

    [ ("artists"    , (\x s -> s { stsArtists    = x}) <$> regReadInt)
    , ("albums"     , (\x s -> s { stsAlbums     = x}) <$> regReadInt)
    , ("songs"      , (\x s -> s { stsSongs      = x}) <$> regReadInt)
    , ("uptime"     , (\x s -> s { stsUptime     = x}) <$> regReadInt)
    , ("playtime"   , (\x s -> s { stsPlaytime   = x}) <$> regReadInt)
    , ("db_playtime", (\x s -> s { stsDbPlaytime = x}) <$> regReadInt)
    , ("db_update"  , (\x s -> s { stsDbUpdate   = x}) <$> regReadInt)
    ]



readStatus :: Parser Status
readStatus = assocTypeLax' zeroStatus

    [ ("volume"        , (\x s -> s { stVolume         = x            }) <$> regReadVol        )
    , ("repeat"        , (\x s -> s { stRepeat         = x            }) <$> regReadBool       )
    , ("random"        , (\x s -> s { stRandom         = x            }) <$> regReadBool       )
    , ("single"        , (\x s -> s { stSingle         = x            }) <$> regReadBool       )
    , ("consume"       , (\x s -> s { stConsume        = x            }) <$> regReadBool       )
    , ("playlist"      , (\x s -> s { stPlaylist       = PLV x        }) <$> regReadInt        )
    , ("playlistlength", (\x s -> s { stPlaylistLength = x            }) <$> regReadInt        )
    , ("xfade"         , (\x s -> s { stXfade          = x            }) <$> regReadInt        )
    , ("state"         , (\x s -> s { stState          = x            }) <$> regReadPlayState  )
    , ("song"          , (\x s -> s { stSong           = x            }) <$> regReadInt        )
    , ("songid"        , (\x s -> s { stSongId         = TID x        }) <$> regReadInt        )
    , ("bitrate"       , (\x s -> s { stBitrate        = x            }) <$> regReadInt        )
    , ("nextsong"      , (\x s -> s { stNextsong       = x            }) <$> regReadInt        )
    , ("nextsongid"    , (\x s -> s { stNextsongId     = TID x        }) <$> regReadInt        )
    , ("updating_db"   , (\x s -> s { stUpdatingDB     = Just (JID x) }) <$> regReadInt        )
    , ("time"     , (\[a, b] s -> s { stTime           = (a, b)       }) <$> regReadInts 2     )
    , ("audio" , (\[a, b, c] s -> s { stAudio          = (a, b, c)    }) <$> regReadInts 3     )

--      elapsed: [3] Total time elapsed within the current song, but with higher resolution.
--      error: if there is an error, returns message here 

     ]


regReadVol = (\x -> if x < 0 then Nothing else Just x) <$> regReadInt

regReadPlayState = popReg >>= chk
    where chk x | x == pack "play"  = return StatePlay
                | x == pack "stop"  = return StateStop
                | x == pack "pause" = return StatePause
                | otherwise         = unexpected (unpack x)


readPosIDs :: Decoder [(PlaylistPos, TrackID)]
readPosIDs = asDecoder' $
        many ( (,) <$> (key "cpos" >> regReadInt) <*> readTID )


readSongsPltime :: Decoder (Int, Seconds)
readSongsPltime = asDecoder' $
        (,) <$> (key "songs" >> regReadInt)
            <*> (key "playtime" >> regReadInt)


readSingleTags :: Text -> Decoder [Text]
readSingleTags t = asDecoder' . many $
        lineKV >>= \k ->
            if k == t then popReg
                    else unexpected (unpack k)

dirkey = key "directory" >> popReg

readDirsFiles :: Decoder [Either Text Text]
readDirsFiles = asDecoder' . many $
        ( Left <$> dirkey
      <|> Right <$> (key "file" >> popReg) )

readDirsTracks :: Decoder [Either Text Track]
readDirsTracks = asDecoder $ \ts ->
        many $ Left <$> dirkey
           <|> (Right <$> readTrack (mpdTags ts))

-- }}}


gimme :: IO [[Text]]
gimme = read `fmap` readFile "/tmp/xx.hs"


-- vim:set fdm=marker:
