{-# LANGUAGE TypeSynonymInstances  #-}

module Types where


import Core

import Data.Maybe
import qualified Data.Map as M




newtype URI = URI Text
    deriving (Eq, Show)

type QueryPred = (MetaField, MetaContent)


type Seconds = Int

type PlaylistPos = Int


newtype PlaylistVersion = PLV Int
    deriving (Eq, Ord, Show)

newtype TrackID = TID Int
    deriving (Eq, Show)

newtype JobID = JID Int
    deriving (Eq, Show)


data Stats =
    Stats { stsArtists, stsAlbums, stsSongs
          , stsUptime, stsPlaytime, stsDbPlaytime :: Int
          , stsDbUpdate :: Int } -- <- unix time

    deriving (Eq, Show)


zeroStats = 
    Stats { stsArtists  = 0, stsAlbums   = 0, stsSongs      = 0
          , stsUptime   = 0, stsPlaytime = 0, stsDbPlaytime = 0
          , stsDbUpdate = 0 }




data Status =
    Status { stRepeat, stRandom, stSingle, stConsume :: Bool

           , stVolume               :: Maybe Int
           , stPlaylist             :: PlaylistVersion
           , stPlaylistLength       :: Int
           , stXfade                :: Seconds
           , stState                :: PlayState
           , stSong, stNextsong     :: PlaylistPos
           , stSongId, stNextsongId :: TrackID
           , stTime                 :: (Seconds, Seconds)
           , stBitrate              :: Int
           , stAudio                :: (Int, Int, Int)
           , stUpdatingDB           :: Maybe JobID }

    deriving (Eq, Show)


zeroStatus =
    Status { stRepeat = False, stRandom = False, stSingle = False, stConsume = False

           , stVolume         = Nothing
           , stPlaylist       = PLV 0
           , stPlaylistLength = 0
           , stXfade          = 0
           , stState          = StateStop
           , stSong           = 0
           , stNextsong       = 0
           , stSongId         = TID 0
           , stNextsongId     = TID 0
           , stTime           = (0, 0)
           , stBitrate        = 0
           , stAudio          = (0, 0, 0)
           , stUpdatingDB     = Nothing }


data PlayState = StatePlay | StateStop | StatePause
    deriving (Eq, Show)


data SubsysChanged =
          ChangedDatabase
        | ChangedMixer
        | ChangedOptions
        | ChangedOutput
        | ChangedPlayer
        | ChangedPlaylist
        | ChangedStoredPlaylist
        | ChangedUpdate

    deriving (Eq, Ord, Show)



data Track =
    Track { trackFile :: URI
          , trackTime :: Maybe Seconds
          , trackTags :: Tags }
    
    deriving (Eq, Show)


zeroTrack =
    Track { trackFile = undefined
          , trackTime = Nothing
          , trackTags = mkTags [] }



data PlaylistTrack =
    PlaylistTrack { plTrackPos :: PlaylistPos
                  , plTrackId  :: TrackID
                  , plTrack    :: Track }
    
    deriving (Eq, Show)


newtype Playlist = Playlist Text
    deriving (Show, Eq)


data Range = PlaylistPos :/: PlaylistPos
    deriving (Eq, Show)


newtype OutputID = OutputID Int
    deriving (Eq, Show)


data Output =
    Output { outputID      :: OutputID
           , outputName    :: Text
           , outputEnabled :: Bool }

    deriving (Eq, Show)




data AckError = AckNotList | AckArg | AckPassword | AckPermission | AckUnknown

              | AckNoExist | AckPlaylistMax | AckSystem | AckPlaylistLoad
              | AckUpdateAlready | AckPlayerSync | AckExist

              | AckUnrecognizedAck

    deriving (Eq, Show)


ackErrorMap = [ (AckNotList       , 1)
              , (AckArg           , 2)
              , (AckPassword      , 3)
              , (AckPermission    , 4)
              , (AckUnknown       , 4)

              , (AckNoExist       , 50)
              , (AckPlaylistMax   , 51)
              , (AckSystem        , 52)
              , (AckPlaylistLoad  , 53)
              , (AckUpdateAlready , 54)
              , (AckPlayerSync    , 55)
              , (AckExist         , 56)
              ]

int2AckErr :: Int -> AckError
int2AckErr = fromMaybe AckUnrecognizedAck . (`M.lookup` amap)
    where
        amap = M.fromDistinctAscList ( map (\(a, b) -> (b, a)) ackErrorMap )



data Ack =
    Ack { ackError       :: AckError
        , ackPosition    :: Int
        , ackCommand     :: Maybe String
        , ackDescription :: String }

    deriving (Eq, Show)


