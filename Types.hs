{-# LANGUAGE TypeSynonymInstances  #-}

module Types where


import Core


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


data Stats = Stats { stsArtists, stsAlbums,   stsSongs
                   , stsUptime,  stsPlaytime, stsDbPlaytime :: Int
                   , stsDbUpdate :: Int } -- <- unix time
    deriving (Eq, Show)


zeroStats = 
    Stats { stsArtists  = 0, stsAlbums   = 0, stsSongs      = 0
          , stsUptime   = 0, stsPlaytime = 0, stsDbPlaytime = 0
          , stsDbUpdate = 0 }




data Status = Status { stVolume :: Maybe Int
                     , stRepeat, stRandom, stSingle, stConsume :: Bool
                     , stPlaylist :: PlaylistVersion
                     , stPlaylistLength :: Int
                     , stXfade :: Seconds
                     , stState :: PlayState
                     , stSong, stNextsong :: PlaylistPos
                     , stSongId, stNextsongId :: TrackID
                     , stTime :: (Seconds, Seconds)
                     , stBitrate :: Int
                     , stAudio :: (Int, Int, Int)
                     , stUpdatingDB :: Maybe JobID
                    }
    deriving (Eq, Show)


zeroStatus =
    Status { stVolume = Nothing
           , stRepeat = False, stRandom = False, stSingle = False, stConsume = False
           , stPlaylist = PLV 0
           , stPlaylistLength = 0
           , stXfade = 0
           , stState = StateStop
           , stSong = 0, stNextsong = 0
           , stSongId = TID 0, stNextsongId = TID 0
           , stTime = (0, 0)
           , stBitrate = 0
           , stAudio = (0, 0, 0)
           , stUpdatingDB = Nothing
           }


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



data Track = Track
    { trackFile :: URI
    , trackTime :: Maybe Seconds
    , trackTags :: Tags
    } deriving (Eq, Show)


zeroTrack = Track { trackFile = undefined
                  , trackTime = Nothing
                  , trackTags = mkTags [] }



data PlaylistTrack = PlaylistTrack
    { plTrackPos :: PlaylistPos
    , plTrackId  :: TrackID
    , plTrack    :: Track
    } deriving (Eq, Show)



data Range = PlaylistPos :/: PlaylistPos
    deriving (Eq, Show)


newtype OutputID = OutputID Int
    deriving (Eq, Show)


data Output = Output { outputID      :: OutputID
                     , outputName    :: Text
                     , outputEnabled :: Bool }
        deriving (Eq, Show)


