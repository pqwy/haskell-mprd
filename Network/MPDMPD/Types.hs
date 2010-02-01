{-# LANGUAGE TypeSynonymInstances  #-}

module Network.MPDMPD.Types
    ( Text, ByteString
    
    , Result
    , MPDError(..), AckError(..), Ack(..)
    , int2AckErr
    
    , module Network.MPDMPD.Tags
    , QueryPred, URI(..)
    , queryStr

    , Seconds, PlaylistPos, PlaylistVersion(..), TrackID(..), JobID(..), OutputID(..)
    , Stats(..), Status(..), PlayState(..), SubsysChanged(..)
    , Track(..), PlaylistTrack(..), Playlist(..), Range(..), Output(..)

    , uri, lkpTag
    , zeroStats, zeroStatus, zeroTrack

    ) where


import Network.MPDMPD.Tags ( MetaField(..), MetaContent, Tags, mkTags )

import Data.Text ( Text, pack )
import Data.ByteString ( ByteString )
import qualified Data.Map as M





type Result = Either MPDError

instance Monad Result where
    return = Right

    Right x >>= f = f x
    Left x  >>= _ = Left x

    fail = Left . OtherError


data MPDError = DecodeError Int String
              | AckError Ack
              | ConnLocked
              | UnknownProtoResponse
              | DaemonGone
              | DaemonNotResponding
              | OtherError String

    deriving (Eq, Show)


data AckError = AckNotList | AckArg | AckPassword | AckPermission | AckUnknown

              | AckNoExist | AckPlaylistMax | AckSystem | AckPlaylistLoad
              | AckUpdateAlready | AckPlayerSync | AckExist

              | AckUnrecognizedAck

    deriving (Eq, Show)


int2AckErr :: Int -> AckError
int2AckErr 1  = AckNotList
int2AckErr 2  = AckArg
int2AckErr 3  = AckPassword
int2AckErr 4  = AckPermission
int2AckErr 5  = AckUnknown
int2AckErr 50 = AckNoExist
int2AckErr 51 = AckPlaylistMax
int2AckErr 52 = AckSystem
int2AckErr 53 = AckPlaylistLoad
int2AckErr 54 = AckUpdateAlready
int2AckErr 55 = AckPlayerSync
int2AckErr 56 = AckExist
int2AckErr _  = AckUnrecognizedAck


data Ack =
    Ack { ackError       :: AckError
        , ackPosition    :: Int
        , ackCommand     :: Maybe String
        , ackDescription :: String }

    deriving (Eq, Show)





newtype URI = URI Text
    deriving (Eq, Show)

uri :: String -> URI
uri = URI . pack


type QueryPred = (MetaField, MetaContent)

queryStr :: MetaField -> String -> QueryPred
queryStr f s = (f, pack s)


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

lkpTag :: MetaField -> Track -> Maybe MetaContent
lkpTag f t = f `M.lookup` trackTags t 


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
           , outputName    :: String
           , outputEnabled :: Bool }

    deriving (Eq, Show)


