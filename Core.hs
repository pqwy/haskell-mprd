{-# LANGUAGE TypeSynonymInstances, OverloadedStrings  #-}

module Core
    ( Text, ByteString
    , MetaField, MetaContent, Tagset, hasTag, mkTagset, tagsetTags
    , Tags, mkTags, lookupTag, tags
    , tag, query
    , MPDError(..), Result
    , MPDConnState(..), zeroState

    , Ack(..), AckError(..), int2AckErr
    -- , deftags
    ) where


import Control.Monad ( MonadPlus(..) )
import Data.Maybe

import Data.List ( sort, sortBy )
import Data.Function ( on )

import Data.Text ( Text, pack )
import qualified Data.Text as T
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B

import qualified Data.Set as S
import qualified Data.Map as M

import System.IO ( Handle )


data MPDConnState = MPDConnState
        { mpdConn :: Handle
        , mpdTags :: Tagset
        }

zeroState = MPDConnState { mpdConn = undefined , mpdTags = mkTagset [] }

-- deftags :: [ByteString]
-- deftags =
--     [ "Artist"   , "Album" , "AlbumArtist"
--     , "Title"    , "Track" , "Name"
--     , "Genre"    , "Date"  , "Composer"
--     , "Performer", "Disc"
--     , "MUSICBRAINZ_ARTISTID", "MUSICBRAINZ_ALBUMID"
--     , "MUSICBRAINZ_ALBUMARTISTID", "MUSICBRAINZ_TRACKID"
--     ]
-- 
-- zeroState = MPDConnState { mpdConn = undefined , mpdTags = mkTagset deftags }
-- 


data MPDError = DecodeError Int String
              | AckError Ack
              | ConnLocked
              | UnknownProtoResponse
              | DaemonGone
              | DaemonNotResponding
              | OtherError String

    deriving (Eq, Show)



type MetaField = ByteString
type MetaContent = Text


newtype Tagset = Tagset (S.Set MetaField)
    deriving (Eq, Show)


hasTag (Tagset s) x = x `S.member` s

mkTagset :: [MetaField] -> Tagset
mkTagset = Tagset . S.fromDistinctAscList . sort

tagsetTags :: Tagset -> [MetaField]
tagsetTags (Tagset s) = S.toAscList s



newtype Tags = Tags (M.Map MetaField MetaContent)
    deriving (Eq, Show)


mkTags :: [(MetaField, MetaContent)] -> Tags
mkTags = Tags . M.fromAscList . sortBy (compare `on` fst)

lookupTag :: MetaField -> Tags -> Maybe MetaContent
lookupTag t (Tags m) = M.lookup t m

tags :: Tags -> [(MetaField, MetaContent)]
tags (Tags m) = M.toAscList m


tag :: String -> MetaField
tag = B.pack

query :: String -> String -> (MetaField, MetaContent)
query a b = (B.pack a, T.pack b)


type Result = Either MPDError


instance Monad Result where
    return = Right

    Right x >>= f = f x
    Left x  >>= _ = Left x

    fail = Left . OtherError


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


