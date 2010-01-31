{-# LANGUAGE TypeSynonymInstances, OverloadedStrings  #-}

module Core
    ( Text, ByteString
    , MetaField, MetaContent, Tagset, hasTag, mkTagset, tagsetTags
    , Tags, mkTags, lookupTag, tags
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



type Result = Either MPDError


instance Monad Result where
    return = Right

    Right x >>= f = f x
    Left x  >>= _ = Left x

    fail = Left . OtherError


-- instance MonadPlus Result where
--     Left _ `mplus` b = b
--     a `mplus` b = a
--     
--     mzero = Left (OtherError "mzero!")



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


