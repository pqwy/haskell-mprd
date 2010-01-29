{-# LANGUAGE TypeSynonymInstances  #-}

module Core
    ( Text
    , Tag, Tagset, hasTag, mkTagset, tagsetTags
    , Tags, mkTags, lookupTag, tags
    , MPDError(..), Result --, mapError
    , MPDConnState(..), zeroState
    , deftags
    ) where


import Control.Monad ( MonadPlus(..) )

import Data.List ( sort )

import Data.Text ( Text, pack )
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

import System.IO ( Handle )


data MPDConnState = MPDConnState
        { mpdConn :: Handle
        , mpdTags :: Tagset
        }

-- zeroState = MPDConnState { mpdConn = undefined , mpdTags = mkTagset [] }
deftags = map pack
    [ "Artist"   , "Album" , "AlbumArtist"
    , "Title"    , "Track" , "Name"
    , "Genre"    , "Date"  , "Composer"
    , "Performer", "Disc"
    , "MUSICBRAINZ_ARTISTID", "MUSICBRAINZ_ALBUMID"
    , "MUSICBRAINZ_ALBUMARTISTID", "MUSICBRAINZ_TRACKID"
    ]

zeroState = MPDConnState { mpdConn = undefined , mpdTags = mkTagset deftags }



data MPDError = DecodeError String (Maybe Text)
              | DecodeError2 Int
              | ErrIdling
              | OtherError String

    deriving (Eq, Show)




type Tag = Text

newtype Tagset = Tagset (S.Set Tag)
    deriving (Eq, Show)

hasTag (Tagset s) x = x `S.member` s

mkTagset :: [Tag] -> Tagset
mkTagset = Tagset . S.fromDistinctAscList . sort

tagsetTags :: Tagset -> [Tag]
tagsetTags (Tagset s) = S.toAscList s



newtype Tags = Tags (M.Map Tag Text)
    deriving (Eq, Show)

mkTags :: [(Tag, Text)] -> Tags
mkTags = Tags . M.fromList -- XXX may be slow, might require pre-sorting

lookupTag :: Tag -> Tags -> Maybe Text
lookupTag t (Tags m) = M.lookup t m

tags :: Tags -> [(Tag, Text)]
tags (Tags m) = M.toAscList m



type Result = Either MPDError


instance Monad Result where
    return = Right

    Right x >>= f = f x
    Left x  >>= _ = Left x

    fail = Left . OtherError


instance MonadPlus Result where
    Left _ `mplus` b = b
    a `mplus` b = a
    
    mzero = Left (OtherError "mzero!")


-- mapError :: (a -> b) -> Either a c -> Either b c
-- mapError _ (Right x) = Right x
-- mapError f (Left  x) = Left (f x)

