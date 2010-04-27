{-# LANGUAGE OverloadedStrings  #-}

module Network.MPrD.Tags
    ( MetaField, MetaContent

    , Tags, mkTags

    , metafieldToString, metafieldToBytestring
    , stringToMetafield, bytestringToMetafield
    , isPackedMetafield

    , artist, album, albumArtist, title, track, name
    , genre, date, composer, performer, disc

    ) where



import GHC.Exts ( IsString(..) )

import Data.Function ( on )
import Data.List ( sortBy )
import qualified Data.Map as M

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B


type MetaContent = Text


data MetaField = Artist | Album | AlbumArtist | Title | Track
               | Name | Genre | Date | Composer | Performer | Disc
               | MusicbrainzArtistid | MusicbrainzAlbumid | MusicbrainzAlbumartistid | MusicbrainzTrackid
               | MetaOther Text
    deriving (Eq, Ord)


mfstr :: (IsString s) => (Text -> s) -> MetaField -> s
mfstr _ Artist      = "Artist"
mfstr _ Album       = "Album"
mfstr _ AlbumArtist = "AlbumArtist"
mfstr _ Title       = "Title"
mfstr _ Track       = "Track"
mfstr _ Name        = "Name"
mfstr _ Genre       = "Genre"
mfstr _ Date        = "Date"
mfstr _ Composer    = "Composer"
mfstr _ Performer   = "Performer"
mfstr _ Disc        = "Disc"

mfstr _ MusicbrainzArtistid      = "MUSICBRAINZ_ARTISTID"
mfstr _ MusicbrainzAlbumid       = "MUSICBRAINZ_ALBUMID"
mfstr _ MusicbrainzAlbumartistid = "MUSICBRAINZ_ALBUMARTISTID"
mfstr _ MusicbrainzTrackid       = "MUSICBRAINZ_TRACKID"

mfstr f (MetaOther t) = f t

strmf :: (IsString s, Eq s) => (s -> Text) -> s -> MetaField
strmf _ "Artist"      = Artist
strmf _ "Album"       = Album
strmf _ "AlbumArtist" = AlbumArtist
strmf _ "Title"       = Title
strmf _ "Track"       = Track
strmf _ "Name"        = Name
strmf _ "Genre"       = Genre
strmf _ "Date"        = Date
strmf _ "Composer"    = Composer
strmf _ "Performer"   = Performer
strmf _ "Disc"        = Disc

strmf _ "MUSICBRAINZ_ARTISTID"      = MusicbrainzArtistid
strmf _ "MUSICBRAINZ_ALBUMID"       = MusicbrainzAlbumid
strmf _ "MUSICBRAINZ_ALBUMARTISTID" = MusicbrainzAlbumartistid
strmf _ "MUSICBRAINZ_TRACKID"       = MusicbrainzTrackid

strmf f t = MetaOther (f t)


metafieldToString :: MetaField -> String
metafieldToString = mfstr T.unpack

metafieldToBytestring :: MetaField -> B.ByteString
metafieldToBytestring = mfstr E.encodeUtf8

stringToMetafield :: String -> MetaField
stringToMetafield = strmf T.pack

bytestringToMetafield :: B.ByteString -> MetaField
bytestringToMetafield = strmf E.decodeUtf8


isPackedMetafield :: MetaField -> Bool
isPackedMetafield (MetaOther _) = False
isPackedMetafield _             = True


instance IsString MetaField where
    fromString = stringToMetafield

instance Show MetaField where
    show = metafieldToString



artist, album, albumArtist, title, track, name, genre, date, composer, performer, disc :: MetaField

artist      = Artist
album       = Album
albumArtist = AlbumArtist
title       = Title
track       = Track
name        = Name
genre       = Genre
date        = Date
composer    = Composer
performer   = Performer
disc        = Disc



type Tags = M.Map MetaField MetaContent

mkTags :: [(MetaField, MetaContent)] -> Tags
mkTags = M.fromAscList . sortBy (compare `on` fst)


