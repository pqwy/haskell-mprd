module Tags
    ( Text, ByteString
    , MetaField(..), MetaContent
    , tag, Tags, mkTags

    , artist, album, albumArtist, title, track, name
    , genre, date, composer, performer, disc
    ) where



import Data.Function ( on )
import Data.List ( sortBy )
import qualified Data.Map as M

import Data.Text ( Text )
import qualified Data.Text as T
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as B


type MetaContent = Text

newtype MetaField = MF ByteString
    deriving (Eq, Ord)

instance Show MetaField where
    show (MF b) = B.unpack b


tag :: String -> MetaField
tag = MF . B.pack


artist      = tag "Artist"
album       = tag "Album"
albumArtist = tag "AlbumArtist"
title       = tag "Title"
track       = tag "Track"
name        = tag "Name"
genre       = tag "Genre"
date        = tag "Date"
composer    = tag "Composer"
performer   = tag "Performer"
disc        = tag "Disc"



type Tags = M.Map MetaField MetaContent

mkTags :: [(MetaField, MetaContent)] -> Tags
mkTags = M.fromAscList . sortBy (compare `on` fst)


