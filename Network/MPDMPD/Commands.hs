{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}


module Network.MPDMPD.Commands
    ( Command(..), RangeLike

    , stats, status, clearerror, currentsong, idle

    , consume, random, repeat, single, crossfade, setvol

    , next, previous, stop, pause, play, playid, seek, seekid 

    , add, addid, clear, move, moveid, delete, deleteid
    , playlistid, playlistidAll, playlistfind, playlistinfo, playlistinfo1
    , playlistsearch, plchanges, plchangesposid, shuffle, swap, swapid

    , listplaylists, listplaylist, listplaylistinfo, load, playlistadd
    , playlistclear, playlistdelete, playlistmove, rename, rm, save

    , count, find, list, listAlbumsByArtist, listall, listallinfo, lsinfo, search, update

    , stickerGet, stickerSet, stickerDelete, stickerList, stickerFind

    , unsafeClose, unsafeKill, password, ping

    , disableoutput, enableoutput, outputs

    , commands, notcommands, tagtypes, urlhandlers

    ) where


import Network.MPDMPD.Types
    ( Text, ByteString
    , MetaField, MetaContent
    , URI(..), QueryPred, Seconds, PlaylistPos, PlaylistVersion(..), TrackID(..), JobID(..)
    , Output(..), OutputID(..), Stats, Status, SubsysChanged, Track, PlaylistTrack, Playlist, Range
    )
import Network.MPDMPD.Tags
import Network.MPDMPD.Codec
import Network.MPDMPD.Misc


import Prelude hiding ( repeat )

import Control.Monad
import Control.Applicative


data Command a = Command ByteString (Decoder a)
               | Commands ([ByteString] -> [ByteString]) (Decoder a)


instance Functor Command where
    fmap f (Command  x d) = Command  x (fmap (first f) . d)
    fmap f (Commands x d) = Commands x (fmap (first f) . d)


instance Applicative Command where
    pure = Command "" . nullDecoder

    Command ax ad <*> b = Commands (ax:) ad <*> b
    a <*> Command bx bd = a <*> Commands (bx:) bd
    Commands ax ad <*> Commands bx bd =
        Commands (ax . bx) (ad >=> \(a', bss') -> first (a'$) <$> bd bss')


class CmdBuilder c where
    type Res c :: *
    commandWith :: ByteString -> Decoder (Res c) -> c

instance CmdBuilder (Command a) where
    type Res (Command a) = a
    commandWith s d = Command s d

instance (CmdBuilder c, Parameter p) => CmdBuilder (p -> c) where
    type Res (p -> c) = Res c
    commandWith s d p = commandWith (s <+> encode p) d


command ::  (CmdBuilder c, Res c ~ ()) => ByteString -> c
command s = commandWith s (nullDecoder ())


class (Parameter a) => RangeLike a where

instance RangeLike PlaylistPos where
instance RangeLike Range where


-- querying {{{

stats :: Command Stats
stats = commandWith "stats" decodeStats

status :: Command Status
status = commandWith "status" decodeStatus

idle :: [SubsysChanged] -> Command [SubsysChanged]
idle = commandWith "idle" decodeSubsysChanged

clearerror :: Command ()
clearerror = command "clearerror"

currentsong :: Command PlaylistTrack
currentsong = commandWith "currentsong" decodePLTrack
-- }}}

-- playback options {{{

consume, random, repeat, single :: Bool -> Command ()

consume = command "consume"
random  = command "random"
repeat  = command "repeat"
single  = command "single"

crossfade, setvol :: Int -> Command ()

crossfade = command "crossfade"
setvol    = command "setvol"
-- }}}

-- controlling playback {{{

next, previous, stop :: Command ()
next     = command "next"
previous = command "previous"
stop     = command "stop"

pause :: Bool -> Command ()
pause = command "pause"

play :: PlaylistPos -> Command ()
play = command "play"

playid :: TrackID -> Command ()
playid (TID t) = command "playid" t

seek :: PlaylistPos -> Seconds -> Command ()
seek = command "seek"

seekid :: TrackID -> Seconds -> Command ()
seekid (TID t) = command "seekid" t
-- }}}

-- current playlist {{{

add :: URI -> Command ()
add = command "add"

addid :: URI -> Command TrackID
addid = commandWith "addid" decodeTID

clear :: Command ()
clear = command "clear"

delete :: (RangeLike i) => i -> Command ()
delete = command "delete"

deleteid :: TrackID -> Command ()
deleteid (TID t) = command "deleteid" t

move :: (RangeLike i) => i -> PlaylistPos -> Command ()
move = command "move"

moveid :: TrackID -> PlaylistPos -> Command ()
moveid (TID t) = command "moveid" t

playlistidAll :: Command [PlaylistTrack]
playlistidAll = commandWith "playlistid" decodePLTracks

playlistid :: TrackID -> Command PlaylistTrack
playlistid (TID t) = commandWith "playlistid" decodePLTrack t

playlistfind :: [QueryPred] -> Command [PlaylistTrack]
playlistfind = commandWith "playlistfind" decodePLTracks

playlistinfo :: Maybe Range -> Command [PlaylistTrack]
playlistinfo = commandWith "playlistinfo" decodePLTracks

playlistinfo1 :: PlaylistPos -> Command PlaylistTrack
playlistinfo1 = commandWith "playlistinfo" decodePLTrack

playlistsearch :: [QueryPred] -> Command [PlaylistTrack]
playlistsearch = commandWith "playlistsearch" decodePLTracks

plchanges :: PlaylistVersion -> Command [PlaylistTrack]
plchanges (PLV p) = commandWith "plchanges" decodePLTracks p

plchangesposid :: PlaylistVersion -> Command [(PlaylistPos, TrackID)]
plchangesposid (PLV p) = commandWith "plchangesposid" decodePosIDs p

shuffle :: Maybe Range -> Command ()
shuffle = command "shuffle"

swap :: PlaylistPos -> PlaylistPos -> Command ()
swap = command "swap"

swapid :: TrackID -> TrackID -> Command ()
swapid (TID t1) (TID t2) = command "swapid" t1 t2
-- }}}

-- stored playlists {{{

listplaylist :: Playlist -> Command [URI]
listplaylist = commandWith "listplaylist" decodeURIs

listplaylistinfo :: Playlist -> Command [Track]
listplaylistinfo = commandWith "listplaylistinfo" decodeTracks

-- XXX time type.
listplaylists :: Command [(Playlist, Text)]
listplaylists = commandWith "listplaylists" decodePlaylists

load :: Playlist -> Command ()
load = command "load"

playlistadd :: Playlist -> URI -> Command ()
playlistadd = command "playlistadd"

playlistclear :: Playlist -> Command ()
playlistclear = command "playlistclear"

playlistdelete :: Playlist -> PlaylistPos -> Command ()
playlistdelete = command "playlistdelete"

playlistmove :: Playlist -> TrackID -> PlaylistPos -> Command ()
playlistmove p (TID t) = command "playlistmove" p t

rename :: Playlist -> Playlist -> Command ()
rename = command "rename"

rm :: Playlist -> Command ()
rm = command "rm"

save :: Playlist -> Command ()
save = command "save"
-- }}}

-- music database {{{

count :: [QueryPred] -> Command (Int, Seconds)
count = commandWith "count" decodeSongsPltime

find :: [QueryPred] -> Command [Track]
find = commandWith "find" decodeTracks

-- findadd??

list :: MetaField -> Command [MetaContent]
list t = Command (joinParams ["list", t']) (decodeSingleTags t')
    where t' = encode t

listAlbumsByArtist :: MetaContent -> Command [MetaContent]
listAlbumsByArtist a =
    Command (joinParams ["list", "Album", encode a])
            (decodeSingleTags ("Album"))

listall :: Maybe URI -> Command [Either URI URI]
listall = commandWith "listall" decodeDirsFiles

listallinfo :: Maybe URI -> Command [Either URI Track]
listallinfo = commandWith "listallinfo" decodeDirsTracks

lsinfo :: Maybe URI -> Command [Either URI Track]
lsinfo = commandWith "lsinfo" decodeDirsTracks

search :: [QueryPred] -> Command [Track]
search = commandWith "search" decodeTracks

update :: Maybe URI -> Command JobID
update = commandWith "update" decodeJID

-- rescan??
-- }}}

-- stickers {{{

stickerGet :: URI -> Text -> Command Text
stickerGet = commandWith "sticker get song" decodeSticker

stickerSet :: URI -> Text -> Text -> Command ()
stickerSet = command "sticker set song"

stickerDelete :: URI -> Maybe Text -> Command ()
stickerDelete = command "sticker delete song"

stickerList :: URI -> Command [(Text, Text)]
stickerList = commandWith "sticker list song" decodeStickers

stickerFind :: URI -> Text -> Command [(URI, Text)]
stickerFind = commandWith "sticker find song" decodeStickersFiles
-- }}}

-- connection {{{

unsafeClose, unsafeKill, ping :: Command ()

unsafeKill  = command "kill"
unsafeClose = command "close"
ping        = command "ping"

password :: String -> Command ()
password = command "password"
-- }}}

-- outputs {{{

disableoutput, enableoutput :: OutputID -> Command ()
disableoutput (OutputID o) = command "disableoutput" o
enableoutput (OutputID o)  = command "enableoutput" o

outputs :: Command [Output]
outputs = commandWith "outputs" decodeOutputs
-- }}}

-- reflection {{{

commands :: Command [String]
commands = commandWith "commands" decodeCommands

notcommands :: Command [String]
notcommands = commandWith "notcommands" decodeCommands

tagtypes :: Command [MetaField]
tagtypes = commandWith "tagtypes" decodeTagTypes

urlhandlers :: Command [String]
urlhandlers = commandWith "urlhandlers" decodeURLHandlers

-- decoders???
-- }}}

-- vim:set fdm=marker:
