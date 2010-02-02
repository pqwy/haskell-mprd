{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances  #-}


module Network.MPDMPD.Commands
    ( Command(..), RangeLike

    , stats, status, clearerror, currentsong, unsafeIdle

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
    , MetaField(..), MetaContent
    , URI, QueryPred, Seconds, PlaylistPos, PlaylistVersion, TrackID, JobID, Output, OutputID
    , Stats, Status, SubsysChanged, Track, PlaylistTrack, Playlist, Range
    )

import Network.MPDMPD.Codec
    ( Decoder, Parameter(..), Response(..), joinParams, (<+>)

    , decodePosIDs, decodeSongsPltime, decodeSingleTags, decodeDirsFiles, decodeDirsTracks
    , decodeTagTypes, decodeURLHandlers, decodeCommands, decodeURIs, decodePlaylists
    , decodeSticker, decodeStickers, decodeStickersFiles
    )


import Prelude hiding ( repeat )



data Command a = Command ByteString (Decoder a)


class (Parameter a) => RangeLike a where

instance RangeLike PlaylistPos where
instance RangeLike Range where



class CmdBuilder c a | c -> a where
    commandWith :: ByteString -> Decoder a -> c

instance CmdBuilder (Command a) a where
    commandWith s d = Command s d

instance (CmdBuilder c a, Parameter p) => CmdBuilder (p -> c) a where
    commandWith s d p = commandWith (s <+> encode p) d


command :: (CmdBuilder a b, Response b) => ByteString -> a
command s = commandWith s decode



-- querying {{{

stats :: Command Stats
stats = command "stats"

status :: Command Status
status = command "status"

unsafeIdle :: [SubsysChanged] -> Command [SubsysChanged]
unsafeIdle = command "idle"

clearerror :: Command ()
clearerror = command "clearerror"

currentsong :: Command PlaylistTrack
currentsong = command "currentsong"

-- }}}

-- playback options {{{

consume, random, repeat, single :: Bool -> Command ()

consume = command "consume"
random  = command "random"
repeat  = command "repeat"
single  = command "single"

crossfade, setvol :: Int -> Command ()

crossfade = command "crossfade"

setvol x | x >= 0 && x <= 100 = command "setvol" x
         | otherwise = error ("setvol: allowed range 0-100, got " ++ show x)

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
playid = command "playid"

seek :: PlaylistPos -> Seconds -> Command ()
seek = command "seek"

seekid :: TrackID -> Seconds -> Command ()
seekid = command "seekid"

-- }}}

-- current playlist {{{

add :: URI -> Command ()
add = command "add"

addid :: URI -> Command TrackID
addid = command "addid"

clear :: Command ()
clear = command "clear"

delete :: (RangeLike i) => i -> Command ()
delete = command "delete"

deleteid :: TrackID -> Command ()
deleteid = command "deleteid"

move :: (RangeLike i) => i -> PlaylistPos -> Command ()
move = command "move"

moveid :: TrackID -> PlaylistPos -> Command ()
moveid = command "moveid"

playlistidAll :: Command [PlaylistTrack]
playlistidAll = command "playlistid"

playlistid :: TrackID -> Command PlaylistTrack
playlistid = command "playlistid"

playlistfind :: [QueryPred] -> Command [PlaylistTrack]
playlistfind = command "playlistfind"

playlistinfo :: Maybe Range -> Command [PlaylistTrack]
playlistinfo = command "playlistinfo"

playlistinfo1 :: PlaylistPos -> Command PlaylistTrack
playlistinfo1 = command "playlistinfo"

playlistsearch :: [QueryPred] -> Command [PlaylistTrack]
playlistsearch = command "playlistsearch"

plchanges :: PlaylistVersion -> Command [PlaylistTrack]
plchanges = command "plchanges"

plchangesposid :: PlaylistVersion -> Command [(PlaylistPos, TrackID)]
plchangesposid = commandWith "plchangesposid" decodePosIDs

shuffle :: Maybe Range -> Command ()
shuffle = command "shuffle"

swap :: PlaylistPos -> PlaylistPos -> Command ()
swap = command "swap"

swapid :: TrackID -> TrackID -> Command ()
swapid = command "swapid"

-- }}}

-- stored playlists {{{

listplaylist :: Playlist -> Command [URI]
listplaylist = commandWith "listplaylist" decodeURIs

listplaylistinfo :: Playlist -> Command [Track]
listplaylistinfo = command "listplaylistinfo"

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
playlistmove = command "playlistmove"

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
find = command "find"

-- findadd??

list :: MetaField -> Command [MetaContent]
list (MF t) = Command (joinParams ["list", t]) (decodeSingleTags t)

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
search = command "search"

update :: Maybe URI -> Command JobID
update = command "update"

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
disableoutput = command "disableoutput"
enableoutput  = command "enableoutput"

outputs :: Command [Output]
outputs = command "outputs"

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


