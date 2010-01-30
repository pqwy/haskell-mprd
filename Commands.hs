{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}


module Commands
    ( Command(..)
    , RangeLike

    , stats, status, clearerror, currentsong, unsafeIdle

    , consume, random, repeat, single
    , crossfade, setvol, next, previous, stop
    , pause, play, playid, seek, seekid 

    , add, addid, clear, move, moveid, delete, deleteid

    , playlistid, playlistidAll, playlistfind, playlistinfo, playlistinfo1
    , playlistsearch, plchanges, plchangesposid, shuffle, swap, swapid

    , count, find, list, listAlbumsByArtist, listall, listallinfo, lsinfo, search, update

    , listplaylists, listplaylist, listplaylistinfo, load, playlistadd
    , playlistclear, playlistdelete, playlistmove, rename, rm, save

    , close, kill, password, ping

    , disableoutput, enableoutput, outputs
    ) where




import Core
import Types
import Codec


import Prelude hiding ( repeat )


import Control.Monad
import Control.Applicative


import Data.Text ( Text, pack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B




data Command a = Command ByteString (Decoder a)



class (Parameter a) => RangeLike a where

instance RangeLike PlaylistPos where
instance RangeLike Range where


-- command builders {{{


command0 :: (Response a) => ByteString -> Command a
command0 s = command0with s decode

command1 :: (Parameter a, Response b) => ByteString -> a -> Command b
command1 s = command1with s decode

command2 :: (Parameter a, Parameter b, Response c) => ByteString -> a -> b -> Command c
command2 s a b = Command (joinParams [s, encode a, encode b]) decode

command3 :: (Parameter a, Parameter b, Parameter c, Response d)
         => ByteString -> a -> b -> c -> Command d
command3 s a b c = Command (joinParams [s, encode a, encode b, encode c]) decode


command0with :: ByteString -> Decoder a -> Command a
command0with s d = Command s d

command1with :: (Parameter a) => ByteString -> Decoder b -> a -> Command b
command1with s d a = Command (joinParams [s, encode a]) d

command1optWith :: (Parameter a) => ByteString -> Decoder b -> Maybe a -> Command b
command1optWith s d (Just x) = command1with s d x
command1optWith s d Nothing  = command0with s d

command1opt :: (Parameter a, Response b) => ByteString -> Maybe a -> Command b
command1opt s = command1optWith s decode

-- }}}



stats :: Command Stats
stats = command0 "stats"


status :: Command Status
status = command0 "status"


unsafeIdle :: [SubsysChanged] -> Command [SubsysChanged]
unsafeIdle = command1 "idle"


clearerror :: Command ()
clearerror = command0 "clearerror"


currentsong :: Command PlaylistTrack
currentsong = command0 "currentsong"


consume, random, repeat, single :: Bool -> Command ()

consume = command1 "consume"
random  = command1 "random"
repeat  = command1 "repeat"
single  = command1 "single"


crossfade, setvol :: Int -> Command ()

crossfade = command1 "crossfade"

setvol x | x >= 0 && x <= 100 = command1 "setvol" x
         | otherwise = error ("setvol: allowed range 0-100, got " ++ show x)


next, previous, stop :: Command ()
next     = command0 "next"
previous = command0 "previous"
stop     = command0 "stop"

pause :: Bool -> Command ()
pause = command1 "pause"

play :: PlaylistPos -> Command ()
play = command1 "play"

playid :: TrackID -> Command ()
playid = command1 "playid"


seek :: PlaylistPos -> Seconds -> Command ()
seek = command2 "seek"

seekid :: TrackID -> Seconds -> Command ()
seekid = command2 "seekid"




add :: URI -> Command ()
add = command1 "add"

addid :: URI -> Command TrackID
addid = command1 "addid"


clear :: Command ()
clear = command0 "clear"


delete :: (RangeLike i) => i -> Command ()
delete = command1 "delete"

deleteid :: TrackID -> Command ()
deleteid = command1 "deleteid"

move :: (RangeLike i) => i -> PlaylistPos -> Command ()
move = command2 "move"

moveid :: TrackID -> PlaylistPos -> Command ()
moveid = command2 "moveid"


playlistidAll :: Command [PlaylistTrack]
playlistidAll = command0 "playlistid"

playlistid :: TrackID -> Command PlaylistTrack
playlistid = command1 "playlistid"

playlistfind :: [QueryPred] -> Command [PlaylistTrack]
playlistfind = command1 "playlistfind"

playlistinfo :: Maybe Range -> Command [PlaylistTrack]
playlistinfo = command1opt "playlistinfo"

playlistinfo1 :: PlaylistPos -> Command PlaylistTrack
playlistinfo1 = command1 "playlistinfo"

playlistsearch :: [QueryPred] -> Command [PlaylistTrack]
playlistsearch = command1 "playlistsearch"

plchanges :: PlaylistVersion -> Command [PlaylistTrack]
plchanges = command1 "plchanges"


plchangesposid :: PlaylistVersion -> Command [(PlaylistPos, TrackID)]
plchangesposid = command1with "plchangesposid" decodePosIDs


shuffle :: Maybe Range -> Command ()
shuffle = command1opt "shuffle"


swap :: PlaylistPos -> PlaylistPos -> Command ()
swap = command2 "swap"

swapid :: TrackID -> TrackID -> Command ()
swapid = command2 "swap"



count :: [QueryPred] -> Command (Int, Seconds)
count = command1with "count" decodeSongsPltime


find :: [QueryPred] -> Command [Track]
find = command1 "find"

-- findadd??

list :: MetaField -> Command [MetaContent]
list t = Command (joinParams ["list", t]) (decodeSingleTags t)

listAlbumsByArtist :: MetaContent -> Command [MetaContent]
listAlbumsByArtist a =
    Command (joinParams ["list", "Album", encode a])
            (decodeSingleTags ("Album"))


listall :: Maybe URI -> Command [Either URI URI]
listall = command1optWith "listall" decodeDirsFiles

listallinfo :: Maybe URI -> Command [Either URI Track]
listallinfo = command1optWith "listallinfo" decodeDirsTracks

lsinfo :: Maybe URI -> Command [Either URI Track]
lsinfo = command1optWith "lsinfo" decodeDirsTracks

search :: [QueryPred] -> Command [Track]
search = command1 "search"


-- findadd?

update :: Maybe URI -> Command JobID
update = command1opt "update"

-- rescan??


listplaylist :: Playlist -> Command [URI]
listplaylist = command1with "listplaylist" decodeURIs


listplaylistinfo :: Playlist -> Command [Track]
listplaylistinfo = command1 "listplaylistinfo"


listplaylists :: Command [(Playlist, Text)]
listplaylists = command0with "listplaylists" decodePlaylists


load :: Playlist -> Command ()
load = command1 "load"


playlistadd :: Playlist -> URI -> Command ()
playlistadd = command2 "playlistadd"

playlistclear :: Playlist -> Command ()
playlistclear = command1 "playlistclear"

playlistdelete :: Playlist -> PlaylistPos -> Command ()
playlistdelete = command2 "playlistdelete"

playlistmove :: Playlist -> TrackID -> PlaylistPos -> Command ()
playlistmove = command3 "playlistmove"

rename :: Playlist -> Playlist -> Command ()
rename = command2 "rename"

rm :: Playlist -> Command ()
rm = command1 "rm"

save :: Command ()
save = command0 "save"



commands :: Command [Text]
commands = command0with "commands" decodeCommands

-- notcommands :: Command [Text] -- ???
-- notcommands = command0with "notcommands" decodeCo

tagtypes :: Command [MetaField]
tagtypes = command0with "tagtypes" decodeTagTypes

urlhandlers :: Command [Text]
urlhandlers = command0with "urlhandlers" decodeURLHandlers

-- decoders???



close :: Command ()
close = command0 "close"


kill :: Command ()
kill = command0 "kill"


password :: Text -> Command ()
password = command1 "password"


ping :: Command ()
ping = command0 "ping"



disableoutput, enableoutput :: OutputID -> Command ()

disableoutput = command1 "disableoutput"

enableoutput  = command1 "enableoutput"


outputs :: Command [Output]
outputs = command0 "outputs"



