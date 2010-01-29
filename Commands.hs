{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}


module Commands
--     ( Command(..)
--     , RangeLike
-- 
--     , stats, status, clearerror, currentsong
--     , consume, random, repeat, single
--     , crossfade, setvol, next, previous, stop
--     , pause, play, playid, seek, seekid 
--     , add, addid, clear, move, moveid, delete, deleteid
--     , playlistid, playlistidAll, playlistfind, playlistinfo, playlistinfo1
--     , playlistsearch, plchanges, plchangesposid, shuffle, swap, swapid
-- 
--     ) where
where


import Core
import Types
import Codec


import Prelude hiding ( readList, repeat )


import Control.Monad
import Control.Applicative


import Data.Text ( Text, pack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E




data Command a = Command Text (Decoder a)



class (Parameter a) => RangeLike a where

instance RangeLike PlaylistPos where
instance RangeLike Range where


-- command builders {{{


command0 :: (Response a) => String -> Command a
command0 s = command0with s decode

command1 :: (Parameter a, Response b) => String -> a -> Command b
command1 s = command1with s decode

command2 :: (Parameter a, Parameter b, Response c) => String -> a -> b -> Command c
command2 s a b = Command (joinParams [pack s, encode a, encode b]) decode

command0with :: String -> Decoder a -> Command a
command0with s d = Command (pack s) d

command1with :: (Parameter a) => String -> Decoder b -> a -> Command b
command1with s d a = Command (joinParams [pack s, encode a]) d

command1optWith :: (Parameter a) => String -> Decoder b -> Maybe a -> Command b
command1optWith s d (Just x) = command1with s d x
command1optWith s d Nothing  = command0with s d

command1opt :: (Parameter a, Response b) => String -> Maybe a -> Command b
command1opt s = command1optWith s decode

-- }}}



stats :: Command Stats
stats = command0 "stats"


status :: Command Status
status = command0 "status"



-- idleTextMap = map (first pack)
-- 
--     [ ("database"       , IdleDatabase      ) 
--     , ("mixer"          , IdleMixer         ) 
--     , ("options"        , IdleOptions       ) 
--     , ("output"         , IdleOutput        ) 
--     , ("player"         , IdlePlayer        ) 
--     , ("playlist"       , IdlePlaylist      ) 
--     , ("stored_playlist", IdleStoredPlaylist) 
--     , ("update"         , IdleUpdate        ) 
--     ]



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
         | otherwise = error $ "setvol: allowed range 0-100, got " ++ show x


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
plchangesposid = command1with "plchangesposid" readPosIDs


shuffle :: Maybe Range -> Command ()
shuffle = command1opt "shuffle"


swap :: PlaylistPos -> PlaylistPos -> Command ()
swap = command2 "swap"

swapid :: TrackID -> TrackID -> Command ()
swapid = command2 "swap"



count :: [QueryPred] -> Command (Int, Seconds)
count = command1with "count" readSongsPltime


find :: [QueryPred] -> Command [Track]
find = command1 "find"

-- findadd??

list :: Tag -> Command [Text]
list t = Command (joinParams [pack "list", t]) (readSingleTags t)

listAlbumsByArtist :: Text -> Command [Text]
listAlbumsByArtist a =
        Command (joinParams [pack "list", album, encode a]) (readSingleTags album)
    where album = pack "Album"


listall :: Maybe URI -> Command [Either Text Text]
listall = command1optWith "listall" readDirsFiles

listallinfo :: Maybe URI -> Command [Either Text Track]
listallinfo = command1optWith "listallinfo" readDirsTracks

lsinfo :: Maybe URI -> Command [Either Text Track]
lsinfo = command1optWith "lsinfo" readDirsTracks

search :: [QueryPred] -> Command [Track]
search = command1 "search"


-- findadd?

update :: Maybe URI -> Command JobID
update = command1opt "update"

-- rescan??


