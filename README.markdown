# The Music Player reDeemon library #

> FTW!

### reDeemon? ###

It's much faster than the other Haskell lib, and IMHO, better organized.
The other one was beyond any redemption.

### Get started. ###

With


    import Network.MPDMPD
    import Control.Applicative
    
    main = connect >>= \c' ->
            case c' of
                e@(Left _) -> return ()
                Right c -> do
                    cmd c ( map (\p -> ( plTrackPos p, trackFile (plTrack p) )
                                <$> playlistidAll )
                       >>= mapM_ print
                    cmd c (play 1)
                    close c


...oooor...


    import Network.MPDMPD.Monad
    import qualified Network.MPDMPD.Tags as TS
    import Control.Monad.Trans
    
    import Control.Applicative
    import Data.Traversable
    
    main = runMPDt $ do
        cmd ( map (\p -> ( plTrackPos p, trackFile (plTrack p) ))
                 <$> playlistidAll )
            >>= lift . mapM_ print
    
        tracks <- cmd ( (++) <$> search [TS.artist <?> "Alan Vega"]
                             <*> search [TS.artist <?> "Suicide"] )
    
        first <- cmd ( head <$> traverse (addid . trackFile) tracks )
    
        cmd $ playid first


### Layout ###

`Commands.hs`  
Primitive MPD-level commands. They form an Applicative, emitted using `command_list_begin` when combined.

`Connection.hs`  
    cmd :: Command a -> IO a

`Monad.hs`  
A simple reader-over-either for managing the connection and MPD-related errors.

`Types.hs`  
Interface types.

`Tags.hs`  
Tags behave like free-form tags, only faster.


