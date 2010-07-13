
The Music Player reDeemon library
---------------------------------

> FTW!


### reDeemon? ###

It's much faster than the other Haskell lib, and IMHO, better organized.
The other one was beyond any redemption.


### Get started. ###

With

    import Network.MPrD
    import Control.Applicative ( (<$>) )

    main = connect >>= \c' ->
            case c' of
                e@(Left _) -> return ()
                Right c -> do
                    cmd c posTrack >>= either (\_ -> return ()) (mapM_ print)
                    cmd c (play 1)
                    close c

                    return ()
    where
        posTrack = map (\p -> (plTrackPos p, (trackFile . plTrack) p))
                    <$> playlistidAll


... oooor ...

    import qualified Network.MPrD.Commands as Cmd
    import Network.MPrD.Monad
    import qualified Network.MPrD.Tags as TAG
    import "mtl" Control.Monad.Trans ( lift )

    import Control.Applicative ( Applicative(..), (<$>) )
    import Data.Traversable ( Traversable(..) )

    main = runMPDt $ do
        cmd posTrack >>= lift . mapM_ print

        tracks <- cmd ( (++) <$> Cmd.search [TAG.artist <?> "Alan Vega"]
                             <*> Cmd.search [TAG.artist <?> "Suicide"] )

        first <- cmd ( head <$> traverse (Cmd.addid . trackFile) tracks )

        cmd (Cmd.playid first)

      where
        posTrack = map (\p -> ( plTrackPos p, trackFile (plTrack p) ))
                        <$> Cmd.playlistidAll


### Layout ###

* `Commands.hs`  

  Primitive MPD-level commands. They form an Applicative, emitted using `command_list_begin` when combined.

* `Connection.hs`  

      cmd :: Command a -> IO a

* `Monad.hs`  

  A simple reader-over-either for managing the connection and MPD-related errors.

* `Types.hs`  

  Interface types.

* `Tags.hs`  

  Tags behave like free-form tags, only faster.


