{-# LANGUAGE PackageImports  #-}
module Network.MPDMPD.Monad
    ( module Network.MPDMPD
    , module Control.Monad.Trans
    , MPDt, MPD
    , runMPDt, runMPDtEx, attachMPDt
    , cmd, cmds, protocolVersion
    ) where

import Network.MPDMPD hiding
            ( cmd, cmds, connect, connectEx, protocolVersion )
import qualified Network.MPDMPD.Connection as C

import Control.Monad
import "mtl" Control.Monad.Trans
import Control.Applicative

import Network


newtype MPDt m a = MPDt { unMPDt :: C.MPDConn -> m (Result a) }

type MPD = MPDt IO


instance (Monad m) => Monad (MPDt m) where
    return x = MPDt (\_ -> return (return x))
    MPDt a >>= f = MPDt $ \c ->
        a c >>= either (return . Left) (\x -> unMPDt (f x) c)

instance (Monad m) => MonadPlus (MPDt m) where
    mzero = MPDt (\_ -> return (Left $ OtherError ""))
    MPDt a `mplus` MPDt b = MPDt $ \c ->
        a c >>= either (\_ -> b c) (return . Right)

instance (Functor f) => Functor (MPDt f) where
    fmap f (MPDt a) = MPDt (fmap (fmap f) . a)

instance (Functor f, Monad f) => Applicative (MPDt f) where
    pure = return
    (<*>) = ap

instance (MonadIO m) => MonadIO (MPDt m) where
    liftIO a = MPDt (\_ -> Right `liftM` liftIO a)

instance MonadTrans MPDt where
    lift a = MPDt (\_ -> Right `liftM` a)


askConn :: (Monad m) => MPDt m C.MPDConn
askConn = MPDt (\c -> return (return c))

cmd :: (MonadIO m) => Command a -> MPDt m a
cmd k = MPDt (\c -> liftIO (C.cmd c k))

cmds :: (MonadIO m) => [Command a] -> MPDt m [a]
cmds ks = MPDt (\c -> liftIO (C.cmds c ks))

protocolVersion :: (Monad m) => MPDt m String
protocolVersion = C.protocolVersion `liftM` askConn


runMPDtEx :: (MonadIO m) => MPDt m a
          -> HostName -> Int -> Maybe String -> m (Result a)

runMPDtEx (MPDt a) hn p pw = do
    liftIO (C.connectEx hn p pw) >>= \mpd' ->
        case mpd' of
             Left e    -> return (Left e)
             Right mpd -> do
                 res <- a mpd
                 liftIO (C.close mpd)
                 return res

runMPDt :: (MonadIO m) => MPDt m a -> m (Result a)
runMPDt m = runMPDtEx m "localhost" 6600 Nothing

attachMPDt :: (MonadIO m) => FilePath -> MPDt m a -> m (Result a)
attachMPDt fp (MPDt a) = liftIO (attachFile fp) >>= \(Right c) -> a c

