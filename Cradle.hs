{-# LANGUAGE PackageImports  #-}
module Main where

-- import Network.MPDMPD.Connection
import Network.MPDMPD.Monad
import Network.MPDMPD.Commands
import qualified Network.MPDMPD.Tags as TS
import "mtl" Control.Monad.Trans

import Data.List

import Control.Applicative
import System.Environment


main1, main2, main3, main4 :: IO ()

main1 = do attachMPDt "snippets/torture1.txt" $
            cmd (length <$> listall Nothing) >>= lift . print
           return ()

main2 = do attachMPDt "snippets/torture3.txt" $
            cmd (length <$> listallinfo Nothing) >>= lift . print
           return ()

main3 = do attachMPDt "snippets/manyping.txt" $
            cmd (ping *> ping *> ping *> ping) >>= lift . print
           return ()

main4 = do attachMPDt "snippets/torture3.txt" $
            cmd (counter . mult 100 <$> listallinfo Nothing) >>= lift . print
           return ()
    where
        counter = flip foldl' 0 $ \a u ->
                    case u of
                         Left _ -> a
                         Right s -> maybe a (const (a+1)) $
                                TS.artist `lkpTag` s >> TS.album `lkpTag` s >> TS.title `lkpTag` s

        mult n = concat . replicate n

main = main4
