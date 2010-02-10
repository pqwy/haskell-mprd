module Main where

-- import Network.MPDMPD.Connection
import Network.MPDMPD.Monad
import Network.MPDMPD.Commands
import qualified Network.MPDMPD.Tags as TS
import Control.Monad.Trans

import Data.List

import Control.Applicative
import System.Environment


oneshotWith :: (Show a) => FilePath -> Command a -> IO ()
oneshotWith f c = 
    (attachMPDt f $ cmd c >>= lift . print) >> return ()


main1, main2, main3, main4 :: IO ()

main1 = "snippets/torture1.txt" `oneshotWith`
            (length <$> listall Nothing)

main2 = "snippets/torture3.txt" `oneshotWith`
            (length <$> listallinfo Nothing)

main3 = "snippets/manyping.txt" `oneshotWith` 
            (ping *> ping *> ping *> ping)

main4 = "snippets/torture3.txt" `oneshotWith` 
            (counter . mult 100 <$> listallinfo Nothing)
    where
        counter = flip foldl' 0 $ \a u ->
                    case u of
                         Left _ -> a
                         Right s -> maybe a (const (a+1)) $
                                TS.artist `lkpTag` s >> TS.album `lkpTag` s >> TS.title `lkpTag` s

        mult n = concat . replicate n

main = main4
