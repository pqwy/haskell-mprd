{-# LANGUAGE PatternGuards  #-}


module Codec where


import Core

import qualified Data.Text as T

import Control.Monad
import Control.Applicative


data DecoderM s r a = DecoderM { unDecoderM :: s -> (MPDError -> r) -> (s -> a -> r) -> r }


instance Monad (DecoderM s r) where
    return a = DecoderM (\s k1 k2 -> k2 s a)
    DecoderM m >>= f = DecoderM $ \s k1 k2 ->
        m s k1 (\s' a -> unDecoderM (f a) s' k1 k2)
    fail = throwD Nothing


instance MonadPlus (DecoderM s r) where
    mzero = fail "mzero"
    DecoderM a `mplus` DecoderM b = DecoderM $ \s k1 k2 ->
        flip (a s) k2 $ \_ -> b s k1 k2

instance Functor (DecoderM s r) where
    fmap = liftM

instance Applicative (DecoderM s r) where
    pure  = return
    (<*>) = ap

throwD :: Maybe Text -> String -> DecoderM s r a
throwD tx e = DecoderM (\_ k1 _ -> k1 (DecodeError e tx))

getD :: DecoderM s r s
getD = DecoderM (\s _ k2 -> k2 s s)

putD :: s -> DecoderM s r ()
putD s = DecoderM (\_ _ k2 -> k2 s ())

ground :: DecoderM s (Result a) a -> s -> Result a
ground m s = unDecoderM m s Left (const Right)


mapError :: DecoderM s r a -> (String -> Maybe Text -> (String, Maybe Text)) -> DecoderM s r a
mapError (DecoderM f) g = DecoderM $ \s k1 k2 -> f s (k1 . g') k2
    where g' (DecodeError s tx) = uncurry DecodeError (g s tx)


tok :: DecoderM [s] r s
tok = getD >>= \ts ->
    case ts of
         []      -> fail "end of input"
         (x:tts) -> putD tts >> return x

sat :: (Text -> Bool) -> DecoderM [Text] r Text
sat p = tok >>= \t -> if p t then return t else throwD (Just t) "something else"


splitKV :: Text -> DecoderM s r (Text, Text)
splitKV t | (k, v) <- split t, T.length v > 2 = return (k, T.drop 2 v)
          | otherwise = throwD (Just t) ("\"key: value\" pair")
    where
        split = T.break (T.pack ": ")


key :: String -> DecoderM [Text] r Text
key k = (sat (T.isPrefixOf k') >>= fmap snd . splitKV)
            `mapError` (\_ t -> ("key " ++ k, t))
    where k' = T.pack (k ++ ": ")


nextKV :: DecoderM [Text] r (Text, Text)
nextKV = tok >>= splitKV


