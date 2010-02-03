module Network.MPDMPD.Misc
    ( first, second
    ) where


first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a, b) = (a, f b)

