module MonadChar
    ( MonadChar(..)
    ) where

class Monad m => MonadChar m where
    putC :: Char -> m ()
    getC :: m Char

instance MonadChar IO where
    putC = putChar
    getC = getChar
