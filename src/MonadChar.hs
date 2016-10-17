{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadChar
    ( MonadChar(..)
    , Numeric(..)
    ) where

import Data.Char (ord)

class Monad m => MonadChar m where
    putC :: Char -> m ()
    getC :: m Char

instance MonadChar IO where
    putC = putChar
    getC = getChar

newtype Numeric a = Numeric { runNumeric :: IO a }
    deriving (Functor, Applicative, Monad)

instance MonadChar Numeric where
    putC = Numeric . print . ord
    getC = Numeric getChar
