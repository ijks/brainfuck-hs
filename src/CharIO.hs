module CharIO
    ( CharIO
    , ListIO(..)
    ) where

import Data.Char (chr, ord)
import Data.List (uncons)

import Unsigned

class CharIO a where
    get :: Unsigned u => a -> Maybe u
    put :: Unsigned u => u -> a -> a

data ListIO = ListIO { input :: [Char], output :: [Char] }

instance CharIO ListIO where
    get = fmap (fromChar . fst) . uncons . input
    put char (ListIO inp outp) = ListIO inp (toChar char : outp)
