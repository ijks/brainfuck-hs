module Unsigned
    ( Unsigned(..)
    ) where

import Data.Char (chr, ord)
import Data.Word
import Numeric.Natural

class Integral a => Unsigned a where
    fromChar :: Char -> a
    fromChar = fromIntegral . ord

    toChar :: a -> Char
    toChar = chr . fromIntegral

instance Unsigned Word where

instance Unsigned Word8 where

instance Unsigned Word16 where

instance Unsigned Word32 where

instance Unsigned Word64 where

instance Unsigned Natural where
