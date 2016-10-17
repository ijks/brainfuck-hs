module Tape
    ( Tape
    , empty
    , goLeft
    , goRight
    , currentCell
    , set
    , increment
    , decrement
    ) where

import Data.List (intercalate)

data Tape a = Tape [a] a [a]

instance Show a => Show (Tape a) where
    show (Tape l x r) =
        concat ["... ", trailing l, " ", show x, " ", trailing r, " ..."]
        where
            trailing = intercalate " " . take 10 . map show

empty :: Num a => Tape a
empty = Tape zeros 0 zeros
    where zeros = repeat 0

goRight :: Tape a -> Tape a
goRight (Tape ls x (r:rs)) = Tape (x:ls) r rs

goLeft :: Tape a -> Tape a
goLeft (Tape (l:ls) x rs) = Tape ls l (x:rs)

currentCell :: Tape a -> a
currentCell (Tape _ x _) = x

modify :: (a -> a) -> Tape a -> Tape a
modify f (Tape l x r) = Tape l (f x) r

set :: a -> Tape a -> Tape a
set x = modify (const x)

increment :: Num a => Tape a -> Tape a
increment = modify (+ 1)

decrement :: Num a => Tape a -> Tape a
decrement = modify (subtract 1)
