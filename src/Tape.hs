module Tape
    ( Tape
    , infinite
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

infinite :: Num a => Tape a
infinite = Tape zeros 0 zeros
    where zeros = repeat 0

infiniteRight :: Num a => Tape a
infiniteRight = Tape [] 0 (repeat 0)

bounded :: Num a => Int -> Int -> Tape a
bounded l r = Tape left 0 right
    where
        left = replicate l 0
        right = replicate r 0

boundedRight :: Num a => Int -> Tape a
boundedRight = bounded 0

goRight :: Tape a -> Maybe (Tape a)
goRight (Tape _ _ []) = Nothing
goRight (Tape ls x (r:rs)) = Just $ Tape (x:ls) r rs

goLeft :: Tape a -> Maybe (Tape a)
goLeft (Tape [] _ _) = Nothing
goLeft (Tape (l:ls) x rs) = Just $ Tape ls l (x:rs)

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
