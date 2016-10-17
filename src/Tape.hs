module Tape
    ( Cell
    , Tape
    , empty
    , getCell
    , modify
    , set
    , increment
    , decrement
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

type Cell = Int

newtype Tape = Tape { unTape :: Map Int Cell }
    deriving (Show)

empty :: Tape
empty = Tape Map.empty

getCell :: Int -> Tape -> Cell
getCell ix (Tape tape) = Map.findWithDefault 0 ix tape

modify :: (Cell -> Cell) -> Int -> Tape -> Tape
modify f ix (Tape tape) =
    Tape $ Map.alter f' ix tape
    where
        f' m = Just $ case m of
            Just x -> f x
            Nothing -> f 0

set :: Cell -> Int -> Tape -> Tape
set x = modify (const x)

increment :: Int -> Tape -> Tape
increment = Tape.modify (+ 1)

decrement :: Int -> Tape -> Tape
decrement = Tape.modify (subtract 1)
