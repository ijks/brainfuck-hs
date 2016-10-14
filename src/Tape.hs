module Tape
    ( Tape
    , empty
    , modify
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

newtype Tape = Tape { unTape :: Map Int Int }

empty :: Tape
empty = Tape Map.empty

modify :: (Int -> Int) -> Int -> Tape -> Tape
modify f index (Tape tape) =
    Tape $ Map.alter f' index tape
    where
        f' m = Just $ case m of
            Just x -> f x
            Nothing -> 0
