module Program
    ( Command(..)
    , needsIO
    , Program
    , empty
    , getCommand
    , parse
    , loopBegin
    , loopEnd
    ) where

import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as Vector
import Data.Maybe (mapMaybe)
import Control.Monad.State

data Command
    = GoLeft
    | GoRight
    | Increment
    | Decrement
    | PutChar
    | GetChar
    | BeginLoop
    | EndLoop
    deriving (Eq, Show)

needsIO :: Command -> Bool
needsIO PutChar = True
needsIO GetChar = True
needsIO _ = False

type Program = Vector Command

empty :: Program
empty = Vector.fromList []

getCommand :: Int -> Program -> Maybe Command
getCommand = flip (!?)

parse :: String -> Program
parse = Vector.fromList . mapMaybe parseCommand
    where
        parseCommand char =
            case char of
                '<' -> Just GoLeft
                '>' -> Just GoRight
                '+' -> Just Increment
                '-' -> Just Decrement
                '.' -> Just PutChar
                ',' -> Just GetChar
                '[' -> Just BeginLoop
                ']' -> Just EndLoop
                _   -> Nothing

loopBegin :: Int -> Program -> Int
loopBegin start program = evalState (search (start - 1)) 0
    where
        search ix = do
            let continue = search (ix - 1)
            depth <- get
            case program ! ix of
                BeginLoop
                    | depth == 0 -> return ix
                    | otherwise -> put (depth - 1) >> continue
                EndLoop -> put (depth + 1) >> continue
                _ -> continue

loopEnd :: Int -> Program -> Int
loopEnd start program = evalState (search (start + 1)) 0
    where
        search ix = do
            let continue = search (ix + 1)
            depth <- get
            case program ! ix of
                EndLoop
                    | depth == 0 -> return ix
                    | otherwise -> put (depth - 1) >> continue
                BeginLoop -> put (depth + 1) >> continue
                _ -> continue
