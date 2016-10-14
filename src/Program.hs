module Program
    ( Program
    , Command(..)
    , needsIO
    ) where

import Data.Array

type Program = Array Int Command

data Command
    = GoLeft
    | GoRight
    | Increment
    | Decrement
    | PutChar
    | GetChar
    | BeginLoop
    | EndLoop

needsIO :: Command -> Bool
needsIO PutChar = True
needsIO GetChar = True
needsIO _ = False
