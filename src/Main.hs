{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State
import Data.Array
-- import qualified Data.Map

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

type Program = Array Int Command

data Tape = Tape

data Interpreter = Interp
    { program :: Program
    , pc :: Int
    , tape :: Tape
    , pointer :: Int
    }

create :: Program -> Interpreter
create program =
    Interp
        { program = program
        , pc = 0
        , tape = undefined
        , pointer = 0
        }

modifyPC :: (Int -> Int) -> Interpreter -> Interpreter
modifyPC f interp @ Interp { pc } =
    interp { pc = f pc }

setPC :: Int -> Interpreter -> Interpreter
setPC = modifyPC . const

modifyPointer :: (Int -> Int) -> Interpreter -> Interpreter
modifyPointer f interp @ Interp { pointer } =
    interp { pointer = f pointer }

currentCommand :: Interpreter -> Command
currentCommand Interp { program, pc } = program ! pc

execute :: MonadIO m => Command -> Interpreter -> m Interpreter
execute command interp =
    if needsIO command then
        return $ case command of
            GoLeft -> modifyPointer (subtract 1) interp
            GoRight -> modifyPointer (+ 1) interp
            Increment -> undefined
            Decrement -> undefined
            BeginLoop -> undefined
            EndLoop -> undefined
    else do
        case command of
            PutChar -> liftIO $ putChar undefined
            GetChar -> liftIO $ void getChar
        return interp

step :: (MonadIO m) => Interpreter -> m Interpreter
step interp = execute (currentCommand interp) interp

evaluate :: (MonadIO m) => Interpreter -> m Interpreter
evaluate = step >=> evaluate

main :: IO ()
main = do
  putStrLn "hello world"
