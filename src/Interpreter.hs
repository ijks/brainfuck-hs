{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Interpreter
    ( Interpreter
    , create
    , step
    , run
    ) where

import Control.Monad
import Data.Char (chr, ord)

import MonadChar
import Program (Program, Command(..))
import qualified Program
import Tape (Tape, Cell)
import qualified Tape

data Interpreter =
    Interp
        { program :: Program
        , pc :: Int
        , tape :: Tape
        , pointer :: Int
        }
    deriving (Show)

create :: Program -> Interpreter
create program =
    Interp
        { program = program
        , pc = 0
        , tape = Tape.empty
        , pointer = 0
        }

execute :: MonadChar m => Command -> Interpreter -> m Interpreter
execute command interp @ Interp {..} =
    let cell = Tape.getCell pointer tape
    in case command of
        GoLeft ->
            return interp { pointer = pointer - 1 }
        GoRight ->
            return interp { pointer = pointer + 1 }
        Increment ->
            return interp { tape = Tape.increment pointer tape }
        Decrement ->
            return interp { tape = Tape.decrement pointer tape }
        PutChar -> do
                putC $ chr cell
                return interp
        GetChar -> do
                char <- getC
                return interp { tape = Tape.set (ord char) pointer tape }
        BeginLoop ->
            return interp
                { pc =
                    if cell <= 0 then
                        Program.loopEnd pc program + 1
                    else
                        pc
                }
        EndLoop ->
            return interp
                { pc =
                    if cell > 0 then
                        Program.loopBegin pc program + 1
                    else
                        pc
                }

step :: MonadChar m => Interpreter -> Maybe (m Interpreter)
step interp @ Interp {..} = do
    command <- Program.getCommand pc program
    return $ execute command interp { pc = pc + 1}

run :: MonadChar m => Interpreter -> m ()
run interp =
    case step interp of
        Just interp' -> interp' >>= run
        Nothing -> return ()
