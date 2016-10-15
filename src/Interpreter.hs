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
import Program (Command(..), Program)
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

create :: Program -> Interpreter
create program =
    Interp
        { program = program
        , pc = 0
        , tape = Tape.empty
        , pointer = 0
        }

step :: MonadChar m => Interpreter -> m Interpreter
step interp @ Interp {..} = do
    let
        command = Program.getCommand pc program
        cell = Tape.getCell pointer tape
    interp' <- case command of
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
                    if cell == 0 then
                        Program.loopEnd pc program + 1
                    else
                        pc
                }
        EndLoop ->
            return interp
                { pc =
                    if cell /= 0 then
                        Program.loopBegin pc program + 1
                    else
                        pc
                }
    return interp' { pc = pc + 1}

run :: MonadChar m => Interpreter -> m Interpreter
run = step >=> run
