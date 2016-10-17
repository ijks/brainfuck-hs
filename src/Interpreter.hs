{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- Inspiration from:
-- https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md
-- https://gist.github.com/adzeitor/4119051
-- http://rickardlindberg.me/writing/reflections-on-programming/2012-06-17-a-beautiful-brainfuck-implementation/

module Interpreter
    ( Interpreter
    , create
    , getOutput
    , run
    ) where

import Data.Char (chr, ord)
import Data.List (uncons)

import Program
import Tape

data Interpreter =
    Interp
        { tape :: Tape Int
        , input :: [Char]
        , output :: [Char]
        }
    deriving (Show)

getOutput :: Interpreter -> [Char]
getOutput = output

create :: String -> Interpreter
create input =
    Interp
        { tape = Tape.empty
        , input = input
        , output = []
        }

run :: Program -> Interpreter -> Interpreter
run [] interp = interp
run program @ (command:rest) interp @ Interp { .. } =
    case command of
        GoLeft ->
            continue interp { tape = goLeft tape }
        GoRight ->
            continue interp { tape = goRight tape }
        Increment ->
            continue interp { tape = increment tape }
        Decrement ->
            continue interp { tape = decrement tape }
        PutChar ->
            continue interp { output = output ++ [chr $ currentCell tape] }
        GetChar ->
            case input of
                [] ->
                    interp
                (c:cs) ->
                    continue interp { tape = set (ord c) tape, input = cs }
        Loop body ->
            if currentCell tape > 0 then
                run program $ run body interp
            else
                continue interp
    where
        continue = run rest
