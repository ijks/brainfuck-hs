{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State

import Program
import Tape (Tape)
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

goLeft :: Interpreter -> Interpreter
goLeft interp @ Interp { pointer } = interp { pointer = pointer - 1 }

goRight :: Interpreter -> Interpreter
goRight interp @ Interp { pointer } = interp { pointer = pointer + 1 }

increment :: Int -> Interpreter -> Interpreter
increment index interp @ Interp { tape } =
    interp { tape = modify index (+ 1) }

decrement :: Int -> Interpreter -> Interpreter
decrement index interp @ Interp { tape } =
    interp { tape = modify index (subtract 1) }

currentCommand :: Interpreter -> Command
currentCommand Interp { program, pc } = program ! pc

execute :: MonadIO m => Command -> Interpreter -> m Interpreter
execute command interp =
    if needsIO command then
        do
            case command of
                PutChar -> liftIO $ putChar undefined
                GetChar -> liftIO $ void getChar
            return interp
    else
        return $ case command of
            GoLeft -> goLeft interp
            GoRight -> goRigh interp
            Increment -> undefined
            Decrement -> undefined
            BeginLoop -> undefined
            EndLoop -> undefined

step :: (MonadIO m) => Interpreter -> m Interpreter
step interp = execute (currentCommand interp) interp

evaluate :: (MonadIO m) => Interpreter -> m Interpreter
evaluate = step >=> evaluate

main :: IO ()
main = do
  putStrLn "hello world"
