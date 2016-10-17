module Main where

import Data.Char (ord)

import Interpreter (Interpreter, create, getOutput, run)
import Program

main :: IO ()
main = do
    putStrLn "hello world"

runBrainfuck :: String -> IO ()
runBrainfuck program =
    case parse program of
        Left err -> putStrLn err
        Right program -> runIO program

runIO :: Program -> IO ()
runIO program = do
    input <- getContents
    let output = getOutput $ runWithInput input program
    putStr output

runDebug :: Program -> IO Interpreter
runDebug program = do
    let interp = runWithInput "" program
    mapM_ (print . ord) . getOutput $ interp
    return interp

runWithInput :: String -> Program -> Interpreter
runWithInput input program =
    run program $ create input
