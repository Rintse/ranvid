module Main (main) where

import Lib
import Syntax.Parse
import Args

import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad ( when, unless )
import System.Exit

main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    unless (null errs) ( do
        putStrLn "The were errors parsing the arguments:"
        mapM_ putStr errs >> exitFailure )

    opts <- foldl (>>=) (return defaultOpts) optArgs
    let Options { optVerbose = verb, optInput = input } = opts
    
    -- Parse input into a program AST
    prog <- input >>= parse
    putStrLn "Parsed program"

