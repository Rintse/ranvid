module Syntax.Parse (parse) where

import Syntax.Grammar.Par
import Syntax.Grammar.Abs
import System.Exit

-- Parses contents of given input file
parse :: String -> IO Trip
parse s = do
    putStr "Parsing program"
    let ts = myLexer s
    case pTrip ts of
        Left r -> do
            putStrLn $ "Parse failed: " ++ r
            putStr $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Right r -> do
            putStr "Parse successful"
            return r
