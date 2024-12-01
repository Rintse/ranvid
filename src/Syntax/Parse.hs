module Syntax.Parse where

import Syntax.Grammar.Par
import Syntax.Grammar.Lex
import Syntax.Grammar.Abs
import Syntax.Grammar.ErrM
import System.Exit

-- Parses contents of given input file
parse :: String -> IO Exp
parse s = do
    putStr "Parsing program"
    let ts = myLexer s
    case pExp ts of
        Bad r -> do
            putStrLn $ "Parse failed: " ++ r
            putStr $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Ok r -> do
            putStr "Parse successful"
            return r
