module Main ( main ) where

import Distribution.Simple
import Distribution.Simple.Program
import System.Process ( system )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { hookedPrograms = [checkBNFC]
    , preConf = \args configFlags -> do
        _ <- system "bnfc -p Syntax -o src -d src/grammar.bnf"
        -- remove the generated test file
        _ <- system "rm -f src/Syntax/Grammar/Test.hs"
        _ <- system "rm -f src/Syntax/Grammar/*.bak"
        preConf simpleUserHooks args configFlags
    }

checkBNFC :: Program
checkBNFC = (simpleProgram "bnfc") 
    { programFindVersion = findProgramVersion "--version" id }
