{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where

-- This module creates random MIDIs from the language generator:
-- 1. Generate a 1d-array where each element is the coordinates of this 
-- array element (normalized to be between -1 and 1).
-- 2. Generate a random expression of type:
--    "Double -> ( Double , Double )"
--    Where we will interpret the first "Double" as the time and the
--    output tuple of "Doubles" as instrument and pitch values of an MIDI

import Syntax.Grammar.Print ( printTree )
import Syntax.Parse ( parseExp, parseType )
import Preprocess ( fillRands, expDepth, expSize )
import Args ( getOpts, Options(..) )
import Gen ( genExp )

import Text.Printf (printf)

typeSpec :: String
typeSpec = "Double -> ( ( Double , Double ) )"

main :: IO ()
main = do
    Options { optSize = canvasSize
            , optParallel = parallel
            , optInputFile = inFile
            , optOutputFile = outFile
            , optSeedHash = seed
            } <- getOpts

    putStrLn $ "Seeded with first 8 bytes of: " ++ seed
    tripleWithRands <- case inFile of
        Just s -> parseExp s
        Nothing -> do
            requiredType <- parseType typeSpec
            genExp requiredType seed

    let triple = fillRands tripleWithRands seed
    printf "Using the expression [depth=%s, size=%s]:\n"
        (show $ expDepth triple)
        (show $ expSize triple)
    putStrLn $ printTree triple

    let simplified = triple
    return ()
    -- let simplified = simplifyTrip triple
    -- printf "Simplified to [depth=%s, size=%s]:" 
    --     (showTripDepths simplified)
    --     (showTripSizes simplified)
    -- putStrLn $ printTree simplified

    case generateRGBs simplified canvasSize parallel of
        Left err -> putStrLn $ "Could not generate RGBs: " ++ err
        Right notes -> do
            return ()
