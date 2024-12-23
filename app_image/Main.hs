{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where

-- This module creates random images from the language generator:
-- 1. Generate a 2d-array where each element is a tuple of the x and y 
--    coordinates of this array element (normalized to be between -1 and 1)
-- 2. Generate a random expression of type:
--    "Double -> Double -> ( Double , ( Double , Double ) )"
--    Where we will interpret the first two "Doubles" as X and Y values and the
--    output triple of "Doubles" as RGB values of an image
-- 3. For each element in our 2d-array, we map the coordinates to an expression
--    that is the generated expression applied to the X and Y parts of the 
--    coordinates.
-- 4. We evaluate all these expressions to obtain some value of type: 
--    "( Double , ( Double , Double ) )"
-- 5. We extract the three doubles from the value and pass them to an image
--    processing library which constructs a picture out of them

import Syntax.Grammar.Print ( printTree )
import Syntax.Parse ( parseExp, parseType )
import Image ( generateRGBs, checkRGBs, rgbsToImg )
import Preprocess ( fillRands, expDepth, expSize )
import Args ( getOpts, Options(..) )
import Gen ( genExp )

import Graphics.Image (writeImage, displayImageUsing, defaultViewer)
import Text.Printf (printf)

typeSpec :: String
typeSpec = "Double -> Double -> ( Double , ( Double , Double ) )"

main :: IO ()
main = do
    Options { optVerbose = verb
            , optSize = canvasSize
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
    -- let simplified = simplifyTrip triple
    -- printf "Simplified to [depth=%s, size=%s]:" 
    --     (showTripDepths simplified)
    --     (showTripSizes simplified)
    -- putStrLn $ printTree simplified

    case generateRGBs simplified canvasSize parallel of
        Left err -> putStrLn $ "Could not generate RGBs: " ++ err
        Right rgbs -> do
            checkRGBs rgbs
            let action = case outFile of
                    Nothing -> displayImageUsing defaultViewer True
                    Just filename -> writeImage filename
            action $ rgbsToImg rgbs
