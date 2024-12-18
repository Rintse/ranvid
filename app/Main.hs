{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Main (main) where

import Syntax.Grammar.Print ( printTree )
import Syntax.Parse ( parse )
import Eval ( generateRGBs, checkRGBs )
import Preprocess ( fillRands, simplifyTrip, showTripDepths, showTripSizes )
import Args ( getOpts, Options(..) )
import Image ( rgbsToImg )
import Gen ( genTrip )

import Graphics.Image (writeImage, displayImageUsing, defaultViewer)
import Text.Printf (printf)

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
    tripleRaw <- case inFile of
            Just s -> parse s
            Nothing -> return $ genTrip seed

    let triple = fillRands tripleRaw seed

    printf "Using the expression [depths=%s, sizes=%s]:\n" 
        (showTripDepths triple)
        (showTripSizes triple)
    putStrLn $ printTree triple

    let simplified = simplifyTrip triple
    printf "Simplified to [depths=%s, sizes=%s]:\n" 
        (showTripDepths simplified)
        (showTripSizes simplified)
    putStrLn $ printTree simplified

    let rgbs = generateRGBs simplified canvasSize parallel
    checkRGBs rgbs
    let action = case outFile of 
            Nothing -> displayImageUsing defaultViewer True 
            Just filename -> writeImage filename
    action $ rgbsToImg rgbs
