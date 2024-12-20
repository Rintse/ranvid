{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Syntax.Grammar.Abs ( Type(..) )
import Syntax.Grammar.Print ( printTree )
import Syntax.Parse ( parse )
import Eval ( generateRGBs, checkRGBs )
import Preprocess ( fillRands, expDepth, expSize )
import Args ( getOpts, Options(..) )
import Image ( rgbsToImg )
import Gen ( genExp )

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
    tripleWithRands <- case inFile of
            Just s -> parse s
            Nothing -> return $ genExp TDouble seed

    let triple = fillRands tripleWithRands seed
    printf "Using the expression [depths=%s, sizes=%s]:" 
        (show $ expDepth triple)
        (show $ expSize triple)
    putStrLn $ printTree triple

    let simplified = triple
    -- let simplified = simplifyTrip triple
    -- printf "Simplified to [depths=%s, sizes=%s]:" 
    --     (showTripDepths simplified)
    --     (showTripSizes simplified)
    -- putStrLn $ printTree simplified

    let rgbs = generateRGBs simplified canvasSize parallel
    checkRGBs rgbs
    let action = case outFile of 
            Nothing -> displayImageUsing defaultViewer True 
            Just filename -> writeImage filename
    action $ rgbsToImg rgbs
