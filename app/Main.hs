module Main (main) where

import Syntax.Grammar.Print ( printTree )
import Syntax.Parse ( parse )
import Eval ( generateRGBs, fillRands, putRGBs )
import Args ( getOpts, Options(..) )
import Image ( rgbsToImg )
import Control.Concurrent
import Gen ( genTrip )

import Graphics.Image (displayImage, writeImage)

main :: IO ()
main = do
    Options { optVerbose = verb
            , optSize = canvasSize
            , optParallel = parallel
            , optInputFile = inputFile
            , optSeedHash = seed
            } <- getOpts

    putStrLn $ "Seeded with first 8 bytes of: " ++ seed
    tripleRaw <- case inputFile of
            Just s -> parse s
            Nothing -> return $ genTrip seed

    let triple = fillRands tripleRaw seed
    putStrLn "Using the expression:"
    putStrLn $ printTree triple

    let rgbs = generateRGBs triple canvasSize parallel
    writeImage "out/test.png" $ rgbsToImg rgbs

    putStrLn "done."
