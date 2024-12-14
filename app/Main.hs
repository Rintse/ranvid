module Main (main) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )
import Syntax.Parse (parse)
import Eval ( evalTrip, runEval, showRGBTup )
import Args ( getOpts, Options(..) )
import Image ( rgbsToImg )
import Control.Concurrent
import Gen

import Test.QuickCheck
import Graphics.Image (displayImage)

-- A 2d list where each element is a tuple of its coordinates
canvas :: (Int, Int) -> [[(Int, Int)]]
canvas (w, h) = map (zip [0..w-1] . replicate w) [0..h-1]

main :: IO ()
main = do
    Options { optVerbose = verb
            , optSize = canvasSize
            , optInputFile = inputFile
            , optSeedHash = seed
            } <- getOpts

    prog <- case inputFile of
            Just s -> parse s
            Nothing -> generate (arbitrary :: Gen Trip)

    putStrLn "Using the expression:"
    putStrLn $ printTree prog
    putStrLn $ "Seeded with first 8 bytes of: " ++ seed

    let stateMCanvas = (map.map) (uncurry (evalTrip prog)) (canvas canvasSize)
    let canvasM = mapM sequence stateMCanvas -- collapse into one monad
    rgbs <- runEval canvasM canvasSize seed -- run all with one random draws list

    putStrLn "Evaluated result:"
    (mapM_.mapM_) (putStrLn . showRGBTup) rgbs

    displayImage $ rgbsToImg rgbs
    threadDelay $ 10 * 1000000 
    putStrLn "done."
