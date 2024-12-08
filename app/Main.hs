module Main (main) where

import Syntax.Grammar.Abs
import Eval
import Args
import Gen

import Test.QuickCheck
import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad ( unless )
import System.Exit
import Syntax.Grammar.Print (printTree)

defaultCanvasSize :: (Int, Int)
defaultCanvasSize = (3, 3)

showTrip :: (Double, Double, Double) -> String
showTrip (r,b,g) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

-- A 2d list where each element is a tuple of its coordinates
canvas :: (Int, Int) -> [[(Int, Int)]]
canvas (size_x, size_y) = map (zip [0..size_x] . replicate size_x) [0..size_y]

defaultCanvas :: [[(Int, Int)]]
defaultCanvas = canvas defaultCanvasSize

main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    unless (null errs) ( do
        putStrLn "The were errors parsing the arguments:"
        mapM_ putStr errs >> exitFailure )

    opts <- foldl (>>=) (return defaultOpts) optArgs
    let Options 
            { optVerbose = verb
            , optInput = input 
            , optSeedHash = seed
            } = opts

    -- TODO: add option to pass saved program

    prog2 <- generate (arbitrary :: Gen Trip)
    putStrLn "Generated expression:"
    putStrLn $ printTree prog2

    putStrLn $ "Seeding with first 8 bytes of: " ++ seed
    let rgb = (map.map) (uncurry (evalTrip prog2 seed)) defaultCanvas
    putStrLn "Evaluated result:"
    let x = (map.map) showTrip rgb
    (mapM_.mapM_) print x
    putStrLn "done."
