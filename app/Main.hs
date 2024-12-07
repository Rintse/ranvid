module Main (main) where

import Syntax.Grammar.Abs
import Eval
import Args
import Gen

import Test.QuickCheck
import System.Environment ( getArgs )
import System.Console.GetOpt
import Control.Monad ( when, unless )
import System.Exit
import Syntax.Grammar.Print (printTree)

showTrip :: (Double, Double, Double) -> String
showTrip (r,b,g) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

defaultCanvasSize :: (Int, Int)
defaultCanvasSize = (3, 3)

-- A 2d list where each element is a tuple of its coordinates
canvas :: (Int, Int) -> [[(Int, Int)]]
canvas (size_x, size_y) = map (zip [0..size_x] . replicate size_x) [0..size_y]

main :: IO ()
main = do
    args <- getArgs -- Get and parse options
    let (optArgs, nonOpts, errs) = getOpt RequireOrder Args.options args

    unless (null errs) ( do
        putStrLn "The were errors parsing the arguments:"
        mapM_ putStr errs >> exitFailure )

    opts <- foldl (>>=) (return defaultOpts) optArgs
    let Options { optVerbose = verb, optInput = input } = opts

    -- Parse input into a program AST
    -- prog <- input >>= parse
    -- putStrLn "Parsed program"
    -- print prog

    prog2 <- generate (arbitrary :: Gen Trip)
    putStrLn "Generated expression:"
    putStrLn $ printTree prog2

    let rgb = (map.map) (\(x,y) -> evalTrip prog2 x y) (canvas defaultCanvasSize)
    putStrLn "Evaluated result:"
    let x = (map.map) showTrip rgb
    putStrLn "done."
