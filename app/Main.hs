module Main (main) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )
import Syntax.Parse (parse)
import Eval
import Args ( getOpts, Options(..) )
import Gen

import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import System.Random ( mkStdGen, uniformR )
import Test.QuickCheck
import Control.Monad.State (evalState)

defaultCanvasSize :: (Int, Int)
defaultCanvasSize = (3, 3)

showTrip :: (Double, Double, Double) -> String
showTrip (r,b,g) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

-- A 2d list where each element is a tuple of its coordinates
canvas :: (Int, Int) -> [[(Int, Int)]]
canvas (size_x, size_y) = map (zip [0..size_x-1] . replicate size_x) [0..size_y-1]

defaultCanvas :: [[(Int, Int)]]
defaultCanvas = canvas defaultCanvasSize

-- Lazily evaluated list of random draws
randomList :: String -> [Double]
randomList seed = do
    -- Get first 4 bytes and interpret as an int to be able to seed mkStdGen
    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    rec (mkStdGen $ intFromHash seed)
    where rec g = let (r, g2) = uniformR (-1, 1) g in r : rec g2

main :: IO ()
main = do
    Options { optVerbose = verb
            , optInputFile = inputFile
            , optSeedHash = seed
            } <- getOpts

    prog <- case inputFile of
            Just s -> parse s
            Nothing -> generate (arbitrary :: Gen Trip)

    putStrLn "Using the expression:"
    putStrLn $ printTree prog
    putStrLn $ "Seeded with first 8 bytes of: " ++ seed

    let randomDraws = randomList seed
    let stateMCanvas = (map.map) (uncurry (evalTrip prog)) defaultCanvas
    let rgbs = evalState (mapM sequence stateMCanvas) randomDraws

    putStrLn "Evaluated result:"
    mapM_ (mapM_ (print . showTrip)) rgbs
    putStrLn "done."
