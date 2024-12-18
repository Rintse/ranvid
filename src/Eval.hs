{-# LANGUAGE TypeFamilies #-}

module Eval (
    RGBTup, generateRGBs, checkRGBs,
    expUnit, sqrtPos, modUnit, addUnit, subUnit, divUnit,
) where

import Syntax.Grammar.Abs

import Control.Applicative ( liftA3, liftA2 )
import Control.Monad.Reader
import Data.Fixed ( mod' )
import Control.Parallel.Strategies
import Debug.Trace (trace)
import System.Exit (exitFailure)
import Data.List (intercalate)

type RGBTup = (Double, Double, Double)

showRGBTup :: RGBTup -> String
showRGBTup (r,g,b) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

-- |A 2d list where each element is a tuple of its coordinates
-- All the coordinates are normalized to lie within the [-1, 1] range
canvas :: (Int, Int) -> [[(Double, Double)]]
canvas (w, h) = do
    let scaleCoord maxC c = (fromIntegral c / fromIntegral maxC) * 2 - 1
    let widths = map (scaleCoord w) [0..w-1]
    let heights = map (scaleCoord h) [0..h-1]
    map (zip widths . replicate w) heights

-- |Generate a canvas for Triple `trip` with size `size`
-- Runs in `p` parallel threads simultaneously
-- Expects all `Rand` nodes to already have been substituted for `EDVal`s
generateRGBs :: Trip -> (Int, Int) -> Int -> [[RGBTup]]
generateRGBs trip size p = do
    let calcRow = map (evalTrip trip)
    let calcRows = map calcRow (canvas size)
    if p > 1
        then calcRows `using` parListChunk p rdeepseq
        else calcRows

checkRGBs :: [[RGBTup]] -> IO ()
checkRGBs rgbs = do
    let invalids = filter isInvalidPixel (concat rgbs) where
            isInvalidPixel (r, g, b) = invalid r || invalid g || invalid b
            invalid c = c < 0 || c > 1

    unless (null invalids) $ do 
        putStrLn $ "ERROR: Invalid RGB values: \n  - "
            ++ intercalate "\n  - " (map showRGBTup invalids)
        exitFailure

-- |Evaluate the triple `trip` at coordinates `coords`
evalTrip :: Trip -> (Double, Double) -> RGBTup
evalTrip trip coords = do
    let result = runReader (evalTripM trip) coords
    -- The arithmetic operations are defined on [-1, 1]
    -- We need to convert to the required [0, 1] interval for rgb values
    scalePixel result where
        scaleC c = (c + 1) / 2
        scalePixel (r, g, b) = (scaleC r, scaleC g, scaleC b)

evalTripM :: Trip -> Reader (Double, Double) (Double, Double, Double)
evalTripM (Triple a b c) = liftA3 (,,) (evalExpM a) (evalExpM b) (evalExpM c)

-- Custom operators that go ([-1, 1], [-1, 1]) -> [-1, 1] or that filter
-- out some undefined inputs and just return 0 in that case
expUnit :: Double -> Double
expUnit x = exp x / exp 1
sqrtPos :: Double -> Double
sqrtPos a = if a < 0 then sqrt (-a) else sqrt a
modUnit :: Double -> Double -> Double
modUnit a b = if b == 0 then 0 else a `mod'` b
addUnit :: Double -> Double -> Double
addUnit a b = (a + b) / 2
subUnit :: Double -> Double -> Double
subUnit a b = (a - b) / 2
divUnit :: Double -> Double -> Double
divUnit a b
    | a == 0 && b == 0 = 0
    | abs a < abs b = a / b
    | otherwise = b / a

evalExpM :: Exp -> Reader (Double, Double) Double
evalExpM (EVar XVar) = asks fst
evalExpM (EVar YVar) = asks snd
-- TODO: make this an error?
evalExpM Rand = trace "WARNING: Encountered 'Rand' in evaluation" return 0
evalExpM (EDVal (Val d)) = return d
evalExpM (Min e) = negate <$> evalExpM e
evalExpM (Sqrt e) = fmap sqrtPos (evalExpM e)
evalExpM (Sin e) = sin <$> evalExpM e
evalExpM (Cos e) = cos <$> evalExpM e
evalExpM (EPow e) = expUnit <$> evalExpM e
evalExpM (Mul e1 e2) = liftA2 (*) (evalExpM e1) (evalExpM e2)
evalExpM (Div e1 e2) = liftA2 divUnit (evalExpM e1) (evalExpM e2)
evalExpM (Mod e1 e2) = liftA2 modUnit (evalExpM e1) (evalExpM e2)
evalExpM (Add e1 e2) = liftA2 addUnit (evalExpM e1) (evalExpM e2)
evalExpM (Sub e1 e2) = liftA2 subUnit (evalExpM e1) (evalExpM e2)
evalExpM (Ite c e1 e2) = evalBExp c >>= evalIf
    where evalIf cond = if cond then evalExpM e1 else evalExpM e2

evalBExp :: BExp -> Reader (Double, Double) Bool
evalBExp (Eq e1 e2) = liftA2 (==) (evalExpM e1) (evalExpM e2)
evalBExp (Lt e1 e2) = liftA2 (<) (evalExpM e1) (evalExpM e2)
evalBExp (Gt e1 e2) = liftA2 (>) (evalExpM e1) (evalExpM e2)
evalBExp (Neq e1 e2) = liftA2 (/=) (evalExpM e1) (evalExpM e2)
evalBExp (Leq e1 e2) = liftA2 (<=) (evalExpM e1) (evalExpM e2)
evalBExp (Geq e1 e2) = liftA2 (>=) (evalExpM e1) (evalExpM e2)
evalBExp (Not e) = not <$> evalBExp e
evalBExp (And e1 e2) = liftA2 (&&) (evalBExp e1) (evalBExp e2)
evalBExp (Or e1 e2) = liftA2 (||) (evalBExp e1) (evalBExp e2)
