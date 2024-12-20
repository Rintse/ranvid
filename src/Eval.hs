module Eval (
    RGBTup, generateRGBs, checkRGBs,
    expUnit, sqrtPos, modUnit, addUnit, subUnit, divUnit,
) where

import Syntax.Grammar.Abs
import Value ( Value(..) )

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

-- The arithmetic operations are defined on [-1, 1]
-- We need to convert to the required [0, 1] interval for rgb values
scalePixel :: (Double, Double, Double) -> (Double, Double, Double)
scalePixel = f3 (\c -> (c + 1) / 2) where f3 f (a, b, c) = (f a, f b, f c)

valToRGB :: Value -> RGBTup
valToRGB (VPair (VDVal r) (VPair (VDVal g) (VDVal b))) = scalePixel (r, g, b)
valToRGB (VPair (VPair (VDVal r) (VDVal g)) (VDVal b)) = scalePixel (r, g, b)
valToRGB _ = (0.0, 0.0, 0.0)

-- |Generate a canvas for expression `e` with size `size`
-- Runs in `p` parallel threads simultaneously
-- Expects all `Rand` nodes to already have been substituted for `EDVal`s
generateRGBs :: Exp -> (Int, Int) -> Int -> [[RGBTup]]
generateRGBs e size p = do
    let calcRow = map ( valToRGB . evalExp e ) where
            evalExp e' = runReader (evalExpM e')
    let calcRows = map calcRow (canvas size)
    if p > 1
        then calcRows `using` parListChunk p rdeepseq
        else calcRows

checkRGBs :: [[RGBTup]] -> IO ()
checkRGBs rgbs = do
    let invalids = concatMap (filter isInvalidPixel) rgbs where
            isInvalidPixel (r, g, b) = invalid r || invalid g || invalid b
            invalid c = c < 0 || c > 1

    unless (null invalids) $ do
        putStrLn $ "ERROR: Invalid RGB values: \n  - "
            ++ intercalate "\n  - " (map showRGBTup invalids)
        exitFailure

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

evalExpM :: Exp -> Reader (Double, Double) Value
evalExpM e = case e of
    -- (EVar XVar) -> asks fst
    -- (EVar YVar) -> asks snd
    -- (EDVal (Val d)) -> return d
    -- (Min a) -> negate <$> evalExpM a
    -- (Sqrt a) -> fmap sqrtPos (evalExpM a)
    -- (Sin a) -> sin <$> evalExpM a
    -- (Cos a) -> cos <$> evalExpM a
    -- (EPow a) -> expUnit <$> evalExpM a
    -- (Mul a b) -> liftA2 (*) (evalExpM a) (evalExpM b)
    -- (Div a b) -> liftA2 divUnit (evalExpM a) (evalExpM b)
    -- (Mod a b) -> liftA2 modUnit (evalExpM a) (evalExpM b)
    -- (Add a b) -> liftA2 addUnit (evalExpM a) (evalExpM b)
    -- (Sub a b) -> liftA2 subUnit (evalExpM a) (evalExpM b)
    -- (Eq a b) -> liftA2 (==) (evalExpM a) (evalExpM b)
    -- (Lt a b) -> liftA2 (<) (evalExpM a) (evalExpM b)
    -- (Gt a b) -> liftA2 (>) (evalExpM a) (evalExpM b)
    -- (Neq a b) -> liftA2 (/=) (evalExpM a) (evalExpM b)
    -- (Leq a b) -> liftA2 (<=) (evalExpM a) (evalExpM b)
    -- (Geq a b) -> liftA2 (>=) (evalExpM a) (evalExpM b)
    -- (Not a) -> not <$> evalBExp a
    -- (And a b) -> liftA2 (&&) (evalBExp a) (evalBExp b)
    -- (Or a b) -> liftA2 (||) (evalBExp a) (evalBExp b)
    -- (Ite c a b) -> evalBExp c >>= evalIf
    --     where evalIf cond = if cond then evalExpM a else evalExpM b
    -- -- TODO: make this an error?
    Rand -> trace "WARNING: Encountered 'Rand' in evaluation" return (VDVal 0.0)
    _ -> trace "WARNING: Not implemented" return (VDVal 0.0)
