{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies, LambdaCase #-}

module Eval ( RGBTup, putRGBs, generateRGBs, fillRands, countDepth ) where

import Syntax.Grammar.Abs
import Syntax.AbsF
import Control.Applicative ( liftA3, liftA2 )
import Control.Monad.Reader
import Control.Monad.Random
import Data.Functor.Foldable.Monadic ( anaM, )
import Data.Functor.Foldable( project, Recursive (cata) )
import Data.Binary.Get ( runGet, getInt64host )
import Data.Functor ( (<&>) )
import Data.ByteString.Lazy.Char8 ( pack )
import Data.Fixed ( mod' )
import Control.Parallel.Strategies
import Debug.Trace (trace)

type RGBTup = (Double, Double, Double)

showRGBTup :: RGBTup -> String
showRGBTup (r,g,b) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

putRGBs :: [[RGBTup]] -> IO ()
putRGBs rgbs = do
    let putRow = mapM_ (putStrLn . showRGBTup)
    mapM_ (\x -> putRow x >> putStrLn "") rgbs

-- |A 2d list where each element is a tuple of its coordinates
-- All the coordinates are normalized to lie within the [-1, 1] range
canvas :: (Int, Int) -> [[(Double, Double)]]
canvas (w, h) = do
    let scaleCoord maxC c = (fromIntegral c / fromIntegral maxC) * 2 - 1
    let widths = map (scaleCoord w) [0..w-1]
    let heights = map (scaleCoord h) [0..h-1]
    map (zip widths . replicate w) heights

-- |Replace all the `Rand` occurences with a random double value 
-- (seeded with `seed`)
fillRands :: Trip -> String -> Trip
fillRands (Triple a b c) seed = do
    let g1 = mkStdGen $ intFromHash seed
            where intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let (ra, g2) = runRand (fillRandsM a) g1
    let (rb, g3) = runRand (fillRandsM b) g2
    let (rc, __) = runRand (fillRandsM c) g3
    -- TODO: find a nicer way to chain these?
    Triple ra rb rc

countDepth :: Exp -> Int
countDepth = cata go where
    go :: ExpF Int -> Int
    go other = 1 + foldr max 0 other

-- Fill in `rand`s and recurse into `BExp`s (see `fillRandsBM`)
fillRandsM :: Exp -> Rand StdGen Exp
fillRandsM = anaM go where
    go Rand = getRandomR (-1, 1) <&> EDValF . Val
    go (Ite b e1 e2) = liftA3 IteF (fillRandsBM b) (return e1) (return e2)
    go other = return (project other)

-- The boolean expressions just need to recurse into the expressions again
fillRandsBM :: BExp -> Rand StdGen BExp
fillRandsBM = anaM go where
    go (Eq e1 e2) = liftA2 EqF (fillRandsM e1) (fillRandsM e2)
    go (Lt e1 e2) = liftA2 LtF (fillRandsM e1) (fillRandsM e2)
    go (Gt e1 e2) = liftA2 GtF (fillRandsM e1) (fillRandsM e2)
    go (Neq e1 e2) = liftA2 NeqF (fillRandsM e1) (fillRandsM e2)
    go (Leq e1 e2) = liftA2 LeqF (fillRandsM e1) (fillRandsM e2)
    go (Geq e1 e2) = liftA2 GeqF (fillRandsM e1) (fillRandsM e2)
    go other = return $ project other

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

-- |A monad for pogram evaluation, containing:
-- - A reader with the x and y values for this evaluation
-- - IO for convenience during debugging
newtype EvalMonad a = EvalMonad {
    evalMonad :: Reader (Double, Double) a
} deriving (Functor, Applicative, Monad, MonadReader (Double, Double))

evalTrip :: Trip -> (Double, Double) -> RGBTup
evalTrip trip coords = do
    let toEval = evalMonad (evalTripM trip)
    let result = runReader toEval coords
    -- The arithmetic operations are defined on [-1, 1]
    -- We need to convert to the required [0, 1] interval for rgb values
    let scaled = scalePixel result where 
            scaleC c = (c + 1) / 2
            scalePixel (r, g, b) = (scaleC r, scaleC g, scaleC b)
    -- After this point, all the values should be in the [0, 1] interval
    let isInvalid = isInvalidPixel scaled where
            isInvalidPixel (r, g, b) = invalid r || invalid g || invalid b
            invalid c = c < 0 || c > 1
    if isInvalid
        then trace ("WARNING: Invalid RGB: " ++ showRGBTup scaled) scaled
        else scaled

evalTripM :: Trip -> EvalMonad (Double, Double, Double)
evalTripM (Triple a b c) = liftA3 (,,) (evalExpM a) (evalExpM b) (evalExpM c)

evalExpM :: Exp -> EvalMonad Double
evalExpM (EVar XVar) = asks fst
evalExpM (EVar YVar) = asks snd
-- TODO: make this an error?
evalExpM Rand = trace "WARNING: Encountered 'Rand' in evaluation" return 0

evalExpM (EDVal (Val d)) = return d
evalExpM (Min e) = negate <$> evalExpM e
evalExpM (Sqrt e) = fmap sqrtPos (evalExpM e)
    where sqrtPos a = if a < 0 then sqrt (-a) else sqrt a
evalExpM (Sin e) = sin <$> evalExpM e
evalExpM (Cos e) = cos <$> evalExpM e
evalExpM (EPow e) = expUnit <$> evalExpM e where expUnit x = (exp x / exp 1)
evalExpM (Mul e1 e2) = liftA2 (*) (evalExpM e1) (evalExpM e2)
evalExpM (Div e1 e2) = liftA2 divUnit (evalExpM e1) (evalExpM e2)
    where divUnit a b
            | a == 0 && b == 0 = 0
            | abs a < abs b = a / b
            | otherwise = b / a
evalExpM (Mod e1 e2) = liftA2 modUnit (evalExpM e1) (evalExpM e2)
    where modUnit a b = if b == 0 then 0 else a `mod'` b
evalExpM (Add e1 e2) = liftA2 addUnit (evalExpM e1) (evalExpM e2)
    where addUnit a b = (a + b) / 2
evalExpM (Sub e1 e2) = liftA2 subUnit (evalExpM e1) (evalExpM e2)
    where subUnit a b = (a - b) / 2
evalExpM (Ite c e1 e2) = evalBExp c >>= evalIf
    where evalIf cond = if cond then evalExpM e1 else evalExpM e2

evalBExp :: BExp -> EvalMonad Bool
evalBExp (Eq e1 e2) = liftA2 (==) (evalExpM e1) (evalExpM e2)
evalBExp (Lt e1 e2) = liftA2 (<) (evalExpM e1) (evalExpM e2)
evalBExp (Gt e1 e2) = liftA2 (>) (evalExpM e1) (evalExpM e2)
evalBExp (Neq e1 e2) = liftA2 (/=) (evalExpM e1) (evalExpM e2)
evalBExp (Leq e1 e2) = liftA2 (<=) (evalExpM e1) (evalExpM e2)
evalBExp (Geq e1 e2) = liftA2 (>=) (evalExpM e1) (evalExpM e2)
evalBExp (Not e) = not <$> evalBExp e
evalBExp (And e1 e2) = liftA2 (&&) (evalBExp e1) (evalBExp e2)
evalBExp (Or e1 e2) = liftA2 (||) (evalBExp e1) (evalBExp e2)
