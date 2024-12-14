{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval ( runEval, evalTrip, RGBTup ) where

import Control.Monad.State.Lazy
import Syntax.Grammar.Abs
import Control.Applicative ( liftA3, liftA2 )
import Data.InfList (InfList(..))
import Control.Monad.Reader
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Data.List ( intercalate )
import System.Random ( mkStdGen, uniformR )
import Data.Fixed (mod')
import Debug.Trace (trace)

type RGBTup = (Double, Double, Double)

showRGBTup :: RGBTup -> String
showRGBTup (r,g,b) = "(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

-- |A monad for pogram evaluation, containing:
-- - A reader with the image size, used to scale x and y values
-- - A State with a list of random draws
-- - An error monad to express evaluation failure
newtype EvalMonad a = EvalMonad {
    evalMonad :: ReaderT (Int, Int) (StateT (InfList Double) IO) a
} deriving (    Functor, Applicative, Monad,
                MonadReader (Int, Int),
                MonadState (InfList Double))

putRgbs :: [[RGBTup]] -> IO ()
putRgbs rgbs = do
    let putRow = mapM_ (putStrLn . showRGBTup)
    mapM_ (\x -> putRow x >> putStrLn "") rgbs

-- |Unpack the various levels of the `EvalMonad` and execute respective runners
-- Also scales the [-1, 1] values to be in the [0, 1] interval so that they
-- may be used directly as RGB values in image backends
runEval :: EvalMonad [[RGBTup]] -> (Int, Int) -> String -> IO [[RGBTup]]
runEval evalM size seed = do
    let toEval = evalMonad evalM
    let stateM = runReaderT toEval size
    result <- evalStateT stateM $ randomList seed

    -- The arithmetic operations are defined on [-1, 1]
    -- We need to convert to the required [0, 1] interval for rgb values
    let scale x = (x + 1) / 2
    let scaled = (map.map) (\(r, g, b) -> (scale r, scale g, scale b)) result

    -- After this point, all the values should be in the [0, 1] interval
    let invalids = concatMap getInvalids scaled where 
            getInvalids l = [ p | p <- l, isInvalidPixel p ]
            isInvalidPixel (r, g, b) = invalid r || invalid g || invalid b
            invalid a = a < 0 || a > 1

    -- unless (null invalids) $
    --     putStrLn $ "WARNING: Found invalid RGB values: \n  - "
    --     ++ intercalate "\n  - " (map show invalids)

    return scaled


-- |Lazily evaluated list of random draws
randomList :: String -> InfList Double
randomList seed = do
    -- Get first 4 bytes and interpret as an int to be able to seed mkStdGen
    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    rec (mkStdGen $ intFromHash seed)
    where rec g = let (r, g2) = uniformR (0, 1) g in r ::: rec g2

-- |evaluate triple of expressions at (x,y), with rng initialized with the seed
evalTrip :: Trip -> Int -> Int -> EvalMonad (Double, Double, Double)
evalTrip (Triple a b c) x y = liftA3 (,,)
    (evalExp a x y) (evalExp b x y) (evalExp c x y)

-- |Rescale a coordinate `c` to be in the range [-1, 1] such that:
-- - (`c`= 0 ) maps to -1
-- - (`c`=`max`) maps to 1
scaleCoord :: Int -> Int -> Double
scaleCoord c maxC = ((fromIntegral c / fromIntegral maxC) * 2) - 1

evalExp :: Exp -> Int -> Int -> EvalMonad Double
evalExp (EVar XVar) x _ = asks (\(w, _) -> scaleCoord x (w-1))
evalExp (EVar YVar) _ y = asks (\(_, h) -> scaleCoord y (h-1))
evalExp (EDVal (Val d)) _ _ = return d
evalExp Rand _ _ = get >>= (\(r ::: t) -> put t >> return r)
evalExp (Min e) x y = negate <$> evalExp e x y
evalExp (Sqrt e) x y = fmap (\a -> if a < 0 then sqrt (-a) else sqrt a) (evalExp e x y)
evalExp (Sin e) x y = sin <$> evalExp e x y
evalExp (Cos e) x y = cos <$> evalExp e x y
evalExp (Mul e1 e2) x y = liftA2 (*) (evalExp e1 x y) (evalExp e2 x y)
evalExp (Div e1 e2) x y = liftA2 divUnit (evalExp e1 x y) (evalExp e2 x y)
    where divUnit a b = if a < b then a / b else b / a
evalExp (Mod e1 e2) x y = liftA2 mod' (evalExp e1 x y) (evalExp e2 x y)
evalExp (Add e1 e2) x y = liftA2 (\a b -> (a + b) / 2) (evalExp e1 x y) (evalExp e2 x y)
evalExp (Sub e1 e2) x y = liftA2 (\a b -> (a - b) / 2) (evalExp e1 x y) (evalExp e2 x y)
evalExp (Ite c e1 e2) x y = evalBExp c x y >>= evalIf
    where evalIf cond = if cond then evalExp e1 x y else evalExp e2 x y

evalBExp :: BExp -> Int -> Int -> EvalMonad Bool
evalBExp (Eq e1 e2) x y = liftA2 (==) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Lt e1 e2) x y =  liftA2 (<) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Gt e1 e2) x y =  liftA2 (>) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Neq e1 e2) x y = liftA2 (/=) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Leq e1 e2) x y = liftA2 (<=) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Geq e1 e2) x y = liftA2 (>=) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Not e) x y = not <$> evalBExp e x y
evalBExp (And e1 e2) x y = liftA2 (&&) (evalBExp e1 x y) (evalBExp e2 x y)
evalBExp (Or e1 e2) x y = liftA2 (||) (evalBExp e1 x y) (evalBExp e2 x y)
