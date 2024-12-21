{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

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
import System.Exit (exitFailure)
import Data.List (intercalate)
import Control.Monad.Except ( MonadError(throwError), Except, runExcept )
import Data.Either ( partitionEithers )
import Data.Bifunctor (Bifunctor(first))

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

valToRGB :: Either String Value -> Either String RGBTup
valToRGB (Right (VPair (VVal r) (VPair (VVal g) (VVal b)))) =
    Right $ scalePixel (r, g, b)
valToRGB (Right (VPair (VPair (VVal r) (VVal g)) (VVal b))) =
    Right $ scalePixel (r, g, b)
valToRGB (Right _) = Left "Evaluation did not give a 3-tuple of doubles"
valToRGB (Left e) = Left e

-- |Generate a canvas for expression `e` with size `size`
-- Runs in `p` parallel threads simultaneously
-- Expects all `Rand` nodes to already have been substituted for `EDVal`s
generateRGBs :: Exp -> (Int, Int) -> Int -> Either String [[RGBTup]]
generateRGBs e size p = do
    let calcRow = map ( valToRGB . evalExp ) where
            toEval = evalMonad (evalExpM e)
            evalExp c = runExcept (runReaderT toEval c)
    let calcRows = map calcRow (canvas size)

    let results = if p > 1
        then calcRows `using` parListChunk p rdeepseq
        else calcRows

    let (errors, rgbs) = unzip (map partitionEithers results)
    case concat errors of
        [] -> return rgbs
        (err:_) -> throwError $ "Error generating RGBS: " ++ err

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

-- A monad for pogram evaluation, containing:
--   - A reader with the value for X and Y in this evaluation
--   - An error monad to express evaluation failure
newtype EvalMonad a = EvalMonad {
    evalMonad :: ReaderT (Double, Double) (Except String) a
} deriving  ( Functor, Applicative, Monad
            , MonadReader (Double, Double), MonadError String )

-- |Evaluates 2 arguments and pairs them to allow for easy pattern matching
eval2 :: Exp -> Exp -> EvalMonad (Value, Value)
eval2 a b = liftA2 (,) (evalExpM a) (evalExpM b)

-- |Evaluate binary arithmetic operators
evalAExp2 :: Exp -> (Double -> Double -> Double) -> Exp -> EvalMonad Value
evalAExp2 e1 op e2 = eval2 e1 e2 >>= go where
    go (VVal v1, VVal v2) = return $ VVal $ op v1 v2
    go other = throwError $ 
        "Non-double arguments to arithmetic operator:\n" ++ show other

-- |Evaluate unary arithmetic operators
evalAExp :: (Double -> Double) -> Exp -> EvalMonad Value
evalAExp op e = evalExpM e >>= go where 
    go (VVal v) = return $ VVal $ op v
    go other = throwError $ 
        "Non-double argument to arithmetic operator:\n" ++ show other

-- |Evaluate comparison operators
evalComp :: Exp -> (Bool -> Bool -> Bool) -> Exp -> EvalMonad Value
evalComp e1 op e2 = eval2 e1 e2 >>= go where
    go (VBVal v1, VBVal v2) = return $ VBVal $ op v1 v2
    go other = throwError $ 
        "Non-bool arguments to boolean operator:\n" ++ show other

-- |Evaluate binary boolean operators
evalBExp2 :: Exp -> (Bool -> Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp2 e1 op e2 = eval2 e1 e2 >>= go where
    go (VBVal v1, VBVal v2) = return $ VBVal $ op v1 v2
    go other = throwError $ 
        "Non-bool arguments to boolean operator:\n" ++ show other

-- |Evaluate unary boolean operators
evalBExp :: (Bool -> Bool) -> Exp -> EvalMonad Value
evalBExp op e = evalExpM e >>= go where 
    go (VBVal b) = return $ VBVal $ op b
    go other = throwError $ 
        "Non-bool argument to boolean operator:\n" ++ show other

evalExpM :: Exp -> EvalMonad Value
evalExpM e = case e of
    (EVar XVar) -> asks (VVal . fst)
    (EVar YVar) -> asks (VVal . snd)
    (EDVal (Val d)) -> return $ VVal d
    (Min a) -> evalAExp negate a
    (Sqrt a) -> evalAExp sqrtPos a
    (Sin a) -> evalAExp sin a
    (Cos a) -> evalAExp cos a
    (EPow a) -> evalAExp expUnit a
    (Mul a b) -> evalAExp2 a (*) b
    (Div a b) -> evalAExp2 a divUnit b
    (Mod a b) -> evalAExp2 a modUnit b
    (Add a b) -> evalAExp2 a addUnit b
    (Sub a b) -> evalAExp2 a subUnit b
    (Eq a b) -> evalComp a (==) b
    (Lt a b) -> evalComp a (<) b
    (Gt a b) -> evalComp a (>) b
    (Neq a b) -> evalComp a (/=) b
    (Leq a b) -> evalComp a (<=) b
    (Geq a b) -> evalComp a (>=) b
    (Not a) -> evalBExp not a
    (And a b) -> evalBExp2 a (&&) b
    (Or a b) -> evalBExp2 a (||) b
    (Ite c a b) -> evalExpM c >>= doIf where
        doIf (VBVal rb) = if rb then evalExpM a else evalExpM b
        doIf other = throwError $ "Non-bool in if condition: " ++ show other
    (Tup a b) -> VPair <$> evalExpM a <*> evalExpM b
    other -> throwError $ "WARNING: Not implemented: " ++ show other
