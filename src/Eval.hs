module Eval (evalTrip) where

import Control.Monad.State.Lazy
import Syntax.Grammar.Abs
import Control.Applicative (liftA3)
import Data.InfList (InfList(..))
import Debug.Trace (trace)

type EvalState = InfList Double

-- evaluate triple of expressions at (x,y), with rng initialized with the seed
evalTrip :: Trip -> Int -> Int -> State EvalState (Double, Double, Double)
evalTrip (Triple a b c) x y = liftA3 (,,) 
    (evalExp a x y) (evalExp b x y) (evalExp c x y)

-- general expressions
evalExp :: Exp -> Int -> Int -> State EvalState Double
evalExp (EVar XVar) x _ = return $ fromIntegral x
evalExp (EVar YVar) _ y = return $ fromIntegral y
evalExp (EDVal (Val d)) _ _ = return d
evalExp Rand _ _ = get >>= (\(r ::: t) -> put t >> return r)
evalExp (Min e) x y = negate <$> evalExp e x y
evalExp (Sqrt e) x y = sqrt <$> evalExp e x y
evalExp (Sin e) x y = sin <$> evalExp e x y
evalExp (Cos e) x y = cos <$> evalExp e x y
evalExp (Mul e1 e2) x y = liftA2 (*) (evalExp e1 x y) (evalExp e2 x y)
evalExp (Div e1 e2) x y = liftA2 divUnit (evalExp e1 x y) (evalExp e2 x y)
    where divUnit a b = if a < b then a / b else b / a
evalExp (Add e1 e2) x y = liftA2 (\a b -> a + b / 2) (evalExp e1 x y) (evalExp e2 x y)
evalExp (Ite c e1 e2) x y = do
    cond <- evalBExp c x y
    if cond then evalExp e1 x y else evalExp e2 x y

-- Boolean expressions
evalBExp :: BExp -> Int -> Int -> State EvalState Bool
evalBExp (Eq e1 e2) x y = liftA2 (==) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Lt e1 e2) x y =  liftA2 (<) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Gt e1 e2) x y =  liftA2 (>) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Neq e1 e2) x y = liftA2 (/=) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Leq e1 e2) x y = liftA2 (<=) (evalExp e1 x y) (evalExp e2 x y)
evalBExp (Geq e1 e2) x y = liftA2 (>=) (evalExp e1 x y) (evalExp e2 x y)

evalBExp (Not e) x y = not <$> evalBExp e x y
evalBExp (And e1 e2) x y = liftA2 (&&) (evalBExp e1 x y) (evalBExp e2 x y)
evalBExp (Or e1 e2) x y = liftA2 (||) (evalBExp e1 x y) (evalBExp e2 x y)
