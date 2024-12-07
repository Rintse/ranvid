module Eval (evalTrip, evalExp) where

import Syntax.Grammar.Abs

evalTrip :: Trip -> Int -> Int -> (Double, Double, Double)
evalTrip (Triple a b c) x y = (evalExp a x y, evalExp b x y, evalExp c x y)

evalExp :: Exp -> Int -> Int -> Double
evalExp (EVar XVar) x _ = fromIntegral x
evalExp (EVar YVar) _ y = fromIntegral y
evalExp (EDVal (DVal d)) _ _ = d

evalBConst :: BConst -> Bool
evalBConst BTrue = True
evalBConst BFalse = False

evalBExp :: BExp -> Int -> Int -> Bool
evalBExp (EBVal b) _ _ = evalBConst b
-- Comparison operators
evalBExp (Eq e1 e2) x y =  evalExp e1 x y == evalExp e2 x y
evalBExp (Lt e1 e2) x y =  evalExp e1 x y == evalExp e2 x y
evalBExp (Gt e1 e2) x y =  evalExp e1 x y == evalExp e2 x y
evalBExp (Neq e1 e2) x y = evalExp e1 x y == evalExp e2 x y
evalBExp (Leq e1 e2) x y = evalExp e1 x y == evalExp e2 x y
evalBExp (Geq e1 e2) x y = evalExp e1 x y == evalExp e2 x y
-- Boolean operators
evalBExp (Not e) x y = not (evalBExp e x y)
evalBExp (And e1 e2) x y = evalBExp e1 x y && evalBExp e2 x y
evalBExp (Or e1 e2) x y = evalBExp e1 x y || evalBExp e2 x y
