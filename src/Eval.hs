{-# LANGUAGE FlexibleContexts #-}

module Eval (
    expUnit, sqrtPos, modUnit, addUnit, subUnit, divUnit, evalExpM
) where

import Syntax.AbsF
import Syntax.Grammar.Abs
import Value ( Value(..), valToExp )

import Control.Applicative ( liftA2 )
import Data.Fixed ( mod' )
import Control.Monad.Except ( MonadError(throwError), Except )
import Data.Functor.Foldable (Recursive(project), Corecursive (ana))

type EvalMonad a = Except String a

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
evalComp :: Exp -> (Double -> Double -> Bool) -> Exp -> EvalMonad Value
evalComp e1 op e2 = eval2 e1 e2 >>= go where
    go (VVal v1, VVal v2) = return $ VBVal $ op v1 v2
    go other = throwError $ 
        "Non-double arguments to comparison operator:\n" ++ show other

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

-- |Substitue, in `e`, `x` for `s`
substitute :: Ident -> Exp -> Exp -> Exp
substitute (Ident x) s = ana go where
    go :: Exp -> ExpF Exp
    go v@(Var (Ident x')) = if x' == x
        then project s
        else project v
    go other = project other

evalExpM :: Exp -> EvalMonad Value
evalExpM e = case e of
    (DVal d) -> return $ VVal d
    -- arithmetic expressions
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

    -- Comparison operators
    (Eq a b) -> evalComp a (==) b
    (Lt a b) -> evalComp a (<) b
    (Gt a b) -> evalComp a (>) b
    (Neq a b) -> evalComp a (/=) b
    (Leq a b) -> evalComp a (<=) b
    (Geq a b) -> evalComp a (>=) b

    -- Boolean operators
    (Not a) -> evalBExp not a
    (And a b) -> evalBExp2 a (&&) b
    (Or a b) -> evalBExp2 a (||) b

    -- Functions
    (Abstr a b) -> return $ VFun a b
    (App a b) -> evalExpM a >>= doApp where
        doApp (VFun i body) = evalExpM $ substitute i b body
        doApp other = throwError $ "Invalid application: " ++ show other

    -- Products
    (Tup a b) -> VPair <$> evalExpM a <*> evalExpM b
    (Fst a) -> evalExpM a >>= doFst where
        doFst (VPair x _) = return x
        doFst other = throwError $ "Non-pair in fst: " ++ show other
    (Snd a) -> evalExpM a >>= doSnd where
        doSnd (VPair _ x) = return x
        doSnd other = throwError $ "Non-pair in snd: " ++ show other

    -- Branches
    (Ite c a b) -> evalExpM c >>= doIf where
        doIf (VBVal rb) = if rb then evalExpM a else evalExpM b
        doIf other = throwError $ "Non-bool in if condition: " ++ show other
    (Match m x1 e1 x2 e2) -> evalExpM m >>= doMatch where
        doMatch (VL l) = evalExpM $ substitute x1 e1 $ valToExp l
        doMatch (VR r) = evalExpM $ substitute x2 e2 $ valToExp r
        doMatch other = throwError $ "Non coproduct in match: " ++ show other

    other -> throwError $ "Not implemented: " ++ show other
