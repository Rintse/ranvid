module Preprocess ( fillRands, expDepth, expSize) where

import Syntax.Grammar.Abs
import Syntax.AbsF
import Eval ( expUnit, sqrtPos, modUnit, addUnit, subUnit, divUnit )

import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Control.Applicative ( liftA2, liftA3 )
import Control.Monad.Random
import Data.Functor.Foldable.Monadic ( anaM )
import Data.Functor.Foldable( project, Recursive(cata))
import Data.Functor ( (<&>) )

-- |The depth of the expression (height of the tree)
expDepth :: Exp -> Int
expDepth = cata go where
    go :: ExpF Int -> Int
    go other = 1 + foldr max 0 other

-- |The size of the expression (number of nodes)
expSize :: Exp -> Int
expSize = cata go where
    go :: ExpF Int -> Int
    go other = 1 + sum other

-- |Replace all the `Rand` occurences with a random double (seeded with `seed`)
fillRands :: Exp -> String -> Exp
fillRands e seed = do
    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let g = mkStdGen $ intFromHash seed
    evalRand (fillRandsM e) g

fillRandsM :: Exp -> Rand StdGen Exp
fillRandsM = anaM go where
    go Rand = getRandomR (-1, 1) <&> DValF
    go other = return (project other)

-- applyDouble2
--     :: (Exp -> Exp -> Exp)          -- Default if args were not doubles
--     -> (Double -> Double -> Double) -- Function to apply if 2 doubles
--     -> Exp -> Exp                   -- Two expressions to simplify
--     -> Exp                          -- Potentially simplified expression
-- applyDouble2 _ f (EDVal (Val a)) (EDVal (Val b)) = EDVal (Val (f a b))
-- applyDouble2 default' _ a b = default' a b
--
-- applyToDouble :: (Exp -> Exp) -> (Double -> Double) -> Exp -> Exp
-- applyToDouble _ f (EDVal (Val a)) = EDVal (Val (f a))
-- applyToDouble default' _ a = default' a
--
-- -- |Simplify an expression by evaluating all subtrees that evaluate to a number
-- -- in advance, saving time in evaluation which is done millions of times
-- simplifyExp :: Exp -> Exp
-- simplifyExp e = case e of
--     (Min a) -> applyToDouble Min negate (simplifyExp a)
--     (Sqrt a) -> applyToDouble Sqrt sqrtPos (simplifyExp a)
--     (Sin a) -> applyToDouble Sin sin (simplifyExp a)
--     (Cos a) -> applyToDouble Cos cos (simplifyExp a)
--     (EPow a) -> applyToDouble EPow expUnit (simplifyExp a)
--     (Mul a b) -> applyDouble2 Mul (*) (simplifyExp a) (simplifyExp b)
--     (Div a b) -> applyDouble2 Div divUnit (simplifyExp a) (simplifyExp b)
--     (Mod a b) -> applyDouble2 Mod modUnit (simplifyExp a) (simplifyExp b)
--     (Add a b) -> applyDouble2 Add addUnit (simplifyExp a) (simplifyExp b)
--     (Sub a b) -> applyDouble2 Sub subUnit (simplifyExp a) (simplifyExp b)
--     (Ite a b c) -> case simplifyBExp a of
--         Left bexp -> Ite bexp (simplifyExp b) (simplifyExp c)
--         Right bool -> if bool then simplifyExp b else simplifyExp c
--     other -> other
--
-- applyToBool2
--     :: (Exp -> Exp -> BExp)         -- Default if args were not doubles
--     -> (Double -> Double -> Bool)   -- Function to apply if 2 doubles
--     -> Exp -> Exp                   -- Two expressions to evaluate
--     -> Either BExp Bool             -- Simplified bexp
-- applyToBool2 _ f (EDVal (Val a)) (EDVal (Val b)) = Right $ f a b
-- applyToBool2 default' _ a b = Left $ default' a b
--
-- simplifyBExp :: BExp -> Either BExp Bool
-- simplifyBExp e = case e of
--     (Eq a b) -> applyToBool2 Eq (==) (simplifyExp a) (simplifyExp b)
--     (Lt a b) -> applyToBool2 Lt (<) (simplifyExp a) (simplifyExp b)
--     (Gt a b) -> applyToBool2 Gt (>) (simplifyExp a) (simplifyExp b)
--     (Neq a b) -> applyToBool2 Neq (/=) (simplifyExp a) (simplifyExp b)
--     (Leq a b) -> applyToBool2 Leq (<=) (simplifyExp a) (simplifyExp b)
--     (Geq a b) -> applyToBool2 Geq (>=) (simplifyExp a) (simplifyExp b)
--     (And a b) -> case (simplifyBExp a, simplifyBExp b) of
--         (Left ra, Left rb) -> Left $ And ra rb
--         (Right b1, Right b2) -> Right $ b1 && b2
--         -- And identity law
--         (Left _, Right False) -> Right False
--         (Right False, Left _) -> Right False
--         (Left ra, Right True) -> Left ra
--         (Right True, Left rb) -> Left rb
--     (Or a b) -> case (simplifyBExp a, simplifyBExp b) of
--         (Left ra, Left rb) -> Left $ Or ra rb
--         (Right b1, Right b2) -> Right $ b1 && b2
--         -- Or identity law
--         (Left _, Right True) -> Right True
--         (Right True, Left _) -> Right True
--         (Left ra, Right False) -> Left ra
--         (Right False, Left rb) -> Left rb
--     (Not a) -> case simplifyBExp a of
--         (Left r) -> Left $ Not r
--         (Right b) -> Right $ not b
