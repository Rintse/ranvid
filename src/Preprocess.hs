module Preprocess (
    fillRands, expDepth, simplifyTrip,
    showTripSizes, showTripDepths,
) where

import Syntax.Grammar.Abs
import Syntax.AbsF
import Eval ( expUnit, sqrtPos, modUnit, addUnit, subUnit, divUnit )

import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Control.Applicative ( liftA2, liftA3 )
import Control.Monad.Random
import Data.Functor.Foldable.Monadic ( anaM )
import Data.Functor.Foldable( project, Recursive(cata) )
import Data.Functor ( (<&>) )

-- |The depth of the expression (height of the tree)
expDepth :: Exp -> Int
expDepth = cata go where
    go :: ExpF Int -> Int
    go other = 1 + foldr max 0 other

showTripDepths :: Trip -> String
showTripDepths (Triple a b c) = "(" 
    ++ show (expDepth a) ++ ", " 
    ++ show (expDepth b) ++ ", " 
    ++ show (expDepth c) ++ ")"

-- |The size of the expression (number of nodes)
expSize :: Exp -> Int
expSize = cata go where
    go :: ExpF Int -> Int
    go other = 1 + sum other

showTripSizes :: Trip -> String
showTripSizes (Triple a b c) = "(" 
    ++ show (expSize a) ++ ", " 
    ++ show (expSize b) ++ ", " 
    ++ show (expSize c) ++ ")"

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

applyToDouble :: (Exp -> Exp) -> (Double -> Double) -> Exp -> Exp
applyToDouble _ f (EDVal (Val a)) = EDVal (Val (f a))
applyToDouble default' _ a = default' a

applyDouble2
    :: (Exp -> Exp -> Exp)          -- Default if args were not doubles
    -> (Double -> Double -> Double) -- Function to apply if 2 doubles
    -> Exp -> Exp                   -- Two expressions to evaluate
    -> Exp                          -- Potentially simplified expression
applyDouble2 _ f (EDVal (Val a)) (EDVal (Val b)) = EDVal (Val (f a b))
applyDouble2 default' _ a b = default' a b

simplifyExp :: Exp -> Exp
simplifyExp (Min a) = applyToDouble Min negate (simplifyExp a)
simplifyExp (Sqrt a) = applyToDouble Sqrt sqrtPos (simplifyExp a)
simplifyExp (Sin a) = applyToDouble Sin sin (simplifyExp a)
simplifyExp (Cos a) = applyToDouble Cos cos (simplifyExp a)
simplifyExp (EPow a) = applyToDouble EPow expUnit (simplifyExp a)
simplifyExp (Mul a b) = applyDouble2 Mul (*) (simplifyExp a) (simplifyExp b)
simplifyExp (Div a b) = applyDouble2 Div divUnit (simplifyExp a) (simplifyExp b)
simplifyExp (Mod a b) = applyDouble2 Mod modUnit (simplifyExp a) (simplifyExp b)
simplifyExp (Add a b) = applyDouble2 Add addUnit (simplifyExp a) (simplifyExp b)
simplifyExp (Sub a b) = applyDouble2 Sub subUnit (simplifyExp a) (simplifyExp b)
simplifyExp (Ite a b c) = case simplifyBExp a of
    Left bexp -> Ite bexp (simplifyExp b) (simplifyExp c)
    Right bool -> if bool then simplifyExp b else simplifyExp c
simplifyExp other = other

applyToBool2
    :: (Double -> Double -> Bool)   -- Function to apply if 2 doubles
    -> Exp -> Exp                   -- Two expressions to evaluate
    -> (Exp -> Exp -> BExp)         -- Defaut if args were not doubles
    -> Either BExp Bool             -- Simplified bexp
applyToBool2 f (EDVal (Val a)) (EDVal (Val b)) _ = convertApply f a b
    where convertApply f' a' b' = Right $ f' a' b'
applyToBool2 _ a b default' = Left $ default' a b

simplifyBExp :: BExp -> Either BExp Bool
simplifyBExp (Eq a b) = applyToBool2 (==) (simplifyExp a) (simplifyExp b) Eq
simplifyBExp (Lt a b) = applyToBool2 (<) (simplifyExp a) (simplifyExp b) Lt
simplifyBExp (Gt a b) = applyToBool2 (>) (simplifyExp a) (simplifyExp b) Gt
simplifyBExp (Neq a b) = applyToBool2 (/=) (simplifyExp a) (simplifyExp b) Neq
simplifyBExp (Leq a b) = applyToBool2 (<=) (simplifyExp a) (simplifyExp b) Leq
simplifyBExp (Geq a b) = applyToBool2 (>=) (simplifyExp a) (simplifyExp b) Geq
simplifyBExp (And a b) = case (simplifyBExp a, simplifyBExp b) of
    -- No simplificaiton possible
    (Left ra, Left rb) -> Left $ And ra rb
    -- And identity law
    (Left _, Right False) -> Right False
    (Right False, Left _) -> Right False
    (Left ra, Right True) -> Left ra
    (Right True, Left rb) -> Left rb
    -- Simplify actual bool values
    (Right b1, Right b2) -> Right $ b1 && b2
simplifyBExp (Or a b) = case (simplifyBExp a, simplifyBExp b) of
    -- No simplificaiton possible
    (Left ra, Left rb) -> Left $ Or ra rb
    -- Or identity law
    (Left _, Right True) -> Right True
    (Right True, Left _) -> Right True
    (Left ra, Right False) -> Left ra
    (Right False, Left rb) -> Left rb
    -- Simplify actual bool values
    (Right b1, Right b2) -> Right $ b1 && b2
simplifyBExp (Not a) = case simplifyBExp a of
    (Left r) -> Left $ Not r
    (Right b) -> Right $ not b

simplifyTrip :: Trip -> Trip
simplifyTrip (Triple a b c) = Triple (simplifyExp a) (simplifyExp b) (simplifyExp c)
