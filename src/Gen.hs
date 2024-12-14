{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}

module Gen (arbitrary) where

import Syntax.Grammar.Abs

import Debug.Trace
import Generic.Random
import Test.QuickCheck
import GHC.Generics (Generic)
import Control.Monad

maxExpDepth :: Int
maxExpDepth = 3


deriving instance Generic Var
deriving instance Generic Exp
deriving instance Generic BExp

instance Arbitrary DVal where
    arbitrary = Val <$> choose (-1, 1)

instance Arbitrary Var where
    arbitrary = genericArbitrary uniform

-- Base case in generating expressions.
leafGen :: Gen Exp
leafGen = oneof 
    [ EVar <$> (arbitrary :: Gen Var)
    , EDVal <$> (arbitrary :: Gen DVal)
    , pure Rand
    ]

instance Arbitrary BExp where
    arbitrary = genericArbitraryRec 
        ( 0 -- Eq
        % 1 -- Lt
        % 1 -- Gt
        % 0 -- Neq
        % 1 -- Leq
        % 1 -- Geq
        % 1 -- Not
        % 1 -- And
        % 1 -- Or
        % () )

instance Arbitrary Exp where
    arbitrary = genericArbitraryRec 
        ( 100 -- EVar
        % 100 -- EDVal
        % 100 -- Rand
        % 100 -- Min
        % 100 -- Sqrt
        % 100 -- Sin
        % 100 -- Cos
        % 100 -- Mul
        % 100 -- Div
        % 100 -- Mod
        % 100 -- Add
        % 100 -- Sub
        % 050 -- Ite
        % () )
        `withBaseCase` leafGen

instance Arbitrary Trip where
    arbitrary = do 
        let expGen = resize maxExpDepth (arbitrary :: Gen Exp)
        liftM3 Triple expGen expGen expGen
