{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}

module Gen (arbitrary) where

import Syntax.Grammar.Abs

import Debug.Trace
import Generic.Random
import Test.QuickCheck
import GHC.Generics (Generic)
import Control.Monad

maxExpDepth :: Int
maxExpDepth = 20

-- TODO: make all of these not orphaned instances

deriving instance Generic BConst
deriving instance Generic Var
deriving instance Generic Exp
deriving instance Generic BExp

instance Arbitrary DVal where
    arbitrary = DVal <$> choose (-1, 1)

instance Arbitrary BConst where
    arbitrary = genericArbitrary uniform

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
    arbitrary = do
        let base = EBVal <$> oneof [ pure BTrue, pure BFalse ]
        genericArbitraryRec uniform `withBaseCase` base

instance Arbitrary Exp where
    arbitrary = genericArbitraryRec uniform `withBaseCase` leafGen

instance Arbitrary Trip where
    arbitrary = do 
        let expGen = resize maxExpDepth (arbitrary :: Gen Exp)
        liftM3 Triple expGen expGen expGen