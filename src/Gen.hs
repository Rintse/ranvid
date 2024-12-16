{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}

module Gen (genTrip) where

import Syntax.Grammar.Abs

import Debug.Trace
import Generic.Random
import Test.QuickCheck
import GHC.Generics ( Generic )
import Control.Monad
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen )
import Test.QuickCheck.Gen ( Gen(MkGen) )

maxDepth :: Int
maxDepth = 300

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
        ( 100 -- Eq
        % 100 -- Lt
        % 100 -- Gt
        % 100 -- Neq
        % 100 -- Leq
        % 100 -- Geq
        % 100 -- Not
        % 100 -- And
        % 100 -- Or
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
        % 075 -- Ite
        % () )
        `withBaseCase` leafGen

instance Arbitrary Trip where
    arbitrary = do 
        let expGen = resize maxDepth (arbitrary :: Gen Exp)
        liftM3 Triple expGen expGen expGen

-- |Generate a random triple with rng seeded to `seed`
genTrip :: String -> Trip
genTrip seed = runGen (arbitrary :: Gen Trip) where 
        runGen (MkGen g) = g rng maxDepth
        rng = mkQCGen $ intFromHash seed
        intFromHash s = fromIntegral $ runGet getInt64host (pack s)
