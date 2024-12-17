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
import Control.Applicative (Applicative(liftA2), liftA3)

maxRec :: Int
maxRec = 12

minRec :: Int
minRec = 4

deriving instance Generic Var
deriving instance Generic Exp
deriving instance Generic BExp

instance Arbitrary DVal where
    arbitrary = Val <$> choose (-1, 1)

instance Arbitrary Var where
    arbitrary = genericArbitrary uniform

instance Arbitrary BExp where
    arbitrary = sized selectOnSize where
        selectOnSize size
            | size < maxRec = frequency $ stalksG size
            | otherwise = frequency $ allNodesG size
        -- When max depth is reached, recurse back into Exp with comparission 
        -- operators, which will generate leafs immediately (hence `stalks`)
        stalksG size = 
            [ (50, resize (size + 1) $ liftA2 Eq expG expG)
            , (50, resize (size + 1) $ liftA2 Lt expG expG)
            , (50, resize (size + 1) $ liftA2 Gt expG expG)
            , (50, resize (size + 1) $ liftA2 Neq expG expG)
            , (50, resize (size + 1) $ liftA2 Leq expG expG)
            , (50, resize (size + 1) $ liftA2 Geq expG expG)
            ]
        nonStalksG size = 
            [ (100, resize (size + 1) $ Not <$> bexpG)
            , (100, resize (size + 1) $ liftA2 And bexpG bexpG)
            , (100, resize (size + 1) $ liftA2 Or bexpG bexpG)
            ]
        allNodesG size = stalksG size ++ nonStalksG size
        expG = arbitrary :: Gen Exp
        bexpG = arbitrary :: Gen BExp

instance Arbitrary Exp where
    arbitrary = sized selectOnSize where
        selectOnSize size
            -- | size >= 0 && size < minRec = trace ("<min :: " ++ show size) (oneof $ nonLeafsG size)
            -- | size >= minRec && size < maxRec = trace (">< :: " ++ show size) (oneof $ allNodesG size)
            -- | otherwise = trace (">max :: " ++ show size) (oneof leafsG)
            | size >= 0 && size < minRec = frequency $ nonLeafsG size
            | size >= minRec && size < maxRec = frequency $ allNodesG size
            | otherwise = frequency leafsG
        leafsG =
            [ (100, EVar <$> (arbitrary :: Gen Var))
            , (100, EDVal <$> (arbitrary :: Gen DVal))
            , (100, pure Rand)
            ]
        nonLeafsG size = 
            [ (50, resize (size + 1) $ Min <$> expG)
            , (50, resize (size + 1) $ Sqrt <$> expG)
            , (50, resize (size + 1) $ Sin <$> expG)
            , (50, resize (size + 1) $ Cos <$> expG)
            , (50, resize (size + 1) $ EPow <$> expG)
            , (50, resize (size + 1) $ liftA2 Mul expG expG)
            , (50, resize (size + 1) $ liftA2 Div expG expG)
            , (50, resize (size + 1) $ liftA2 Mod expG expG)
            , (50, resize (size + 1) $ liftA2 Add expG expG)
            , (50, resize (size + 1) $ liftA2 Sub expG expG)
            , (50, resize (size + 1) $ liftA3 Ite bexpG expG expG)
            ]
        allNodesG size = leafsG ++ nonLeafsG size
        expG = arbitrary :: Gen Exp
        bexpG = arbitrary :: Gen BExp

instance Arbitrary Trip where
    arbitrary = liftM3 Triple g g g where g = arbitrary :: Gen Exp

-- |Generate a random triple with rng seeded to `seed`
genTrip :: String -> Trip
genTrip seed = runGen (arbitrary :: Gen Trip) where
        runGen (MkGen g) = g rng 0
        rng = mkQCGen $ intFromHash seed
        intFromHash s = fromIntegral $ runGet getInt64host (pack s)
