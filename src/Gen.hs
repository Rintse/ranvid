{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}

module Gen (genTrip) where

import Syntax.Grammar.Abs

import Generic.Random
import Test.QuickCheck
import GHC.Generics ( Generic )
import Control.Monad
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen )
import Test.QuickCheck.Gen ( Gen(MkGen) )

maxRec :: Int
maxRec = 11

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
            [ (050, resize (size + 1) $ Eq  <$> expG <*> expG)
            , (050, resize (size + 1) $ Lt  <$> expG <*> expG)
            , (050, resize (size + 1) $ Gt  <$> expG <*> expG)
            , (050, resize (size + 1) $ Neq <$> expG <*> expG)
            , (050, resize (size + 1) $ Leq <$> expG <*> expG)
            , (050, resize (size + 1) $ Geq <$> expG <*> expG)
            ]
        nonStalksG size = 
            [ (100, resize (size + 1) $ Not <$> bexpG)
            , (100, resize (size + 1) $ And <$> bexpG <*> bexpG)
            , (100, resize (size + 1) $ Or  <$> bexpG <*> bexpG)
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
            , (050, EDVal <$> (arbitrary :: Gen DVal))
            , (050, pure Rand)
            ]
        nonLeafsG size = 
            [ (050, resize (size + 1) $ Min <$> expG)
            , (050, resize (size + 1) $ Sqrt <$> expG)
            , (050, resize (size + 1) $ Sin <$> expG)
            , (050, resize (size + 1) $ Cos <$> expG)
            , (050, resize (size + 1) $ EPow <$> expG)
            , (050, resize (size + 1) $ Mul <$> expG <*> expG)
            , (050, resize (size + 1) $ Div <$> expG <*> expG)
            , (050, resize (size + 1) $ Mod <$> expG <*> expG)
            , (050, resize (size + 1) $ Add <$> expG <*> expG)
            , (050, resize (size + 1) $ Sub <$> expG <*> expG)
            , (050, resize (size + 1) $ Ite <$> bxpG <*> expG <*> expG)
            ]
        allNodesG size = leafsG ++ nonLeafsG size
        expG = arbitrary :: Gen Exp
        bxpG = arbitrary :: Gen BExp

instance Arbitrary Trip where
    arbitrary = liftM3 Triple g g g where g = arbitrary :: Gen Exp

-- |Generate a random triple with rng seeded to `seed`
genTrip :: String -> Trip
genTrip seed = runGen (arbitrary :: Gen Trip) where
        runGen (MkGen g) = g rng 0
        rng = mkQCGen $ intFromHash seed
        intFromHash s = fromIntegral $ runGet getInt64host (pack s)
