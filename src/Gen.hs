{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}

module Gen (genExp) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )

import Generic.Random
import Test.QuickCheck
import GHC.Generics ( Generic )
import Control.Monad
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen )
import Test.QuickCheck.Gen ( Gen(MkGen) )

maxRec :: Int
maxRec = 1

minRec :: Int
minRec = 0

deriving instance Generic Var
deriving instance Generic Exp

instance Arbitrary DVal where
    arbitrary = Val <$> choose (-1, 1)

instance Arbitrary Var where
    arbitrary = genericArbitrary uniform

genOfType :: Type -> Gen Exp
genOfType TDouble = sized selectOnSize where
        selectOnSize size
            | size >= 0 && size < minRec = frequency $ nonLeafsG size
            | size >= minRec && size < maxRec = frequency $ allNodesG size
            | otherwise = frequency leafsG
        leafsG =
            [ (100, EVar <$> (arbitrary :: Gen Var))
            , (050, EDVal <$> (arbitrary :: Gen DVal))
            , (050, pure Rand)
            ]
        nonLeafsG size =
            [ (050, resize (size + 1) $ Min <$> genD)
            , (050, resize (size + 1) $ Sqrt <$> genD)
            , (050, resize (size + 1) $ Sin <$> genD)
            , (050, resize (size + 1) $ Cos <$> genD)
            , (050, resize (size + 1) $ EPow <$> genD)
            , (050, resize (size + 1) $ Mul <$> genD <*> genD)
            , (050, resize (size + 1) $ Div <$> genD <*> genD)
            , (050, resize (size + 1) $ Mod <$> genD <*> genD)
            , (050, resize (size + 1) $ Add <$> genD <*> genD)
            , (050, resize (size + 1) $ Sub <$> genD <*> genD)
            , (050, resize (size + 1) $ Ite <$> genB <*> genD <*> genD)
            -- TODO: fst/snd only make sense when function application exists
            ]
        allNodesG size = leafsG ++ nonLeafsG size
        genD = genOfType TDouble
        genB = genOfType TBool

genOfType (TProd a b) = Tup <$> genOfType a <*> genOfType b

genOfType _ = pure Rand

-- |Generate a random expression of type `t` with rng seeded to `seed`
genExp :: Type -> String -> IO Exp
genExp t seed = do
    putStrLn "Generating a random expression of type:"
    putStrLn $ printTree t

    return $ runGen (genOfType t) where
        runGen (MkGen g) = g rng 0
        rng = mkQCGen $ intFromHash seed
        intFromHash s = fromIntegral $ runGet getInt64host (pack s)
