{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen (genExp) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )

import Generic.Random
import Data.HashMap.Lazy ( HashMap, empty )
import Test.QuickCheck
import GHC.Generics ( Generic )
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen, QCGen )
import Control.Monad.Reader ( Reader, runReader, MonadTrans (lift), ReaderT (runReaderT, ReaderT), MonadReader (local) )
import Control.Monad.Identity (Identity)
import QuickCheck.GenT ( MonadGen ( liftGen ), runGenT, GenT, MonadGen )
import qualified QuickCheck.GenT as QCT ( choose )
import Data.Bifunctor (Bifunctor(second, first))
import Debug.Trace (trace)
import Test.QuickCheck.Gen (Gen(MkGen, unGen))

maxRec :: Int
maxRec = 2

minRec :: Int
minRec = 0

deriving instance Generic Ident
deriving instance Generic Exp

instance Arbitrary DVal where
    arbitrary = Val <$> choose (-1, 1)

instance Arbitrary Ident where
    arbitrary = do
        r <- choose (1, 9) :: Gen Int
        return $ Ident $ "x" ++ show r

newtype GenMonad a = EvalMonad {
    genMonad :: GenT (Reader (Int, (HashMap String Type)) ) a
} deriving  ( Functor, Applicative, Monad
            , MonadReader (Int, HashMap String Type)
            , MonadGen
            )

-- genOfType TDouble = do
--     let options = [ (1, pure Rand), (1, pure Rand) ]
--     let gens = frequency options
--     c <- lift (choose (1,2) :: Gen Double)
--     return $ EDVal (Val c)

freqM :: [(Int, GenMonad Exp)] -> GenMonad Exp
freqM gs = do
    QCT.choose (0, tot - 1) >>= (`select` gs) where
        tot = sum (map fst gs)
        select n ((k,x):xs)
            | n <= k    = x
            | otherwise = select (n-k) xs
        select _ _  = error "pick on empty list"

genOfType :: Type -> GenMonad Exp
genOfType TDouble = do
    let genD = local (\x -> x) genOfType TDouble
    let genB = genOfType TBool
    -- test2 <- ask
    -- test1 <- local (\x -> first (+1)) genB
    let nonLeafsG size = 
            [ (050, Min <$> genD )
            -- , (050, return $ Sqrt genD)
            -- , (050, return $ Sin  genD)
            -- , (050, return $ Cos  genD)
            -- , (050, return $ EPow genD)
            -- , (050, return $ Mul  genD genD)
            -- , (050, return $ Div  genD genD)
            -- , (050, return $ Mod  genD genD)
            -- , (050, return $ Add  genD genD)
            -- , (050, return $ Sub  genD genD)
            -- , (050, return $ Ite  genB genD genD)
            -- TODO: fst/snd only make sense when function application exists
            ]
    let leafsG =
            
           -- [ (050, EDVal <$> (arbitrary :: Gen DVal))
           [ (050, return Rand :: GenMonad Exp)
           ]
    let allNodesG size = nonLeafsG size ++ leafsG
    -- let selectOnSize size
    --         | size >= 0 && size < minRec = freqM $ nonLeafsG size
    --         | size >= minRec && size < maxRec = freqM $ allNodesG size
    --         | otherwise = freqM leafsG

    -- s <- lift getSize
    -- trace ("genOfType(TDouble) [size = " ++ show s ++ "]")
    --     selectOnSize s
    return Rand


genOfType TBool = do
    let genDm = genOfType TDouble
    let genBm = genOfType TBool
    genD <- genOfType TDouble
    genB <- genOfType TBool
    let test = [ (050, Not <$> genDm) ] :: [(Int, GenMonad Exp)]
    let stalksG size = map (second (resize (size + 1)))
            [ (050, return $ Eq  genD genD)
            , (050, return $ Lt  genD genD)
            , (050, return $ Gt  genD genD)
            , (050, return $ Neq genD genD)
            , (050, return $ Leq genD genD)
            , (050, return $ Geq genD genD)
            ]
    let nonStalksG size = map (second (resize (size + 1)))
            [ (100, return $ Not genB )
            , (100, return $ And genB genB)
            , (100, return $ Or  genB genB)
            ]
    let allNodesG size = stalksG size ++ nonStalksG size
    -- let selectOnSize size
    --         | size < maxRec = frequency $ stalksG size
    --         | otherwise = frequency $ allNodesG size

    -- s <- lift getSize
    -- trace ("genOfType(TBool) [size = " ++ show s ++ "]")
    --     lift $ sized selectOnSize
    return Rand

-- genOfType TBool = sized selectOnSize where
--     selectOnSize size
--             | size < maxRec = frequency $ stalksG size
--             | otherwise = frequency $ allNodesG size
--     -- When max depth is reached, recurse back into Exp with comparission 
--     -- operators, which will generate leafs immediately (hence `stalks`)
--     stalksG size = 
--         [ (050, resize (size + 1) $ Eq  <$> genD <*> genD)
--         , (050, resize (size + 1) $ Lt  <$> genD <*> genD)
--         , (050, resize (size + 1) $ Gt  <$> genD <*> genD)
--         , (050, resize (size + 1) $ Neq <$> genD <*> genD)
--         , (050, resize (size + 1) $ Leq <$> genD <*> genD)
--         , (050, resize (size + 1) $ Geq <$> genD <*> genD)
--         ]
--     nonStalksG size = 
--         [ (100, resize (size + 1) $ Not <$> genB)
--         , (100, resize (size + 1) $ And <$> genB <*> genB)
--         , (100, resize (size + 1) $ Or  <$> genB <*> genB)
--         ]
--     allNodesG size = stalksG size ++ nonStalksG size
--     genD = genOfType TDouble
--     genB = genOfType TBool

-- genBody :: Type -> Reader (String, Type)
-- genBody t = 

genOfType (TProd a b) = Tup <$> genOfType a <*> genOfType b
-- genOfType (TFun _ b) = do 
--     let body = genOfType b
    -- Abstr 
genOfType _ = return $ EDVal (Val 0)

-- |Generate a random expression of type `t` with rng seeded to `seed`
genExp :: Type -> String -> IO Exp
genExp t seed = do
    putStrLn "Generating a random expression of type:"
    putStrLn $ printTree t

    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let rng = mkQCGen $ intFromHash seed

    -- let gen = runReaderT (genMonad $ genOfType t) (0, empty)
    let gen = runGenT (genMonad $ genOfType t)
    let reader = unGen gen rng 0
    return $ runReader reader (0, empty)
