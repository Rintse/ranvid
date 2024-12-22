{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen (genExp) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )

import Generic.Random
import Data.HashMap.Lazy ( HashMap, empty )
import Test.QuickCheck (Arbitrary, arbitrary)
import GHC.Generics ( Generic )
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen, QCGen )
import Control.Monad.Reader ( Reader, runReader, MonadTrans (lift), ReaderT (runReaderT, ReaderT), MonadReader (local, ask), asks, join )
import Control.Monad.Identity (Identity)
import QuickCheck.GenT ( MonadGen ( liftGen ), runGenT, GenT, MonadGen, getSize, resize)
import qualified QuickCheck.GenT as QCT ( choose )
import Data.Bifunctor (Bifunctor(second, first))
import Debug.Trace (trace)
import Test.QuickCheck.Gen (Gen(MkGen, unGen))

maxRec :: Int
maxRec = 5
minRec :: Int
minRec = 3

-- |Bound variables have a name and type, but also a weight, to be able 
-- |to give higher chances of generating more inner variables
data BoundVar = BoundVar {
    weight :: Double,
    name :: String,
    typ :: Type
} deriving (Show, Eq, Ord, Read)

-- The factor to scale weights of previous vars when new binder is added
varScale :: Double
varScale = 0.2

scaleVar :: BoundVar -> BoundVar
scaleVar v = v { weight = weight v * varScale }

newtype GenMonad a = EvalMonad {
    genMonad :: GenT (Reader [BoundVar]) a
} deriving  ( Functor, Applicative, Monad
            , MonadReader [BoundVar], MonadGen )

withNewVar :: String -> Type -> [BoundVar] -> [BoundVar]
withNewVar n t = (BoundVar {weight=1, name=n, typ=t} :) . map scaleVar

-- |Pick a random element in a weighted list under the generation monad
pickWeightedG :: [(Int, GenMonad a)] -> GenMonad a
pickWeightedG l = do
    QCT.choose (0, sum (map fst l) -1) >>= (`select` l) where
    select n ((k,x):xs)
        | n <= k    = x
        | otherwise = select (n-k) xs
    select _ _  = error "select on empty list"

pickWeighted :: [(Int, a)] -> GenMonad a
pickWeighted l = do
    QCT.choose (0, sum (map fst l) -1) >>= (`select` l) where
    select n ((k,x):xs)
        | n <= k    = return x
        | otherwise = select (n-k) xs
    select _ _  = error "select on empty list"

arbitraryDVal :: GenMonad DVal
arbitraryDVal = Val <$> QCT.choose (-1, 1)

-- Get arbitrary variable identifier of the requested type
pickVar :: [BoundVar] -> GenMonad Exp
pickVar vars = do
    let weightedVars = map (\x -> (round (weight x * 100) , x)) vars
    Var . Ident . name <$> pickWeighted weightedVars

newVarName :: GenMonad String
newVarName = ("x" ++) . show <$> (QCT.choose (0, 9) :: GenMonad Int)

genOfType :: Type -> GenMonad Exp
genOfType TDouble = do
    size <- getSize
    let genD = resize (size + 1) $ genOfType TDouble
    let genB = resize (size + 1) $ genOfType TBool
    let genF = resize (size + 1) $ genOfType (TFun TDouble TDouble)
    -- TODO: only the side that is taken needs to be double in the product
    let genT = resize (size + 1) $ genOfType (TProd TDouble TDouble)
    validVars <- asks (filter ((==TDouble) . typ))
    let varG = ([(050, pickVar validVars) | not (null validVars)])
    -- All the ways (i can think of) to get to a double from other terms
    let nonLeafsG =
            [ (050, Min  <$> genD)
            , (050, Sqrt <$> genD)
            , (050, Sin  <$> genD)
            , (050, Cos  <$> genD)
            , (050, EPow <$> genD)
            , (050, Mul  <$> genD <*> genD)
            , (050, Div  <$> genD <*> genD)
            , (050, Mod  <$> genD <*> genD)
            , (050, Add  <$> genD <*> genD)
            , (050, Sub  <$> genD <*> genD)
            , (050, Fst  <$> genT)
            , (050, Snd  <$> genT)
            , (050, App  <$> genF <*> genD)
            , (050, Ite  <$> genB <*> genD <*> genD)
            ]
    -- let varG = 
    let leafsG =
           [ (050, EDVal <$> arbitraryDVal)
           , (050, return Rand)
           ] ++ varG
    let allNodesG = nonLeafsG ++ leafsG
    trace ("genOfType(TDouble) [size = " ++ show size ++ "]")
        if | size >= 0 && size < minRec -> pickWeightedG nonLeafsG
           | size >= minRec && size < maxRec -> pickWeightedG allNodesG
           | otherwise -> pickWeightedG leafsG

genOfType TBool = do
    size <- getSize
    let genD = resize (size + 1) $ genOfType TDouble
    let genB = resize (size + 1) $ genOfType TBool
    let genF = resize (size + 1) $ genOfType (TFun TBool TBool)
    -- TODO: only the side that is taken needs to be bool in the product
    let genT = resize (size + 1) $ genOfType (TProd TBool TBool)
    let stalksG =
            [ (050, Eq  <$> genD <*> genD)
            , (050, Lt  <$> genD <*> genD)
            , (050, Gt  <$> genD <*> genD)
            , (050, Neq <$> genD <*> genD)
            , (050, Leq <$> genD <*> genD)
            , (050, Geq <$> genD <*> genD)
            ]
    let nonStalksG =
            [ (100, Not <$> genB)
            , (100, And <$> genB <*> genB)
            , (100, Or  <$> genB <*> genB)
            , (050, App <$> genF <*> genB)
            , (100, Ite <$> genB <*> genB <*> genB)
            , (050, Fst <$> genT)
            , (050, Snd <$> genT)
            ]
    let allNodesG = stalksG ++ nonStalksG
    if size < maxRec
        then pickWeightedG stalksG
        else pickWeightedG allNodesG

genOfType (TProd a b) = do
    s <- getSize
    trace ("genOfType(TDouble) [size = " ++ show s ++ "]")
        Tup <$> resize (s + 1) (genOfType a) <*> resize (s+1) (genOfType b)
genOfType (TFun a b) = do
    varName <- newVarName
    let body = local (withNewVar varName a) (genOfType b)
    Abstr (Ident varName) <$> body
genOfType _ = return $ EDVal (Val 0)

-- |Generate a random expression of type `t` with rng seeded to `seed`
genExp :: Type -> String -> IO Exp
genExp t seed = do
    putStrLn "Generating a random expression of type:"
    putStrLn $ printTree t

    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let rng = mkQCGen $ intFromHash seed

    let gen = runGenT (genMonad $ genOfType t)
    let reader = unGen gen rng 0
    return $ runReader reader []
