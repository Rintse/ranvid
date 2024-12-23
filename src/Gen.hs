{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
-- TODO: make all of these not orphaned instances
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen (genExp, genType) where

import Syntax.Grammar.Abs
import Syntax.Grammar.Print ( printTree )

import Generic.Random
import Data.HashMap.Lazy ( HashMap, empty )
import Test.QuickCheck as QC (Arbitrary, arbitrary, frequency, Gen)
import GHC.Generics ( Generic )
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen, QCGen )
import Control.Monad.Reader ( Reader, runReader, MonadTrans (lift), ReaderT (runReaderT, ReaderT), MonadReader (local, ask), asks, join )
import Control.Monad.Identity (Identity)
import QuickCheck.GenT ( MonadGen ( liftGen ), runGenT, GenT, MonadGen, getSize, resize, oneof, sized, choose)
import Data.Bifunctor (Bifunctor(second, first))
import Debug.Trace (trace)
import Test.QuickCheck.Gen (Gen(unGen))

maxRec :: Int
maxRec = 15
minRec :: Int
minRec = 10

-- |Bound variables have a name and type, but also a weight, to be able 
-- |to give higher chances of generating more inner variables
data BoundVar = BoundVar {
    weight :: Double,
    name :: String,
    typ :: Type
} deriving (Show, Eq, Ord, Read)

-- The factor to scale weights of previous vars when new binder is added
varScale :: Double
varScale = 0.8

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
    choose (0, sum (map fst l) -1) >>= (`select` l) where
    select n ((k,x):xs)
        | n <= k    = x
        | otherwise = select (n-k) xs
    select _ _  = error "select on empty list"

pickWeighted :: [(Int, a)] -> GenMonad a
pickWeighted l = do
    choose (0, sum (map fst l) -1) >>= (`select` l) where
    select n ((k,x):xs)
        | n <= k    = return x
        | otherwise = select (n-k) xs
    select _ _  = error "select on empty list"

-- Get arbitrary variable identifier of the requested type
pickVar :: [BoundVar] -> GenMonad Exp
pickVar vars = do
    let weightedVars = map (\x -> (round (weight x * 100) , x)) vars
    Var . Ident . name <$> pickWeighted weightedVars

newVarName :: GenMonad String
newVarName = asks ((("x" ++) . show) . length)

genOfType' :: Type -> GenMonad Exp
genOfType' TDouble = do
    size <- getSize
    -- TODO: this type is now 1/3rd of the remaining budget..? 
    -- make this choice non-arbitrary somehow?
    typG <- resize (size `div` 5) genType -- random type
    let genD = resize (size + 1) $ genOfType TDouble
    let genB = resize (size + 1) $ genOfType TBool
    let genF = resize (size + 1) $ genOfType (TFun typG TDouble)
    let genA = resize (size + 1) $ genOfType typG
    let genL = resize (size + 1) $ genOfType (TProd TDouble typG)
    let genR = resize (size + 1) $ genOfType (TProd typG TDouble)

    validVars <- asks (filter ((==TDouble) . typ))
    let varG = ([(100, pickVar validVars) | not (null validVars)])

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
            , (050, Fst  <$> genL)
            , (050, Snd  <$> genR)
            , (050, App  <$> genF <*> genA)
            , (050, Ite  <$> genB <*> genD <*> genD)
            ]
    let leafsG =
           [ (020, DVal <$> choose (-1, 1))
           , (020, return Rand)
           ] ++ varG
    let allNodesG = nonLeafsG ++ leafsG
    if | size >= 0 && size < minRec -> pickWeightedG nonLeafsG
       | size >= minRec && size < maxRec -> pickWeightedG allNodesG
       | otherwise -> pickWeightedG leafsG

genOfType' TBool = do
    size <- getSize
    -- TODO: this type is now 1/3rd of the remaining budget..? 
    -- make this choice non-arbitrary somehow?
    typG <- resize (size `div` 5) genType -- random type
    let genD = resize (size + 1) $ genOfType TDouble
    let genB = resize (size + 1) $ genOfType TBool
    let genF = resize (size + 1) $ genOfType (TFun typG TBool)
    let genA = resize (size + 1) $ genOfType typG
    let genL = resize (size + 1) $ genOfType (TProd TBool typG)
    let genR = resize (size + 1) $ genOfType (TProd typG TBool)

    validVars <- asks (filter ((==TBool) . typ))
    let varG = ([(100, pickVar validVars) | not (null validVars)])

    let stalksG =
            [ (050, Eq  <$> genD <*> genD)
            , (050, Lt  <$> genD <*> genD)
            , (050, Gt  <$> genD <*> genD)
            , (050, Neq <$> genD <*> genD)
            , (050, Leq <$> genD <*> genD)
            , (050, Geq <$> genD <*> genD)
            ] ++ varG
    let nonStalksG =
            [ (100, Not <$> genB)
            , (100, And <$> genB <*> genB)
            , (100, Or  <$> genB <*> genB)
            , (050, App <$> genF <*> genA)
            , (100, Ite <$> genB <*> genB <*> genB)
            , (050, Fst <$> genL)
            , (050, Snd <$> genR)
            ]
    let allNodesG = stalksG ++ nonStalksG
    if size >= maxRec
        then pickWeightedG stalksG
        else pickWeightedG allNodesG

genOfType' (TProd a b) = do
    s <- getSize
    Tup <$> resize (s+1) (genOfType a) <*> resize (s+1) (genOfType b)
genOfType' (TFun a b) = do
    s <- getSize
    varName <- newVarName
    let body = local (withNewVar varName a) (resize (s+1) (genOfType b))
    Abstr (Ident varName) <$> body
genOfType' other = error $ "Cannot generate type: " ++ show other

genOfType :: Type -> GenMonad Exp
genOfType t = do
    size <- getSize
    trace ("genOfType(" ++ show t ++ ") [" ++ show size ++ "]") $ genOfType' t

genType :: GenMonad Type
genType = sized go where
    go :: Int -> GenMonad Type
    go size = do
        let genT = resize (size - 1) genType
        let nonLeafs =
                [ (100, TFun  <$> genT <*> genT)
                , (100, TProd <$> genT <*> genT)
                ]
        let leafs =
                [ (100, pure TDouble )
                , (100, pure TBool )
                ]
        let allNodes = nonLeafs ++ leafs
        trace ("genType [size = " ++ show size ++ "]")
            $ if size <= 0
                then pickWeightedG leafs
                else  pickWeightedG nonLeafs

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
