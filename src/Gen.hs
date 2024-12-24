{-# LANGUAGE CPP #-}
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
import qualified Test.QuickCheck as QC (Arbitrary, arbitrary, frequency, Gen, resize)
import GHC.Generics ( Generic )
import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Test.QuickCheck.Random ( mkQCGen, QCGen )
import Control.Monad.Reader ( Reader, runReader, MonadTrans (lift), ReaderT (runReaderT, ReaderT), MonadReader (local, ask), asks )
import Control.Monad.Identity (Identity)
import QuickCheck.GenT ( MonadGen ( liftGen ), runGenT, GenT, MonadGen, getSize, resize, oneof, sized, choose)
import Data.Bifunctor (Bifunctor(second, first))
import Test.QuickCheck.Gen (Gen(unGen))
import Preprocess ( typeDepth )
#ifdef DEBUG
import Debug.Trace ( trace )
#else
import Debug.NoTrace ( trace )
#endif

maxRec :: Int
maxRec = 5
minRec :: Int
minRec = 2

-- TODO: dont generate (fst tup) etc when there are no variables in scope

-- |Bound variables have a name and type, but also a weight, to be able 
-- |to give higher chances of generating more inner variables
data BoundVar = BoundVar {
    weight :: Double,
    name :: String,
    typ :: Type
} deriving (Show, Eq, Ord, Read)

-- The factor to scale weights of existing vars when new binder is added
varScale :: Double
varScale = 0.8

-- |Insert new variable and scale all the existing ones
withNewVar :: String -> Type -> [BoundVar] -> [BoundVar]
withNewVar n t = (BoundVar {weight=1, name=n, typ=t} :) . map scaleVar where
    scaleVar v = v { weight = weight v * varScale }

newtype GenMonad a = EvalMonad {
    genMonad :: GenT (Reader [BoundVar]) a
} deriving  ( Functor, Applicative, Monad
            , MonadReader [BoundVar], MonadGen )

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
genOfType' t@TDouble = do
    size <- getSize
    -- TODO: this type is now with depth 1/5rd of the remaining budget..? 
    -- make this choice non-arbitrary somehow?
    randomType <- resize 0 genType
    let genDouble = resize (size - 1) $ genOfType TDouble
    let genBool = resize (size - 1) $ genOfType TBool
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    validVars <- asks (filter ((==t) . typ))

    -- All the ways (i can think of) to get to a double from other terms
    let nonLeafsG =
            [ (050, trace "Min"  $ Min   <$> genDouble)
            , (050, trace "Sqrt" $ Sqrt  <$> genDouble)
            , (050, trace "Sin"  $ Sin   <$> genDouble)
            , (050, trace "Cos"  $ Cos   <$> genDouble)
            , (050, trace "EPow" $ EPow  <$> genDouble)
            , (050, trace "Mul"  $ Mul   <$> genDouble <*> genDouble)
            , (050, trace "Div"  $ Div   <$> genDouble <*> genDouble)
            , (050, trace "Mod"  $ Mod   <$> genDouble <*> genDouble)
            , (050, trace "Add"  $ Add   <$> genDouble <*> genDouble)
            , (050, trace "Sub"  $ Sub   <$> genDouble <*> genDouble)
            , (050, trace "Ite"  $ Ite   <$> genBool <*> genDouble <*> genDouble)
            , (010, trace "App"  $ App   <$> genFunction <*> genArg)
            , (010, trace "Fst"  $ Fst   <$> genLeft)
            , (010, trace "Snd"  $ Snd   <$> genRight)
            ]
    let leafsG =
           [ (100, trace "DVal" $ DVal <$> choose (-1, 1))
           , (100, trace "Rand" $ return Rand)
           ] ++ 
           [ (200, pickVar validVars) | not (null validVars) ]
    let allNodesG = nonLeafsG ++ leafsG

    notInFunc <- asks null
    if | notInFunc -> pickWeightedG leafsG
       | size >= maxRec - minRec -> pickWeightedG nonLeafsG
       | size >= 0 && size >= maxRec - minRec -> pickWeightedG allNodesG
       | otherwise -> pickWeightedG leafsG


genOfType' t@TBool = do
    size <- getSize
    randomType <- resize 0 genType
    let genDouble = resize (size - 1) $ genOfType TDouble
    let genBool = resize (size - 1) $ genOfType TBool
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    validVars <- asks (filter ((==TBool) . typ))

    let stalksG =
            [ (050, trace "Eq"  $ Eq  <$> genDouble <*> genDouble)
            , (050, trace "Lt"  $ Lt  <$> genDouble <*> genDouble)
            , (050, trace "Gt"  $ Gt  <$> genDouble <*> genDouble)
            , (050, trace "Neq" $ Neq <$> genDouble <*> genDouble)
            , (050, trace "Leq" $ Leq <$> genDouble <*> genDouble)
            , (050, trace "Geq" $ Geq <$> genDouble <*> genDouble)
            ] ++ 
            [ (100, pickVar validVars) | not (null validVars) ]
    let nonStalksG =
            [ (100, trace "Not" $ Not <$> genBool)
            , (100, trace "And" $ And <$> genBool <*> genBool)
            , (100, trace "Or"  $ Or  <$> genBool <*> genBool)
            , (050, trace "Fst" $ Fst <$> genLeft)
            , (050, trace "Snd" $ Snd <$> genRight)
            , (050, trace "App" $ App <$> genFunction <*> genArg)
            , (050, trace "Ite" $ Ite <$> genBool <*> genBool <*> genBool)
            ]
    let notInFuncG = [ (100, return BTrue) , (100, return BFalse) ]

    notInFunc <- asks null
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= 1 -> pickWeightedG nonStalksG
       | otherwise -> pickWeightedG stalksG


genOfType' t@(TProd a b) = do
    size <- getSize
    randomType <- resize 0 genType
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    let genBool = resize (size - 1) $ genOfType TBool
    let genSelf = resize (size - 1) $ genOfType t
    validVars <- asks (filter ((==t) . typ))

    -- Products can also be a tuple of the input types
    let genA = resize (size - 1) $ genOfType a
    let genB = resize (size - 1) $ genOfType b

    let deeperG = 
            [ (020, trace "App" $ App <$> genFunction <*> genArg)
            , (020, trace "Fst" $ Fst <$> genLeft)
            , (020, trace "Snd" $ Snd <$> genRight)
            , (020, trace "Ite" $ Ite <$> genBool <*> genSelf <*> genSelf )
            ]
    let termG = 
            [ (100, trace "Tup" $ Tup <$> genA <*> genB ) ] ++
            [ (200, trace "pickVar" $ pickVar validVars) | not (null validVars) ]
    let notInFuncG = termG ++ [(020, App <$> genFunction <*> genArg)]
    let allG = deeperG ++ termG

    notInFunc <- asks null
    let notInFunc = True
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= 1 -> pickWeightedG allG
       | otherwise -> pickWeightedG termG

genOfType' t@(TFun a b) = do
    size <- getSize
    randomType <- resize 0 genType
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    let genBool = resize (size - 1) $ genOfType TBool
    let genSelf = resize (size - 1) $ genOfType t
    validVars <- asks (filter ((==t) . typ))

    -- Functions type terms can also be abstractions where the body is generated
    -- with the knowledge of the variable that was just introduced
    varName <- newVarName
    let bodyG = resize (size - 1) (genOfType b)
    let body = local (withNewVar varName a) bodyG

    let deeperG = 
            [ (020, trace "App" $ App <$> genFunction <*> genArg)
            , (020, trace "Fst" $ Fst <$> genLeft)
            , (020, trace "Snd" $ Snd <$> genRight)
            , (020, trace "Ite" $ Ite <$> genBool <*> genSelf <*> genSelf )
            ]
    let termG = 
            [ (100, trace "Abstr" $ Abstr (Ident varName) <$> body) ] ++
            [ (100, trace "pickVar" $ pickVar validVars) | not (null validVars) ]
    let notInFuncG = termG
    let allG = deeperG ++ termG

    notInFunc <- asks null
    -- let notInFunc = True
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= 1 -> pickWeightedG allG
       | otherwise -> pickWeightedG termG

genOfType' t@(TCoprod a b) = do
    size <- getSize
    randomType <- resize 0 genType
    let genLeft = resize (size - 1) $ genOfType (TProd t randomType)
    let genRight = resize (size - 1) $ genOfType (TProd randomType t)
    let genFunction = resize (size - 1) $ genOfType (TFun randomType t)
    let genArg = resize (size - 1) $ genOfType randomType
    let genSelf = resize (size - 1) $ genOfType t
    let genBool = resize (size - 1) $ genOfType TBool
    validVars <- asks (filter ((==t) . typ))
    
    -- Coproducts can also be injections of the input types
    let genA = resize (size - 1) $ genOfType a
    let genB = resize (size - 1) $ genOfType b

    let deeperG = 
            [ (020, trace "App" $ App <$> genFunction <*> genArg)
            , (020, trace "Fst" $ Fst <$> genLeft)
            , (020, trace "Snd" $ Snd <$> genRight)
            , (020, trace "Ite" $ Ite <$> genBool <*> genSelf <*> genSelf )
            ]
    let termG = 
            [ (100, trace "InL" $ InL <$> genA )
            , (100, trace "InR" $ InR <$> genB ) ] ++
            [ (100, trace "pickVar" $ pickVar validVars) | not (null validVars) ]
    let notInFuncG = termG ++ [(020, App <$> genFunction <*> genArg)]
    let allG = deeperG ++ termG

    notInFunc <- asks null -- no bounded variables
    if | notInFunc -> pickWeightedG notInFuncG
       | size >= maxRec - minRec -> pickWeightedG deeperG
       | size >= 0 && size >= maxRec - minRec -> pickWeightedG allG
       | otherwise -> pickWeightedG termG

genOfType :: Type -> GenMonad Exp
genOfType t = do
    size <- getSize
    trace ("genOfType(" ++ show t ++ ") [size = " ++ show size ++ "]") $ genOfType' t

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
                else  pickWeightedG allNodes

-- |Generate a random expression of type `t` with rng seeded to `seed`
genExp :: Type -> String -> IO Exp
genExp t seed = do
    putStrLn "Generating a random expression of type:"
    putStrLn $ printTree t

    let intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let rng = mkQCGen $ intFromHash seed

    let gen = runGenT (genMonad $ genOfType t)
    let reader = unGen gen rng (maxRec + typeDepth t) 
    return $ runReader reader []
