module Preprocess (fillRands, expDepth, simplifyTrip) where

import Syntax.Grammar.Abs
import Syntax.AbsF

import Data.Binary.Get ( runGet, getInt64host )
import Data.ByteString.Lazy.Char8 ( pack )
import Control.Applicative ( liftA2, liftA3 )
import Control.Monad.Random
import Data.Functor.Foldable.Monadic ( anaM, )
import Data.Functor.Foldable( project, Recursive (cata) )
import Data.Functor ( (<&>) )

-- |The depth of the expression (height of the tree)
expDepth :: Exp -> Int
expDepth = cata go where
    go :: ExpF Int -> Int
    go other = 1 + foldr max 0 other

-- |Replace all the `Rand` occurences with a random double value 
-- (seeded with `seed`)
fillRands :: Trip -> String -> Trip
fillRands (Triple a b c) seed = do
    let g1 = mkStdGen $ intFromHash seed
            where intFromHash s = fromIntegral $ runGet getInt64host (pack s)
    let (ra, g2) = runRand (fillRandsM a) g1
    let (rb, g3) = runRand (fillRandsM b) g2
    let (rc, __) = runRand (fillRandsM c) g3
    -- TODO: find a nicer way to chain these?
    Triple ra rb rc

-- Fill in `rand`s and recurse into `BExp`s (see `fillRandsBM`)
fillRandsM :: Exp -> Rand StdGen Exp
fillRandsM = anaM go where
    go Rand = getRandomR (-1, 1) <&> EDValF . Val
    go (Ite b e1 e2) = liftA3 IteF (fillRandsBM b) (return e1) (return e2)
    go other = return (project other)

-- The boolean expressions just need to recurse into the expressions again
fillRandsBM :: BExp -> Rand StdGen BExp
fillRandsBM = anaM go where
    go (Eq e1 e2) = liftA2 EqF (fillRandsM e1) (fillRandsM e2)
    go (Lt e1 e2) = liftA2 LtF (fillRandsM e1) (fillRandsM e2)
    go (Gt e1 e2) = liftA2 GtF (fillRandsM e1) (fillRandsM e2)
    go (Neq e1 e2) = liftA2 NeqF (fillRandsM e1) (fillRandsM e2)
    go (Leq e1 e2) = liftA2 LeqF (fillRandsM e1) (fillRandsM e2)
    go (Geq e1 e2) = liftA2 GeqF (fillRandsM e1) (fillRandsM e2)
    go other = return $ project other

simplifyExp :: Exp -> Exp
simplifyExp e = e

simplifyTrip :: Trip -> Trip
simplifyTrip (Triple a b c) = Triple (simplifyExp a) (simplifyExp b) (simplifyExp c)
