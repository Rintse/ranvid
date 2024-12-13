{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.Parse (parse) where

import Data.Functor.Foldable ( cata )
import Data.Functor.Foldable.TH ( makeBaseFunctor )
import Syntax.Grammar.Par
import Syntax.Grammar.Abs
import System.Exit

makeBaseFunctor ''Exp
getInvalidDoubles :: Trip -> [Double]
getInvalidDoubles (Triple a b c) = checkExp a ++ checkExp b ++ checkExp c
    where checkExp = cata go where
            go (EDValF (Val d)) = [d | not (d <= 1 && d >= -1)]
            go other = concat other

-- Parses contents of given input file
parse :: String -> IO Trip
parse s = do
    let ts = myLexer s
    case pTrip ts of
        Left err_msg -> do
            putStrLn $ "Parse failed: " ++ err_msg
            putStrLn $ "Tokens still in stream:\n" ++ show ts
            exitFailure
        Right triple -> do
            case getInvalidDoubles triple of
                [] -> return triple
                non_empty_list -> do
                    putStrLn $ "Parsed expression contains numbers outside of"
                        ++ "the [-1, 1] range: " ++ show non_empty_list
                    exitFailure
