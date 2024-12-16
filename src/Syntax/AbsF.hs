{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, LambdaCase #-}

module Syntax.AbsF where

import Syntax.Grammar.Abs

import Data.Functor.Foldable.TH

makeBaseFunctor ''Exp
makeBaseFunctor ''BExp
